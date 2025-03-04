Spatial checks component: Source code
library(plumber)
library(dplyr)
library(zip)
library(utils)
library(stringr)
library(data.table)
library(sf)
library(ggplot2)


#* @param f:file
#* @serializer octet
#* @post /fdiextrai
function(f, res) {

  tmp <- paste0("/tmp/", stringi::stri_rand_strings(1, 7, pattern = "[A-Za-z0-9]"))
  while (file.exists(tmp)) {
    tmp <- paste0("/tmp/", stringi::stri_rand_strings(1, 7, pattern = "[A-Za-z0-9]"))
  }
  dir.create(tmp)
  # tmp <- tempdir()

  options(scipen = 999)
  options(digits = 9)
  #- Settings paths
  codePath         <- "/app/scripts/"    # R scripts location
  dataF            <- "/app/data/" # data folder
  csqF             <- "/app/csquares/"
  icesrF           <- "/app/ices_rects/"
  outPath          <- tmp   # output
  fshzn            <- "/app/fishing_zones/"

  # FDI DATA ----
  fdidatai <- rawToChar(f[[1]])
  text_con <- textConnection(fdidatai)
  fdi <- read.table(text_con,sep=",",header=T, stringsAsFactors=FALSE)

  fdi <- as.data.table(fdi)
  setnames(fdi, tolower(names(fdi)))

  ms <- unique(fdi$country)[1]

  i<-"table_i"

  fdi.n<-nrow(fdi)
  # Converting NA text to NA value
  fdi[fdi=="NA"] <- NA
  fdi[,c_square:=as.character(c_square)]
  #fdi <- setorder(fdi,year,quarter)
  fdi[,id := 1:.N,]
  ###########################
  # MACIEK 2019 adjustments #
  ###########################
  if("country_code" %in% colnames(fdi)){
    setnames(fdi,old=c("country_code"),new=c("country"))
  }
  fdi[,':='(latitude = as.numeric(latitude),
            longitude = as.numeric(longitude))]

  # Number of NAs rectangle_type
  na.rectangle_type<- fdi[is.na(rectangle_type),]
  # Number of NAs rectangle_type with NA c_square
  na.rectangle_type.na.c_square <- na.rectangle_type[is.na(c_square)]
  # Number of rows not having both coordinates or c_square
  errors.no.lat.lon.no.csq <-
    fdi[is.na(longitude) & is.na(latitude)& is.na(c_square)]
  # Errors in min max coords
  errors.lat.lon.bounds <- fdi[(longitude < -180 | longitude > 180)
                                 |(latitude < -90 | latitude > 90),]
  # Errors in only rectangle type max coords
  errors.rect.only <- fdi[is.na(rectangle_type) &!is.na(latitude)
                            &!is.na(longitude) & is.na(c_square),]

  # Number of rows having only one coordinate. Terrible case but luckily no rows.
  errors.one.coord <- fdi[is.na(longitude) != is.na(latitude)]

  # Number of rows with csquare only and wrong rect type
  errors.csq.rectangle_type <- fdi[is.na(longitude) & is.na(latitude)
                                     & !is.na(c_square) & rectangle_type != '05*05',]
  # SAVING ----
  #setwd(outPath)
  ifelse(nrow(errors.lat.lon.bounds)>0,
         errors.lat.lon.bounds[,fwrite(.SD, paste0(outPath,"/Table.I.errors.lat.lon.bounds","_",country,'.csv')),by=.(country)],'NO RECORDS')
  ifelse(nrow(errors.no.lat.lon.no.csq)>0,
         errors.no.lat.lon.no.csq[,fwrite(.SD, paste0(outPath,"/Table.I.errors.no.lat.lon.no.csq","_",country,'.csv')),by=.(country)],'NO RECORDS')
  ifelse(nrow(errors.rect.only)>0,
         errors.rect.only[,fwrite(.SD, paste0(outPath,"/Table.I.errors.rect.only","_",country,'.csv')),by=.(country)],'NO RECORDS')
  ifelse(nrow(errors.one.coord)>0,
         errors.one.coord[,fwrite(.SD, paste0(outPath,"/Table.I.errors.one.coord.only","_",country,'.csv')),by=.(country)],'NO RECORDS')
  ifelse(nrow(errors.csq.rectangle_type)>0,
         errors.csq.rectangle_type[,fwrite(.SD,paste0(outPath,"/Table.I.errors.csq.rectangle_type","_",country,'.csv')),by=.(country)],'NO RECORDS')

  #setwd(dataF)
  # Data subset having csquares only
  fdi.csq<-fdi[is.na(latitude) & is.na(longitude) & !is.na(c_square)]
  # Dataset with rectangle type and lat lon
  fdi.rect.lat.lon <- fdi[!is.na(latitude) & !is.na(longitude) &
                            !is.na(rectangle_type)&   is.na(c_square)]

  # Dataset with csq, rectangle type and lat lon
  fdi.csq.rect.lat.lon <- fdi[!(id %in% c(fdi.csq$id,fdi.rect.lat.lon$id)),]
  nrow(fdi)-nrow(fdi.csq)-nrow(fdi.rect.lat.lon)-nrow(fdi.csq.rect.lat.lon)
  # Checks
  ids <- unique(c(fdi.csq$id,fdi.rect.lat.lon$id,fdi.csq.rect.lat.lon$id))
  fdi[! id %in% ids,]
  ids <- NULL;

  # Data subset having csquares and coords
  fdi.csq.coords<-fdi[!is.na(latitude) & !is.na(longitude) & !is.na(c_square)]
  # Data subset having coords only
  fdi.coords<-fdi[!is.na(latitude) & !is.na(longitude) & is.na(c_square)]

  #setwd(csqF)
  load(file = paste0(csqF,"grids.RData"))
  csq05$geometry <- NULL
  gc()
  # Checking if coords are consistent with csquares
  fdi.csq.coords<-merge(fdi.csq.coords,csq05[,c("cscode","type","csq_x","csq_y")], all.x=T,by.x="c_square", by.y="cscode")
  fdi.csq.coords[latitude == csq_y & longitude == csq_x,valid:="YES"]
  nrow(fdi.csq.coords[valid=='YES',])
  # Number of records omitted
  nrow(fdi.csq.coords)- nrow(fdi.csq.coords[valid=='YES',])
  errors.csq.coords <- fdi.csq.coords[is.na(valid),]

  # SAVING ----
  #setwd(outPath)
  ifelse(nrow(errors.csq.coords)>0,
         errors.csq.coords[,fwrite(.SD, paste0(outPath,"/Table.I.errors.csq.coords","_",country,'.csv')),by=.(country)],'NO RECORDS')
  #setwd(dataF)

  fdi.csq.coords<-fdi.csq.coords[valid=="YES",.(country,year,quarter,vessel_length,fishing_tech,gear_type,
                                                target_assemblage,mesh_size_range,metier,supra_region,
                                                sub_region,eez_indicator,geo_indicator,specon_tech,deep,
                                                rectangle_type,latitude,longitude,c_square,valid,totfishdays,
                                                confidential,id)]

  fdi.csq.coords$valid <- NA
  # Assigning coords to csquares
  fdi.csq<-merge(fdi.csq,csq05[,c("cscode","type","csq_x","csq_y")], all.x=T,by.x="c_square", by.y="cscode")
  nrow(fdi.csq[is.na(csq_x),])
  nrow(fdi.csq[is.na(csq_x),])
  fdi.csq[is.na(csq_x),]
  errors.csq <- fdi.csq[is.na(csq_x),]

  #setwd(outPath)
  ifelse(nrow(errors.csq)>0,
         errors.csq[,fwrite(.SD, paste0(outPath,"/Table.I.errors.csq","_",country,'.csv')),by=.(country)],'NO RECORDS')

  # the join was a 100% match
  fdi.csq<-fdi.csq[,.(country,year,quarter,vessel_length,fishing_tech,gear_type,
                      target_assemblage,mesh_size_range,metier,supra_region,
                      sub_region,eez_indicator,geo_indicator,specon_tech,deep,totfishdays,
                      rectangle_type,csq_y,csq_x,c_square,
                      confidential,id)]
  setnames(fdi.csq,old = c("csq_y","csq_x"), new = c("latitude","longitude"))
  nrow(fdi.csq[is.na(longitude),])
  nrow(fdi.csq[is.na(latitude),])
  # Checking if coords are consistent with the rectangle type
  # Check coordinates according to the type of rectangle
  fdi.coords[,`:=`(remainder_lon=longitude%%1,
                   remainder_lat=latitude%%1)]
  fdi.coords[rectangle_type == "5*5",`:=`(remainder_lon=longitude%%5,
                                          remainder_lat=latitude%%5)]
  fdi.coords[(rectangle_type=="05*05" & (!remainder_lon %in% c(0.25,0.75) | !remainder_lat %in% c(0.25,0.75))) |
               (rectangle_type=="05*1" & (remainder_lon != 0.50 | !remainder_lat %in% c(0.25,0.75))) |
               (rectangle_type=="1*1" & (remainder_lon != 0.50 | remainder_lat != 0.50)) |
               (rectangle_type=="5*5" & (remainder_lon != 2.5 | remainder_lat != 2.5)),
             valid:="NO"]

  rect.check<-fdi.coords[,.(nrows=.N),by=.(country,rectangle_type,remainder_lon,remainder_lat,valid)]

  # Points on land
  csq05Land$geometry <- NULL
  fdi.coords <- merge(fdi.coords,csq05Land[,c("type","csq_x","csq_y")],
                      by.x = c("longitude","latitude"),
                      by.y = c("csq_x","csq_y"),
                      all.x = TRUE)
  #fdi.coords[type=='land',valid:='NO']

  if (ms=='FRA') {
    fdi.coords[country=='FRA' & latitude==43.25 & longitude==2.75, valid:=NA]
  }
  if (ms=='HRV') {
    fdi.coords[country=='HRV' & latitude==43.25 & longitude==17.75, valid:=NA]
  }

  fdi.coords.on.land <- fdi.coords[type=='land',]
  fdi.csq <- merge(fdi.csq,csq05Land[,c("type","cscode")],
                   by.x = "c_square",
                   by.y = "cscode",
                   all.x = TRUE)

  if (ms=='FRA') {
    fdi.csq[country=='FRA' & latitude==43.25 & longitude==2.75, valid:=NA]
  }
  if (ms=='HRV') {
    fdi.csq[country=='HRV' & latitude==43.25 & longitude==17.75, valid:=NA]
  }

  fdi.csq.on.land <- fdi.csq[type=='land',]
  fdi.csq.coords <- merge(fdi.csq.coords,csq05Land[,c("type","cscode")],
                          by.x = "c_square",
                          by.y = "cscode",
                          all.x = TRUE)

  if (ms=='FRA') {
    fdi.csq.coords[country=='FRA' & latitude==43.25 & longitude==2.75, valid:=NA]
  }
  if (ms=='HRV') {
    fdi.csq.coords[country=='HRV' & latitude==43.25 & longitude==17.75, valid:=NA]
  }

  fdi.csq.coords.on.land <- fdi.csq.coords[type=='land',]
  fdi.csq.on.land    <- fdi.csq.on.land[,lapply(.SD,as.character)]
  fdi.coords.on.land <- fdi.coords.on.land[,lapply(.SD,as.character)]
  cols <- names(fdi.coords.on.land)[names(fdi.coords.on.land)%in% names(fdi.csq.on.land)]

  points.on.land <- rbind(fdi.csq.on.land,fdi.coords.on.land[,.SD,.SDcols = cols])

  if (ms=='FRA') {
    nrow(points.on.land[country=='FRA' & latitude==43.25 & longitude==2.75,])
  }
  if (ms=='HRV') {
    nrow(points.on.land[country=='HRV' & latitude==43.25 & longitude==17.75,])
  }

  if (ms=='FRA') {
    points.on.land <- points.on.land[!(country=='FRA' & latitude==43.25 & longitude==2.75),]
  }
  if (ms=='HRV') {
    points.on.land <- points.on.land[!(country=='HRV' & latitude==43.25 & longitude==17.75),]
  }

  # SAVING ----
  #setwd(outPath)
  ifelse(nrow(points.on.land)>0,
         points.on.land[,fwrite(.SD, paste0(outPath,"/Table.I.points.on.land","_",country,'.csv')),by=.(country)],'NO RECORDS')

  #setwd(dataF)

  errors.csq.rectangle_type$valid <-'NO'
  errors.csq.rectangle_type$type  <- NA
  errors.csq.coords$valid         <-'NO'

  # At the end we have the following errors
  cols <- names(fdi)
  errors.rect.check <- fdi.coords[valid=='NO',]

  # SAVING ----
  #setwd(outPath)
  ifelse(nrow(errors.rect.check)>0,
         errors.rect.check[,fwrite(.SD, paste0(outPath,"/Table.I.errors.rect.check","_",country,'.csv')),by=.(country)],'NO RECORDS')

  #setwd(dataF)

  errors.ids <- unique(
    c(
      errors.lat.lon.bounds$id,
      errors.no.lat.lon.no.csq$id,
      errors.one.coord$id,
      errors.rect.only$id,
      errors.csq.coords$id,
      errors.csq.rectangle_type$id,
      errors.rect.check$id,
      fdi.coords.on.land$id,
      fdi.csq.on.land$id,
      errors.csq$id

    )
  )

  fdi.no.csq <- fdi[!id%in%fdi.csq$id,]
  fdi <- rbind(fdi.csq[,!c("type","valid")],fdi.no.csq, fill=TRUE)
  fdi <- fdi[, valid := "Y"]
  fdi <- fdi[id %in% errors.ids, valid := "N"]

  if (ms=='FRA') {
    fdi[country=='FRA' & latitude==43.25 & longitude==2.75, valid:='Y']
  }
  if (ms=='HRV') {
    fdi[country=='HRV' & latitude==43.25 & longitude==17.75, valid:='Y']
  }

  fwrite(fdi,paste0(outPath,"/",ms,'_table_i_total_valid_and_not.csv'))

  nrow(fdi[valid=='N'])
  nrow(fdi[valid=='N'])/nrow(fdi)*100

  # A bit of recap. We will create a table with the zero zero coords.
  zero0 <- fdi[longitude == 0 & latitude == 0,]
  # We find two unique countries: MLT and FRA. Ask
  unique(zero0$country)
  # We will now select the minus 1 minus 1 coords. It looks like for table I. It is only # HRV. It has been communicated adn uploaded on the ftp (together with the other tables).
  # Igor, the correposndent said that this 46 records represent a mistake and so we deleted them.
  minus1 <- fdi[(longitude == -1 & latitude == -1),]
  unique(minus1$country)

  # SAVING ----
  #setwd(outPath)
  ifelse(nrow(zero0)>0,
         zero0[,fwrite(.SD, paste0(outPath,"/Table.I.zero0","_",country,'.csv')),by=.(country)],'NO RECORDS')
  ifelse(nrow(minus1)>0,
         minus1[,fwrite(.SD, paste0(outPath,"/Table.I.minus1","_",country,'.csv')),by=.(country)],'NO RECORDS')

  #setwd(dataF)
  # List of the gears
  gearsFDI  <- unique(fdi$gear_type)
  # List of the mesh size
  meshSize  <- unique(fdi$mesh_size_range)
  # table(fdi$gear_type,fdi$mesh_size_range)
  # Defining the trawl macro area aggregation
  trawl   <- as.list(c("OTB", "PTB", "OTM", "PTM", "OTT"))
  fdi[gear_type %in%  trawl, gear_typeN := "TRAWL",]
  fdi[!gear_type %in% trawl, gear_typeN := gear_type, ]
  # unique mesh size
  TRAWLMs         <- fdi[gear_typeN == "TRAWL", ]
  TRAWLMsu        <- TRAWLMs[, unique(mesh_size_range), by = "gear_typeN"]
  names(TRAWLMsu) <- c("gear", "meshsize")
  msU             <- unique(TRAWLMsu$meshsize)
  sort(msU)
  # checking which country used the 90D105 mesh size range
  mesh90d105      <- fdi[mesh_size_range == "90D105",]
  unique(mesh90d105$country)

  m100            <- as.list(c("100D110", "100D120", "100DXX","105D110", "110D120", "110DXX",  "120DXX",
                               "100D400", "400DXX"))
  l100            <- as.list(c("00D14","14D16" , "14D20","00D16", "16D20", "16D32", "20D40","40D50" ,
                               "32D70", "32D80","50D100" , "32D90", "40D45", "40D55", "45D50","50D100",
                               "50D65","55D60", "65D100", "60D65", "65D70", "70D100","70D80", "70S90",
                               "80D100","65D70", "70D100","90D105", "80D100", "70D80","00S40", "40SXX",
                               "70S90"))

  fdi[gear_typeN == "TRAWL" & mesh_size_range == "NK",
      gear_typeN := "TRAWLNONE", ]

  fdi[gear_typeN == "TRAWL" & mesh_size_range %in% m100,
      gear_typeN := "TRAWLM100", ]

  fdi[gear_typeN == "TRAWL" & mesh_size_range %in% l100,
      gear_typeN := "TRAWLL100", ]

  unique(fdi$gear_typeN)

  tbbMSU                <- fdi[ gear_typeN == "TBB", unique(mesh_size_range),]
  sort(tbbMSU)

  m120                  <- as.list(c("120DXX", "100D400","400DXX"))

  # The 100DXX mesh size range is presented in 8 lines.
  # Accordingly metier these lines could be assigned as TBBL120.

  l120                  <- as.list(c("00D14","14D16" , "14D20","00D16", "16D20", "16D32", "20D40","40D50" ,
                                     "32D70", "32D80","50D100" , "32D90", "40D45", "40D55", "45D50","50D100",
                                     "50D65","55D60", "65D100", "60D65", "65D70", "70D100","70D80", "70S90",
                                     "80D100","65D70", "70D100","90D105", "80D100", "70D80","00S40", "40SXX",
                                     "70S90", "105D110", "100D110", "110D120", "100DXX"))

  fdi[gear_typeN == "TBB" & mesh_size_range == "NK", gear_typeN := "TBBNONE",]
  fdi[gear_typeN == "TBB" & mesh_size_range %in% m120, gear_typeN := "TBBM120",]
  fdi[gear_typeN == "TBB" & mesh_size_range %in% l120, gear_typeN := "TBBL120",]
  gear_typeU             <- unique(fdi$gear_typeN)
  sort(gear_typeU)

  #########
  #Maksims#
  #########

  #Define gear classes

  seine   <- as.list(c("SDN", "SPR", "SSC", "SB", "SV"))
  nets    <- as.list(c("GND", "GNS", "GNC", "GTR", "GTN"))
  dredges <- as.list(c("DRB", "HMD", "DRH"))
  hooks   <- as.list(c("LHM", "LHP", "LLD", "LLS", "LTL"))
  snets   <- as.list(c("PS", "LA"))
  traps   <- as.list(c("FPO", "FPN", "FYK"))

  fdi[gear_typeN %in% seine, gear_typeN := "SEINE",]
  fdi[gear_typeN %in% nets, gear_typeN := "NETS",]
  fdi[gear_typeN %in% dredges, gear_typeN := "DREDGES",]
  fdi[gear_typeN %in% hooks, gear_typeN := "HOOKS",]
  fdi[gear_typeN %in% snets, gear_typeN := "sNETS",]
  fdi[gear_typeN %in% traps, gear_typeN := "TRAPS",]

  geartypeU             <- unique(fdi$gear_typeN)


  gclasses <- as.list(c("DREDGES", "HOOKS", "NETS", "SEINE", "sNETS", "TBBL120",
                        "TBBM120", "TBBNONE", "TRAPS", "TRAWLL100", "TRAWLM100",
                        "TRAWLNONE"))
  if(i == "table_i") svalue <- "fishing_days=sum(totfishdays)" else svalue <- "landings=sum(totwghtlandg)"

  gearNOTingclasses <- fdi[!gear_typeN %in% gclasses]

  # SAVING ----
  #setwd(outPath)
  ifelse(nrow(gearNOTingclasses)>0,
         gearNOTingclasses[,fwrite(.SD, paste0(outPath,"/Table.I.gearNOTingclasses","_",country,'.csv')),by=.(country)],'NO RECORDS')

  #setwd(dataF)

  fdi <- fdi[gear_typeN %in% gclasses,]
  categorical.fields.tableau <-
    c(
      'year',
      'quarter',
      'vessel_length',
      'fishing_tech',
      'gear_type',
      'mesh_size_range',
      'target_assemblage',
      'metier',
      'supra_region',
      'sub_region',
      'specon_tech',
      'deep',
      'rectangle_type',
      'c_square',
      'latitude',
      'longitude',
      'confidential',
      'valid'
    )
  # I need to remove the confidential records for Portugal for Tableau
  fdi.tableau <-
    fdi[, sum(totfishdays), by = categorical.fields.tableau]
  fdi.tableau[,`:=`(totfishdays = V1,
                    V1 = NULL)]
  fdi <-
    fdi[, eval(parse(text = svalue)), by = .(
      country,
      year,
      quarter,
      gear_typeN,
      specon_tech,
      sub_region,
      rectangle_type,
      c_square,
      latitude,
      longitude,
      confidential,
      valid
    )]

  fdi[,`:=`(totfishdays = V1,
            V1 = NULL)]
  # SAVING ----
  #setwd(outPath)
  save(fdi,file=paste(outPath,"/",ms,"_fdi_", i, ".RData", sep=''))
  fdi_TABLE_I_errors <- fdi[valid == 'N']

  #fdi <- fread(paste0(ms,'_table_i_total_valid_and_not.csv'))
  # SUBREGIONS
  #csqSubr <- fread('../reference_table/SUB_REGION_cscode_reference_table.csv')
  ### OBS !!! the above is not working. Check if it is needed

  # SAVING ----
  #setwd(outPath)
  fwrite(fdi_TABLE_I_errors,paste0(outPath,"/",ms,'_fdi_TABLE_I_errors.csv'))
  save(fdi.tableau,file = paste(outPath,"/",ms,"_fdi_tableau_", i, ".RData", sep=''))

  #rm(list=ls())
  gc()

  ### P-1
  #-------------------------------------------------------------------------------
  # 1_EWG-FDI_spatial_effort(rev-SK)
  # Script to clean, analyse and map the spatial effort and spatial landings
  # datasets of the FDI EWG22-10 20220912 - 20210916
  # Tor 3 team : Maciej, Maksims, Maurizio, Stefanos. Every
  # contribution is highlighted.
  # Contact: maurizio.gibin@gmail.com
  #
  # Date: 2022-09-12 - 2022-09-16
  #
  #
  #-------------------------------------------------------------------------------

  # ###################
  # # Maciej Adamowicz
  # # 14.09.2018
  # ###################
  # library(data.table)
  # library(sf)
  # library(ggplot2)
  # library(dplyr)

  options(scipen = 999)
  options(digits = 9)


  #setwd(csqF)
  load(file = paste0(csqF,"grids.RData"))
  # FDI DATA ----

  #setwd(dataF)

  ####################
  # Maciej 19.09.2019 #
  ####################
  load(file=paste0(dataF,ms,"_fdi_table_i.RData"))

  fdi <- fdi %>%
    group_by(country, year, quarter, gear_typeN, specon_tech, sub_region, rectangle_type,
             latitude, longitude, confidential,valid) %>%
    summarise(totfishdays=sum(totfishdays,na.rm=T))
  fdi<-ungroup(fdi)

  # Please use data.table! In the landings script I achieve the same result
  # using DT in less time. I left this but I suggest to use DT in the future
  # especially when the number of rows is considerable, like Table H.

  #Loading the file with subregions assigned to fishing zones
  #setwd(fshzn)
  fishing_zones <- fread(paste0(fshzn,"fishing_zones_2022.csv"), stringsAsFactors = F)
  #setwd(dataF)

  # test <- unique(fdi$sub_region)
  # t <-
  # sapply(test,function(x){
  #   x==toupper(x)
  # })

  #fdi$sub_region <- toupper(fdi$sub_region)

  #Assign fishing zones to the fdi data
  fdi <- left_join(fdi,fishing_zones,by="sub_region")
  fdi<-data.table(fdi)
  if(nrow(fdi[is.na(fishing_zone)])){
    stop(paste("Fishing zones could not be assigned to the following subregions:",
               fdi[is.na(fishing_zone), .(unique(sub_region))]))
  } else{
    print("Fishing zones assigned successfuly.")
  }

  fwrite(fdi[sub_region=="NK",.(nrows=.N),by=.(country,year,confidential,totfishdays,valid)],paste0(outPath,"/",ms,"_Table.I.missing.subregion.csv"))

  #Remove rows with sub_region = NK and remove BSAs
  fdi<-fdi[!sub_region %in% c("NK","BSA")]
  #Check if all rows have a fishing zone assigned
  unique(fdi[is.na(fishing_zone),.(sub_region)])

  #Remove  incorrect data
  fdi<-fdi[valid=="Y"]
  unique(fdi$rectangle_type)
  fdi[is.na(rectangle_type),rectangle_type:="05*05"]
  fdi.rectangle.na.csq<-fdi[is.na(rectangle_type)]

  #Create id for each lon/lat combination in the fdi data
  fdi <- mutate(fdi,
                rect_id = paste(as.character(longitude),as.character(latitude),sep = '/'))
  #Create id containing lon and lat of the centroid for each ICES rectangle
  icesr <- mutate(icesr,rect_id=paste(as.character(ices_x),as.character(ices_y),sep = '/'))

  #Join fdi data with ices rectangles dataset
  fdi <- left_join(fdi,icesr,by="rect_id")
  fdi <- select(fdi,country:icesname)

  #Keep the the data with the ICES rectangles assigned in a separate dataset
  fdi.ices<-filter(fdi,!is.na(icesname))
  sum(fdi.ices$totfishdays)

  #Join the fdi.ices dataset with c-squares dataset. Warning! Fishing days will be doubled.
  fdi.ices<-left_join(fdi.ices,csq05,by="icesname")
  #Divide fishing days by 2 (each ICES rectangle has 2 c-squares)
  fdi.ices<-mutate(fdi.ices,totfishdays=totfishdays/2)
  #Check if the total fishing days remained the same
  sum(fdi.ices$totfishdays)
  fdi.ices<-select(fdi.ices,country:icesname,cscode)

  #Keep the the data with the ICES rectangles NOT assigned in a separate dataset
  fdi.not.ices<-filter(fdi,is.na(icesname))
  fdi.not.ices.05.1<-filter(fdi.not.ices,rectangle_type=="05*1")
  #Handle 0.5x1 rectangles outside ICES area
  #Create the ids containing lon and lat of the centre left and centre right of the csquare
  sum(fdi.not.ices.05.1$totfishdays)
  csq05<-mutate(csq05,
                rect_id=paste(as.character(w_csq),as.character(csq_y),sep='/'))#rect_id = centre/left
  fdi.not.ices.05.1.left<-left_join(fdi.not.ices.05.1,csq05,by="rect_id") %>%
    mutate(totfishdays=totfishdays/2)

  csq05<-mutate(csq05,
                rect_id=paste(as.character(e_csq),as.character(csq_y),sep='/'))#rect_id = centre/right
  fdi.not.ices.05.1.right<-left_join(fdi.not.ices.05.1,csq05,by="rect_id") %>%
    mutate(totfishdays=totfishdays/2)
  fdi.not.ices.05.1<-rbind(fdi.not.ices.05.1.left,fdi.not.ices.05.1.right)
  fdi.not.ices.05.1<-select(fdi.not.ices.05.1,country:icesname.x,cscode)
  fdi.not.ices.05.1<-rename(fdi.not.ices.05.1,icesname=icesname.x)
  sum(fdi.not.ices.05.1$totfishdays)

  print(paste0("fdi.not.ices.05.1 number of rows without cscode: ",nrow(filter(fdi.not.ices.05.1,is.na(cscode)))))
  temp<-filter(fdi.not.ices.05.1,is.na(cscode)) %>%
    select(country:icesname)
  fdi.not.ices.05.1<-filter(fdi.not.ices.05.1,!is.na(cscode))

  # Check what rectangle types are present in the rest of the data
  fdi.not.ices %>%
    filter(rectangle_type!="05*1") %>%
    group_by(rectangle_type) %>%
    summarise(n=n())

  # The rest of the data has 05*05, 1*1 and 5*5 rectangle_type

  # Handle 05*05 rectangles
  fdi.csq.05.05 <- filter(fdi.not.ices,rectangle_type=="05*05") %>%
    rename(csq_c_id=rect_id)

  # Create the id containing lon and lat of the centroid for each c-square
  csq05<-mutate(csq05,
                csq_c_id=paste(as.character(csq_x),as.character(csq_y),sep='/'))


  # Join the fdi.csq.c dataset with c-squares dataset.
  fdi.csq.05.05<-left_join(fdi.csq.05.05,csq05,by="csq_c_id")
  fdi.csq.05.05<-select(fdi.csq.05.05,country:icesname.x,cscode)

  # Check if there are any rows without csquare assigned
  print(paste0("fdi.csq.05.05 number of rows without cscode: ",nrow(filter(fdi.csq.05.05,is.na(cscode)))))

  # Handle 1*1 rectangles
  fdi.csq.1.1 <- filter(fdi.not.ices,rectangle_type=="1*1") %>%
    rename(csq_c_id=rect_id)

  csquares.1.1 <- select(fdi.csq.1.1,csq_c_id,longitude,latitude) %>%
    mutate(key=1) %>%
    distinct()
  grid.for.1.1 <- data.frame(lon_diff = c(0,0.5,0.5,0),
                             lat_diff = c(0,0,0.5,0.5),
                             key= c(1))
  csquares.1.1 <- inner_join(csquares.1.1,grid.for.1.1,by="key") %>%
    mutate(bl_lon = longitude - lon_diff,
           bl_lat = latitude - lat_diff) %>%
    select(csq_c_id, bl_lon, bl_lat)

  sum(fdi.csq.1.1$totfishdays)
  fdi.csq.1.1 <- fdi.csq.1.1 %>%
    inner_join(csquares.1.1,by="csq_c_id") %>%
    mutate(totfishdays=totfishdays/4,
           rect_id=paste(as.character(bl_lon),as.character(bl_lat),sep='/'))
  sum(fdi.csq.1.1$totfishdays)

  # create bottom-left id of csquare
  csq05<-mutate(csq05,
                rect_id=paste(as.character(w_csq),as.character(s_csq),sep='/'))
  fdi.csq.1.1<-left_join(fdi.csq.1.1,csq05,by="rect_id")
  fdi.csq.1.1<-select(fdi.csq.1.1,country:fishing_zone,rect_id,icesname.x,cscode)
  print(paste0("fdi.csq.1.1 number of rows without cscode: ",nrow(filter(fdi.csq.1.1,is.na(cscode)))))

  # Handle 5*5 rectangles
  fdi.csq.5.5 <- filter(fdi.not.ices,rectangle_type=="5*5") %>%
    rename(csq_c_id=rect_id)

  csquares.5.5 <- select(fdi.csq.5.5,csq_c_id,longitude,latitude) %>%
    mutate(key=1) %>%
    distinct()

  grid.for.5.5 <- inner_join(data.frame(lon_diff=seq(-2, by=0.5, length.out = 10), key=1),
                             data.frame(lat_diff=seq(-2, by=0.5, length.out = 10), key=1),
                             by="key")

  csquares.5.5 <- inner_join(csquares.5.5,grid.for.5.5,by="key") %>%
    mutate(bl_lon = longitude - lon_diff,
           bl_lat = latitude - lat_diff) %>%
    select(csq_c_id, bl_lon, bl_lat)

  sum(fdi.csq.5.5$totfishdays)
  fdi.csq.5.5 <- fdi.csq.5.5 %>%
    inner_join(csquares.5.5,by="csq_c_id") %>%
    mutate(totfishdays=totfishdays/100,
           rect_id=paste(as.character(bl_lon),as.character(bl_lat),sep='/'))
  sum(fdi.csq.5.5$totfishdays)

  # create bottom-left id of csquare
  csq05<-mutate(csq05,
                rect_id=paste(as.character(w_csq),as.character(s_csq),sep='/'))
  fdi.csq.5.5<-left_join(fdi.csq.5.5,csq05,by="rect_id")
  fdi.csq.5.5<-select(fdi.csq.5.5,country:fishing_zone,rect_id,icesname.x,cscode)
  print(paste0("fdi.csq.5.5 number of rows without cscode: ",nrow(filter(fdi.csq.5.5,is.na(cscode)))))

  fdi.ices<-rename(fdi.ices,geo_id=rect_id)
  fdi.not.ices.05.1<-rename(fdi.not.ices.05.1,geo_id=rect_id)
  fdi.csq.05.05<-rename(fdi.csq.05.05,geo_id=csq_c_id,icesname=icesname.x)
  fdi.csq.1.1<-rename(fdi.csq.1.1,geo_id=rect_id,icesname=icesname.x)
  fdi.csq.5.5<-rename(fdi.csq.5.5,geo_id=rect_id,icesname=icesname.x)

  result<-fdi.ices %>%
    rbind(fdi.csq.05.05) %>%
    rbind(fdi.not.ices.05.1) %>%
    rbind(fdi.csq.1.1) %>%
    rbind(fdi.csq.5.5) %>%
    as.data.frame()

  print(paste0("number of rows without cscode: ",nrow(filter(result,is.na(cscode)))))

  result <- as.data.table(result)
  result <-
    result[, sum(totfishdays,na.rm=T), by = .(
      country,
      year,
      quarter,
      gear_typeN,
      specon_tech,
      sub_region,
      fishing_zone,
      icesname,
      confidential,
      cscode
    )]
  result[,`:=`(totfishdays = V1,
               V1 = NULL)]


  result<-left_join(result,csq05,by="cscode") %>%
    select(country:totfishdays,geometry) %>%
    rename(icesname=icesname.x)


  print(paste0("Is total effort correct? : ",round(sum(result$totfishdays),1)==round(sum(fdi$totfishdays),1)))

  result_sf<-st_sf(result)
  #setwd(outPath)
  st_write(result_sf,layer=outPath,"/",ms,"_spatial_effort.shp",dsn=".",driver="ESRI Shapefile", delete_layer = TRUE)
  save(result,file=paste0(outPath,"/",ms,'_spatial_effort.RData'))

  #setwd(outPath)

  if (!file.exists(paste0(outPath,"/effort"))) { dir.create(paste0(outPath,"/effort")) }
  if (!file.exists(paste0(outPath,"/effort/areas"))) { dir.create(paste0(outPath,"/effort/areas")) }
  if (!file.exists(paste0(outPath,"/effort/errors"))) { dir.create(paste0(outPath,"/effort/errors")) }
  if (!file.exists(paste0(outPath,"/effort/gears"))) { dir.create(paste0(outPath,"/effort/gears")) }
  if (!file.exists(paste0(outPath,"/effort/specons"))) { dir.create(paste0(outPath,"/effort/specons")) }

  #library(filesstrings)
  fList <- list.files(path=outPath,patter=glob2rx('*I*.csv'))
  file.copy(fList,paste0(outPath,'/effort/'),overwrite = T)
  file.remove(fList)


  ### P-2
  #-------------------------------------------------------------------------------
  # 2_EWG-FDI_mapping_I(rev-SK)
  # Script to clean, analyse and map the spatial effort and spatial landings
  # datasets of the FDI EWG22-10 20220912 - 20220916
  # Tor 3 team : Maciej, Maksims, Maurizio, Stefanos. Every
  # contribution is highlighted.
  # Contact: maurizio.gibin@gmail.com
  #
  # Date: 2022-09-12 - 2022-09-16
  #
  #
  #-------------------------------------------------------------------------------

  #########
  #Maksims#
  #########
  #setwd(codePath)
  source(paste0(codePath,'CSquareToLonLat.R'))
  world <- map_data('world')

  #setwd(fshzn)
  fz_limits <- read.csv(paste0(fshzn,"fishing_zones_limits.csv"), sep=',')
  #setwd(outPath)
  # EFFORT MAPS -----
  load(file = paste0(outPath,"/",ms,"_spatial_effort.RData"))
  #unique(result$specon_tech)

  # Renaming the variable
  result$fishing_days <- result$totfishdays
  result$totfishdays        <- NULL

  result_sf <- result
  result_sf <- as.data.table(result_sf)

  cc <- result_sf[, CSquare2LonLat(result_sf$cscode, 0.5)]
  result_sf <- cbind(result_sf, cc)
  result_sf <- result_sf[, .(lat = SI_LATI, lon = SI_LONG), by = .(country,year,quarter,gear_typeN,specon_tech,sub_region,
                                                                   fishing_zone,icesname,confidential,cscode, fishing_days)]


  #Group and sum fishing days per fishing zone, gear type, specon_lo
  result_sum <- result_sf[, .(fishing_days=sum(fishing_days), value=log(sum(fishing_days))),
                            by = .(fishing_zone, gear_typeN, specon_tech, year, confidential,
                                   cscode, lon, lat)]

  #Plotting effort
  #setwd(outPath)

  # #Plot all fishing areas
  # # OBS !!! is not working
  # for(i in fz_limits$id){
  #   xmin <- fz_limits$lonMin[i]
  #   xmax <- fz_limits$lonMax[i]
  #   ymin <- fz_limits$latMin[i]
  #   ymax <- fz_limits$latMax[i]
  #   pl <- ggplot(if(fz_limits$fishing_zone[i]=="Earth"){
  #     result_plot <- result_sum[, .(value=log(sum(fishing_days))), by = .(year, lon, lat)]}
  #     else result_plot <- result_sum[fishing_zone==fz_limits$fishing_zone[i]])+
  #     theme_bw() +
  #     geom_tile(aes(lon, lat, fill = value)) +
  #     coord_quickmap(xlim = c(xmin,xmax), ylim = c(ymin, ymax))+
  #     geom_polygon(data=world, aes(long, lat, group=group), fill = "#909191")+
  #     scale_fill_continuous(low = "yellow", high = "red") +
  #     labs(x = NULL, y = NULL, fill = "Logarithm of \nFishing days")
  #     d1 <- pl + facet_wrap( ~ year, nrow = 2) + theme(strip.text.x = element_text(size = 6),
  #                                                 axis.text = element_text(size = 6),
  #                                                 legend.title = element_text(size = 7),
  #                                                 legend.position="bottom",
  #                                                 # legend.key.size = unit(0.3, "cm"),
  #                                                 legend.text = element_text(size = 7))
  #   fname <- paste(outPath,"/effort/areas/", gsub(' ','_',fz_limits$fishing_zone[i]), "_log_of_fishing_days_GRC.png", sep = "")
  #   ggsave(filename=fname, plot=d1)
  # }


  #Plot specons
  result_lo <- result_sf %>%
    group_by(year, specon_tech, confidential, lon, lat) %>%
    summarise(fishing_days=sum(fishing_days)) %>%
    mutate(value=log(fishing_days))

  #result_lo <- result_sf[., .(fishing_days=sum(fishing_days), value=log(sum(fishing_days))), by = .(year, specon_tech, lon, lat)]

  specons <- unique(result_lo$specon_tech)
  specons <- specons[!is.na(specons)]
  for(i in specons){
    r <- result_lo %>% filter(specon_tech==i)
    #r <- result_lo[specon_tech==i]
    xmin <- min(r$lon)
    xmax <- max(r$lon)
    ymin <- min(r$lat)
    ymax <- max(r$lat)
    pl <- ggplot(r)  +
      theme_bw() +
      geom_tile(aes(lon, lat, fill = value)) +
      coord_quickmap(xlim = c(xmin,xmax), ylim = c(ymin, ymax))+
      geom_polygon(data=world, aes(long, lat, group=group), fill = "#909191")+
      scale_fill_continuous(low = "yellow", high = "red") +
      labs(x = NULL, y = NULL, fill = "Logarithm of \nFishing days")
    d1 <- pl + facet_wrap( ~ year, nrow = 2) + theme(strip.text.x = element_text(size = 6),
                                                     axis.text = element_text(size = 6),
                                                     legend.title = element_text(size = 7),
                                                     legend.position="bottom",
                                                     # legend.key.size = unit(0.3, "cm"),
                                                     legend.text = element_text(size = 7))
    fname <- paste(outPath,"/effort/specons/", i, "_log_of_fishing_days_",ms,".png", sep = "")
    ggsave(filename=fname, plot=d1)
  }

  lstpolys <- lapply(1:nrow(fz_limits), function(x) {
    dt <- data.table(
      lon = c(
        fz_limits[x,]$lonMin,
        fz_limits[x,]$lonMin,
        fz_limits[x,]$lonMax,
        fz_limits[x,]$lonMax,
        fz_limits[x,]$lonMin
      ),
      lat = c(
        fz_limits[x,]$latMin,
        fz_limits[x,]$latMax,
        fz_limits[x,]$latMax,
        fz_limits[x,]$latMin,
        fz_limits[x,]$latMin
      ),
      group = x,
      order = 1:5
    )
    # poly <- st_polygon(list(as.matrix(dt)))
  })
  lstpolysdf <- rbindlist(lstpolys)

  # #Plot errors
  # # OBS !!! is not working
  # for(i in fz_limits$id){
  #   if(fz_limits$fishing_zone[i]=="Earth") next
  #   else r <- result_sum %>% filter(fishing_zone==fz_limits$fishing_zone[i])
  #   #r <- result_sum[fishing_zone==fz_limits$fishing_zone[i]]
  #   xmin <- min(r$lon)
  #   xmax <- max(r$lon)
  #   ymin <- min(r$lat)
  #   ymax <- max(r$lat)
  #   pl <- ggplot(r)  +
  #     theme_bw() +
  #     geom_tile(aes(lon, lat, fill = value)) +
  #     coord_quickmap(xlim = c(xmin,xmax), ylim = c(ymin, ymax))+
  #     # xlim(xmin,xmax)+
  #     # ylim(ymin, ymax)+
  #     geom_polygon(data=world, aes(long, lat, group=group), fill = "#909191")+
  #     geom_polygon(data = lstpolysdf[lstpolysdf$group ==i], aes(lon, lat, group=group), fill = NA, colour = 'black')+
  #     scale_fill_continuous(low = "yellow", high = "red") +
  #     labs(x = NULL, y = NULL, fill = "Logarithm of \nFishing days")
  #   d1 <- pl + facet_wrap( ~ year, nrow = 2) + theme(strip.text.x = element_text(size = 6),
  #                                                    axis.text = element_text(size = 6),
  #                                                    legend.title = element_text(size = 7),
  #                                                    legend.position="bottom",
  #                                                    # legend.key.size = unit(0.3, "cm"),
  #                                                    legend.text = element_text(size = 7))
  #   fname <- paste(outPath,"/effort/errors/", gsub(' ','_',fz_limits$fishing_zone[i]), "_errors_log_of_fishing_days_GRC.png", sep = "")
  #   ggsave(filename=fname, plot=d1)
  # }

  #Plot gear types
  result_gr <- result_sf %>%
    group_by(gear_typeN, year, confidential, lon, lat) %>%
    summarise(fishing_days=sum(fishing_days)) %>%
    mutate(value=log(fishing_days))
  #result_gr <- result_sf[., .(fishing_days=sum(fishing_days), value=log(sum(fishing_days))), by = .(gear_typeN, lon, lat)]
  gr <- unique(result_gr$gear_typeN)
  for(i in gr){
    r <- result_gr %>% filter(gear_typeN==i)
    #r <- result_gr[gear_typeN==i]
    xmin <- min(r$lon)
    xmax <- max(r$lon)
    ymin <- min(r$lat)
    ymax <- max(r$lat)
    pl <- ggplot(r)  +
      theme_bw() +
      geom_tile(aes(lon, lat, fill = value)) +
      coord_quickmap(xlim = c(xmin,xmax), ylim = c(ymin, ymax))+
      geom_polygon(data=world, aes(long, lat, group=group), fill = "#909191")+
      scale_fill_continuous(low = "yellow", high = "red") +
      labs(x = NULL, y = NULL, fill = "Logarithm of \nFishing days")
    d1 <- pl + facet_wrap( ~ year, nrow = 2) + theme(strip.text.x = element_text(size = 6),
                                                     axis.text = element_text(size = 6),
                                                     legend.title = element_text(size = 7),
                                                     legend.position="bottom",
                                                     # legend.key.size = unit(0.3, "cm"),
                                                     legend.text = element_text(size = 7))
    fname <- paste(outPath,"/effort/gears/", i, "_log_of_fishing_days_",ms,".png", sep = "")
    ggsave(filename=fname, plot=d1)
  }
  # zipfile <- tempfile()

  zipfile <- paste0(tmp,"/", stringi::stri_rand_strings(1, 10, pattern = "[A-Za-z0-9]"), ".zip")
  while (file.exists(zipfile)) {
    zipfile <- paste0(tmp,"/", stringi::stri_rand_strings(1, 10, pattern = "[A-Za-z0-9]"), ".zip")
  }

  # List all files in the directory and its subdirectories
  filestozip <- list.files(tmp, recursive = TRUE, full.names = TRUE)

  # Create the zip file without including the leading "./" in the paths
  zip::zip(zipfile, files = filestozip, root = tmp)

  # zip::zip(zipfile, ".", root = tmp)

  val <- readBin(zipfile, "raw", n=file.info(zipfile)$size)
  if (file.exists(tmp)) {
    unlink(tmp, recursive=T)
  }
  if (file.exists(zipfile)) {
    unlink(zipfile)
  }
  as_attachment(val, "fdiextrai-results.zip")
}

#* @param f:file
#* @serializer octet
#* @post /fdiextrah
function(f, res) {
  tmp <- paste0("/tmp/", stringi::stri_rand_strings(1, 7, pattern = "[A-Za-z0-9]"))
  while (file.exists(tmp)) {
    tmp <- paste0("/tmp/", stringi::stri_rand_strings(1, 7, pattern = "[A-Za-z0-9]"))
  }
  dir.create(tmp)
  # tmp <- tempdir()

  #- Settings paths
  codePath         <- "/app/scripts/"    # R scripts location
  dataF            <- "/app/data/" # data folder
  csqF             <- "/app/csquares/"
  icesrF           <- "/app/ices_rects/"
  outPath          <- tmp   # output
  fshzn            <- "/app/fishing_zones/"


  # FDI DATA ----
  fdidatah <- rawToChar(f[[1]])
  text_con <- textConnection(fdidatah)
  fdi <- read.table(text_con,sep=",",header=T, stringsAsFactors=FALSE)

  fdi <- as.data.table(fdi)
  setnames(fdi, tolower(names(fdi)))

  ms <- unique(fdi$country)[1]

  i<-"Table.H"

  fdi.n<-nrow(fdi)
  # Converting NA text to NA value
  fdi[fdi=="NA"] <- NA
  fdi[,c_square:=as.character(c_square)]

  fdi[,id := 1:.N,]
  ###########################
  # MACIEK 2019 adjustments #
  ###########################
  if("country_code" %in% colnames(fdi)){
    setnames(fdi,old=c("country_code"),new=c("country"))
  }
  # fdi[,':='(rectangle_lat = as.numeric(rectangle_lat),
  #           rectangle_lon = as.numeric(rectangle_lon))]

  fdi[,':='(latitude = as.numeric(latitude),
            longitude = as.numeric(longitude))]

  # Creating the table for checking unit weight and value
  table.unit.weight.vallandg <- fdi[,.("totwghtlandg" = round(sum(totwghtlandg,na.rm = T),0),
                                       "totvallandg"  = round(sum(as.numeric(totvallandg), na.rm = T),0)),
                                     by=.(country,year)]
  setorder(table.unit.weight.vallandg,country,year)
  # errors.unit.weightDT <- dcast(errors.unit.weight, country ~ year, value.var = "totwghtlandg")
  #
  fwrite(table.unit.weight.vallandg,paste0(outPath,"/",ms,'_Table.H.errors.unit.weight.value.csv'))

  # Number of NAs rectangle_type
  # na.rectangle_type<- fdi[is.na(rectangle_type),]
  # Number of NAs rectangle_type with NA c_square
  # na.rectangle_type.na.c_square <- na.rectangle_type[is.na(c_square)]
  # Number of rows not having both coordinates or c_square
  errors.no.lat.lon.no.csq <-
    fdi[is.na(longitude) & is.na(latitude)& is.na(c_square)]

  # Errors in min max coords
  errors.lat.lon.bounds <- fdi[(longitude < -180 | longitude > 180)
                                 |(latitude < -90 | latitude > 90),]
  # Errors in missing rectangle type where coords are present
  errors.rect.only <- fdi[is.na(rectangle_type) &!is.na(latitude)
                            &!is.na(longitude) & is.na(c_square),]

  # Number of rows having only one coordinate. Terrible case but luckily no rows.
  errors.one.coord <- fdi[is.na(longitude) != is.na(latitude)]

  # Number of rows with csquare only and wrong rect type
  errors.csq.rectangle_type <- fdi[is.na(longitude) & is.na(latitude)
                                     & !is.na(c_square) & !is.na(rectangle_type),]
  # SAVING ----
  #setwd(outPath)
  ifelse(nrow(errors.lat.lon.bounds)>0,
         errors.lat.lon.bounds[,fwrite(.SD, paste0(outPath,"/Table.H.errors.lat.lon.bounds","_",country,'.csv')),by=.(country)],'NO RECORDS')
  ifelse(nrow(errors.no.lat.lon.no.csq)>0,
         errors.no.lat.lon.no.csq[,fwrite(.SD, paste0(outPath,"/Table.H.errors.no.lat.lon.no.csq","_",country,'.csv')),by=.(country)],'NO RECORDS')
  ifelse(nrow(errors.rect.only)>0,
         errors.rect.only[,fwrite(.SD, paste0(outPath,"/Table.H.errors.rect.only","_",country,'.csv')),by=.(country)],'NO RECORDS')
  ifelse(nrow(errors.one.coord)>0,
         errors.one.coord[,fwrite(.SD, paste0(outPath,"/Table.H.errors.one.coord.only","_",country,'.csv')),by=.(country)],'NO RECORDS')
  ifelse(nrow(errors.csq.rectangle_type)>0,
         errors.csq.rectangle_type[,fwrite(.SD,paste0(outPath,"/Table.H.errors.csq.rectangle_type","_",country,'.csv')),by=.(country)],'NO RECORDS')

  #setwd(dataF)
  # Data subset having csquares and coords
  fdi.csq.coords<-fdi[!is.na(latitude) & !is.na(longitude) & !is.na(c_square)]
  # Data subset having csquares only
  fdi.csq<-fdi[is.na(latitude) & is.na(longitude) & !is.na(c_square)]
  # Data subset having coords only
  fdi.coords<-fdi[!is.na(latitude) & !is.na(longitude) & is.na(c_square)]
  nrow(fdi)-nrow(fdi.coords)-nrow(fdi.csq)-nrow(fdi.csq.coords)
  #fdi <- NULL;gc()
  #setwd(csqF)
  load(file = paste0(csqF,"grids.RData"))
  csq05$geometry <- NULL
  gc()
  # Checking if coords are consistent with csquares
  fdi.csq.coords<-merge(fdi.csq.coords,csq05[,c("cscode","type","csq_x","csq_y")], all.x=T,by.x="c_square", by.y="cscode")
  fdi.csq.coords[latitude == csq_y & longitude == csq_x,valid:="YES"]
  nrow(fdi.csq.coords[valid=='YES',])
  # Number of records omitted
  nrow(fdi.csq.coords)- nrow(fdi.csq.coords[valid=='YES',]) # 5120 to omit
  errors.csq.coords <- fdi.csq.coords[is.na(valid),]

  # SAVING ----
  #setwd(outPath)
  ifelse(nrow(errors.csq.coords)>0,
         errors.csq.coords[,fwrite(.SD, paste0(outPath,"/Table.H.errors.csq.coords","_",country,'.csv')),by=.(country)],'NO RECORDS')
  # setwd(dataF)

  fdi.csq.coords<-fdi.csq.coords[valid=="YES",.(country,year,quarter,vessel_length,fishing_tech,gear_type,
                                                target_assemblage,mesh_size_range,metier,supra_region,
                                                sub_region,eez_indicator,geo_indicator,specon_tech,deep,species,
                                                rectangle_type,latitude,longitude,c_square,valid,totwghtlandg,totvallandg,
                                                confidential,id)]

  fdi.csq.coords$valid <- NA;
  # Assigning coords to csquares
  fdi.csq<-merge(fdi.csq,csq05[,c("cscode","type","csq_x","csq_y")], all.x=T,by.x="c_square", by.y="cscode")

  nrow(fdi.csq[is.na(csq_x),])
  fdi.csq[is.na(csq_x),]
  errors.csq <- fdi.csq[is.na(csq_x),]
  #setwd(outPath)
  ifelse(nrow(errors.csq)>0,
         errors.csq[,fwrite(.SD, paste0(outPath,"/Table.H.errors.csq","_",country,'.csv')),by=.(country)],'NO RECORDS')
  #setwd(dataF)
  fdi.csq<-fdi.csq[!is.na(csq_x),.(country,year,quarter,vessel_length,fishing_tech,gear_type,
                                   target_assemblage,mesh_size_range,metier,supra_region,
                                   sub_region,eez_indicator,geo_indicator,specon_tech,deep,species,
                                   rectangle_type,csq_y,csq_x,c_square,totwghtlandg,totvallandg,
                                   confidential,id)]
  setnames(fdi.csq,old = c("csq_y","csq_x"), new = c("latitude","longitude"))
  nrow(fdi.csq[is.na(longitude),])
  nrow(fdi.csq[is.na(latitude),])
  # Checking if coords are consistent with the rectangle type
  # Check coordinates according to the type of rectangle
  fdi.coords[,`:=`(remainder_lon=longitude%%1,
                   remainder_lat=latitude%%1)]
  fdi.coords[rectangle_type == "5*5",`:=`(remainder_lon=longitude%%5,
                                          remainder_lat=latitude%%5)]
  fdi.coords[(rectangle_type=="05*05" & (!remainder_lon %in% c(0.25,0.75) | !remainder_lat %in% c(0.25,0.75))) |
               (rectangle_type=="05*1" & (remainder_lon != 0.50 | !remainder_lat %in% c(0.25,0.75))) |
               (rectangle_type=="1*1" & (remainder_lon != 0.50 | remainder_lat != 0.50)) |
               (rectangle_type=="5*5" & (remainder_lon != 2.5 | remainder_lat != 2.5)),
             valid:="NO"]

  rect.check<-fdi.coords[,.(nrows=.N),by=.(country,rectangle_type,remainder_lon,remainder_lat,valid)]

  # Points on land
  csq05Land$geometry <- NULL
  fdi.coords <- merge(fdi.coords,csq05Land[,c("type","csq_x","csq_y")],
                      by.x = c("longitude","latitude"),
                      by.y = c("csq_x","csq_y"),
                      all.x = TRUE)

  #fdi.coords[type=='land',valid:='NO']
  if (ms=='FRA') {
    fdi.coords[country=="FRA" & latitude==43.25 & longitude==2.75, valid:=NA]
  }
  if (ms=='HRV') {
    fdi.coords[country=="HRV" & latitude==43.25 & longitude==17.75, valid:=NA]
  }
  fdi.coords.on.land <- fdi.coords[type=='land',]

  fdi.csq <- merge(fdi.csq,csq05Land[,c("type","cscode")],
                   by.x = "c_square",
                   by.y = "cscode",
                   all.x = TRUE)
  #fdi.csq[type=='land',valid:='NO']

  if (ms=='FRA') {
    fdi.csq[country=="FRA" & latitude==43.25 & longitude==2.75, valid:=NA]
  }

  if (ms=='HRV') {
    fdi.csq[country=="HRV" & latitude==43.25 & longitude==17.75, valid:=NA]
  }

  fdi.csq.on.land <- fdi.csq[type=='land',]
  fdi.csq.coords <- merge(fdi.csq.coords,csq05Land[,c("type","cscode")],
                          by.x = "c_square",
                          by.y = "cscode",
                          all.x = TRUE)
  #fdi.csq.coords[type=='land',valid:='NO']

  if (ms=='FRA') {
    fdi.csq.coords[country=="FRA" & latitude==43.25 & longitude==2.75, valid:=NA]
  }

  if (ms=='HRV') {
    fdi.csq.coords[country=="HRV" & latitude==43.25 & longitude==17.75, valid:=NA]
  }

  fdi.csq.coords.on.land <- fdi.csq.coords[type=='land',]
  fdi.csq.on.land    <- fdi.csq.on.land[,lapply(.SD,as.character)]
  fdi.coords.on.land <- fdi.coords.on.land[,lapply(.SD,as.character)]
  cols <- names(fdi.coords.on.land)[names(fdi.coords.on.land)%in% names(fdi.csq.on.land)]

  points.on.land <- rbind(fdi.csq.on.land,
                          fdi.coords.on.land[,.SD,.SDcols = cols])
  if (ms=='FRA') {
    points.on.land <- points.on.land[!(country=="FRA" & latitude==43.25 & longitude==2.75),]
  }

  if (ms=='HRV') {
    points.on.land <- points.on.land[!(country=="HRV" & latitude==43.25 & longitude==17.75),]
  }

  # SAVING ----
  #setwd(outPath)
  ifelse(nrow(points.on.land)>0,
         points.on.land[,fwrite(.SD, paste0(outPath,"/Table.H.points.on.land","_",country,'.csv')),by=.(country)],
         'NO RECORDS')
  # setwd(dataF)

  errors.csq.rectangle_type$valid <-'NO'
  errors.csq.rectangle_type$type  <- NA
  errors.csq.coords$valid         <-'NO'

  # At the end we have the following errors
  cols <- names(fdi)
  errors.rect.check <- fdi.coords[valid=='NO',]

  # SAVING ----
  #setwd(outPath)
  ifelse(nrow(errors.rect.check)>0,
         errors.rect.check[,fwrite(.SD, paste0(outPath,"/Table.H.errors.rect.check","_",country,'.csv')),by=.(country)],
         'NO RECORDS')
  # setwd(dataF)

  errors.ids <- unique(
    c(
      errors.lat.lon.bounds$id,
      errors.no.lat.lon.no.csq$id,
      errors.one.coord$id,
      errors.rect.only$id,
      errors.csq.coords$id,
      errors.csq.rectangle_type$id,
      errors.rect.check$id,
      fdi.coords.on.land$id,
      fdi.csq.on.land$id,
      errors.csq$id
    )
  )

  cols <- names(fdi)
  fdi.no.csq <- fdi[!id%in%fdi.csq$id,]

  fdi <- NULL
  csq05 <- NULL
  csq05Land <- NULL
  errors.lat.lon.bounds<- NULL
  errors.no.lat.lon.no.csq<- NULL
  errors.one.coord<- NULL
  errors.rect.only<- NULL
  errors.csq.coords<- NULL
  errors.csq.rectangle_type<- NULL
  errors.rect.check<- NULL
  fdi.coords.on.land<- NULL
  fdi.csq.on.land<- NULL
  fdi.coords <- NULL
  fdi.csq.coords <- NULL
  fdi.csq.coords.on.land <- NULL
  points.on.land <- NULL

  fdi <- rbind(fdi.csq[,!c("type","valid")],fdi.no.csq,fill=TRUE)
  fdi.csq <- NULL;
  fdi.no.csq <- NULL;
  fdi <- fdi[, valid := "Y"]
  fdi <- fdi[id %in% errors.ids, valid := "N"]

  if (ms=='FRA') {
    fdi[country=="FRA" & latitude==43.25 & longitude==2.75, valid:="Y"]
  }

  if (ms=='HRV') {
    fdi[country=="HRV" & latitude==43.25 & longitude==17.75, valid:="Y"]
  }
  gc()

  fwrite(fdi,paste0(outPath,"/",ms,'_table_h_total_valid_and_not.csv'))

  nrow(fdi[valid=='N'])
  nrow(fdi[valid=='N'])/nrow(fdi)*100

  #setwd(outPath)
  # A bit of recap. We will create a table with the zero zero coords.
  zero0 <- fdi[longitude == 0 & latitude == 0,]
  # We find two unique countries: MLT and FRA. Ask
  unique(zero0$country)
  # We will now select the minus 1 minus 1 coords. It looks like for table I. It is only # HRV. It has been communicated adn uploaded on the ftp (together with the other tables).
  # Igor, the correposndent said that this 46 records represent a mistake and so we deleted them.
  minus1  <- fdi[(longitude == -1 & latitude == -1),]

  # SAVING ----
  #setwd(outPath)
  ifelse(nrow(zero0)>0,
         zero0[,fwrite(.SD, paste0(outPath,"/Table.H.zero0","_",country,'.csv')),by=.(country)],'NO RECORDS')
  ifelse(nrow(minus1)>0,
         minus1[,fwrite(.SD, paste0(outPath,"/Table.H.minus1","_",country,'.csv')),by=.(country)],'NO RECORDS')

  gc()
  #setwd(dataF)
  # List of the gears
  gearsFDI       <- unique(fdi$gear_type)
  # List of the mesh size
  meshSize       <- unique(fdi$mesh_size_range)
  # table(fdi$gear_type,fdi$mesh_size_range)
  # Defining the trawl macro area aggregation
  trawl          <- as.list(c("OTB", "PTB", "OTM", "PTM", "OTT"))
  fdi[gear_type %in%  trawl, gear_typeN := "TRAWL",]
  fdi[!gear_type %in% trawl, gear_typeN := gear_type, ]
  # unique mesh size
  TRAWLMs         <- fdi[gear_typeN == "TRAWL", ]
  TRAWLMsu        <- TRAWLMs[, unique(mesh_size_range), by = "gear_typeN"]
  names(TRAWLMsu) <- c("gear", "meshsize")
  msU             <- unique(TRAWLMsu$meshsize)
  sort(msU)
  # checking which country used the 90D105 mesh size range
  mesh90d105      <- fdi[mesh_size_range == "90D105",]
  unique(mesh90d105$country)

  m100            <- as.list(c("100D110", "100D120", "100DXX","105D110", "110D120", "110DXX",  "120DXX",
                               "100D400", "400DXX"))
  l100            <- as.list(c("00D14","14D16" , "14D20","00D16", "16D20", "16D32", "20D40","40D50" ,
                               "32D70", "32D80","50D100" , "32D90", "40D45", "40D55", "45D50","50D100",
                               "50D65","55D60", "65D100", "60D65", "65D70", "70D100","70D80", "70S90",
                               "80D100","65D70", "70D100","90D105", "80D100", "70D80","00S40", "40SXX",
                               "70S90"))

  fdi[gear_typeN == "TRAWL" & mesh_size_range == "NK",
      gear_typeN := "TRAWLNONE", ]

  fdi[gear_typeN == "TRAWL" & mesh_size_range %in% m100,
      gear_typeN := "TRAWLM100", ]

  fdi[gear_typeN == "TRAWL" & mesh_size_range %in% l100,
      gear_typeN := "TRAWLL100", ]

  unique(fdi$gear_typeN)

  tbbMSU                <- fdi[ gear_typeN == "TBB", unique(mesh_size_range),]
  sort(tbbMSU)

  m120                  <- as.list(c("120DXX", "100D400","400DXX"))

  # The 100DXX mesh size range is presented in 8 lines.
  # Accordingly metier these lines could be assigned as TBBL120.

  l120                  <- as.list(c("00D14","14D16" , "14D20","00D16", "16D20", "16D32", "20D40","40D50" ,
                                     "32D70", "32D80","50D100" , "32D90", "40D45", "40D55", "45D50","50D100",
                                     "50D65","55D60", "65D100", "60D65", "65D70", "70D100","70D80", "70S90",
                                     "80D100","65D70", "70D100","90D105", "80D100", "70D80","00S40", "40SXX",
                                     "70S90", "105D110", "100D110", "110D120", "100DXX"))

  fdi[gear_typeN == "TBB" & mesh_size_range == "NK", gear_typeN := "TBBNONE",]
  fdi[gear_typeN == "TBB" & mesh_size_range %in% m120, gear_typeN := "TBBM120",]
  fdi[gear_typeN == "TBB" & mesh_size_range %in% l120, gear_typeN := "TBBL120",]
  gear_typeU             <- unique(fdi$gear_typeN)
  sort(gear_typeU)

  #########
  #Maksims#
  #########

  #Define gear classes

  seine   <- as.list(c("SDN", "SPR", "SSC", "SB", "SV"))
  nets    <- as.list(c("GND", "GNS", "GNC", "GTR", "GTN"))
  dredges <- as.list(c("DRB", "HMD", "DRH"))
  hooks   <- as.list(c("LHM", "LHP", "LLD", "LLS", "LTL"))
  snets   <- as.list(c("PS", "LA"))
  traps   <- as.list(c("FPO", "FPN", "FYK"))
  gc()
  fdi[gear_typeN %in% seine, gear_typeN := "SEINE",]
  fdi[gear_typeN %in% nets, gear_typeN := "NETS",]
  fdi[gear_typeN %in% dredges, gear_typeN := "DREDGES",]
  fdi[gear_typeN %in% hooks, gear_typeN := "HOOKS",]
  fdi[gear_typeN %in% snets, gear_typeN := "sNETS",]
  fdi[gear_typeN %in% traps, gear_typeN := "TRAPS",]

  geartypeU             <- unique(fdi$gear_typeN)


  gclasses <- as.list(c("DREDGES", "HOOKS", "NETS", "SEINE", "sNETS", "TBBL120",
                        "TBBM120", "TBBNONE", "TRAPS", "TRAWLL100", "TRAWLM100",
                        "TRAWLNONE"))
  # if(i == "table.I") svalue <- "fishing_days=sum(totwghtlandg)" else svalue <- "landings=sum(totwghtlandg)"

  #fdi <-
  gearNOTingclasses <- fdi[!gear_typeN %in% gclasses]
  # SAVING ----
  #setwd(outPath)
  ifelse(nrow(gearNOTingclasses)>0,
         gearNOTingclasses[,fwrite(.SD, paste0(outPath,"/Table.H.gearNOTingclasses","_",country,'.csv')),by=.(country)],'NO RECORDS')
  #setwd(dataF)

  fdi <- fdi[gear_typeN %in% gclasses,]
  # fdi <- fdi %>%
  #   group_by(country, year, quarter, gear_typeN, specon_tech, sub_region, rectangle_type,
  #            latitude, longitude, confidential) %>%
  #   summarise(value=eval(parse(text = svalue)))
  # fdi<-ungroup(fdi)

  fdi <- fdi[, .("totwghtlandg" = sum(totwghtlandg, na.rm = T),
                 "totvallandg"  = sum(as.numeric(totvallandg), na.rm = T)),
               by = .(
                 country,
                 year,
                 quarter,
                 gear_typeN,
                 specon_tech,
                 sub_region,
                 rectangle_type,
                 c_square,
                 latitude,
                 longitude,
                 confidential,
                 valid
               )]

  # fdi[,`:=`(totwghtlandg = V1,
  #           V1 = NULL)]
  # SAVING ----
  #setwd(dataF)
  save(fdi,file=paste(outPath,"/",ms,"_fdi_", i, ".RData", sep=''))

  #setwd(outPath)
  fdi_TABLE_H_errors <- fdi[valid == 'N']
  #fwrite(fdi_TABLE_H_errors,'../output/GRC_fdi_TABLE_H_errors.csv')
  fwrite(fdi_TABLE_H_errors,paste0(outPath,"/",ms,'_fdi_TABLE_H_errors.csv'))
  #rm(list=ls())
  gc()

  # #-------------------------------------------------------------------------------
  # # 1_EWG-FDI_spatial_landings_wght(rev-SK)
  # # Script to clean, analyse and map the spatial effort and spatial landings
  # # datasets of the FDI EWG22-10 20220912 - 20220916
  # # Tor 3 team : Maciej, Maksims, Maurizio, Stefanos. Every
  # # contribution is highlighted.
  # # Contact: maurizio.gibin@gmail.com
  # #
  # # Date: 2022-09-12 - 2022-09-16
  # #
  # #
  # #-------------------------------------------------------------------------------
  # ###################
  # # Maciej Adamowicz
  # # 14.09.2022
  # ###################
  options(scipen = 999)
  options(digits = 9)

  #setwd(csqF)
  load(file = paste0(csqF,"grids.RData"))
  # FDI DATA ----
  #setwd(dataF)

  ####################
  #Maciej 19.09.2019 #
  ####################

  load(file=paste0(dataF,ms,"_fdi_Table.H.RData"))

  fdi <- as.data.table(fdi)
  # fdidp <- fdi %>%
  #   group_by(country, year, quarter, gear_typeN, specon_tech, sub_region, rectangle_type,
  #            latitude, longitude, confidential,valid) %>%
  #   summarise(totwghtlandg=sum(totwghtlandg,na.rm=T))
  # fdidp<-ungroup(fdidp)

  fdi <- fdi[, .("totwghtlandg" = sum(totwghtlandg, na.rm = T),
                 "totvallandg" = sum(totvallandg, na.rm = T)),
               by = .(
                 country,
                 year,
                 quarter,
                 gear_typeN,
                 specon_tech,
                 sub_region,
                 rectangle_type,
                 latitude,
                 longitude,
                 confidential,
                 valid
               )]

  # fdiDT <- as.data.table(fdi)
  # errors.unit.weight.vallandg <- fdiDT[,.("totwghtlandg" = round(sum(totwghtlandg,na.rm = T),0),
  #                                         "totvallandg"  = round(sum(totvallandg, na.rm = T),0)),
  #                           by=.(country,year)]
  # setorder(errors.unit.weight,country,year)
  # # errors.unit.weightDT <- dcast(errors.unit.weight, country ~ year, value.var = "totwghtlandg")
  # fwrite(errors.unit.weight,paste0(outPath,'errors.unit.weight.value.table.H.csv'))

  # After a consultation with the group we omit the records form CYP for the extreme values
  # fdi <- fdi[!(country == "CYP" & year =='2017'),]

  #Loading the file with subregions assigned to fishing zones
  #setwd(fshzn)
  fishing_zones <- fread(paste0(fshzn,"fishing_zones_2022.csv"), stringsAsFactors = F)
  #setwd(dataF)
  fishing_zones$sub_region<-tolower(fishing_zones$sub_region)
  fdi$sub_region<-tolower(fdi$sub_region)
  #Assign fishing zones to the fdi data
  fdi <- left_join(fdi,fishing_zones,by="sub_region")
  fdi<-data.table(fdi)
  if(nrow(fdi[is.na(fishing_zone)])){
    stop(paste("Fishing zones could not be assigned to the following subregions:",
               fdi[is.na(fishing_zone), .(unique(sub_region))]))
  } else{
    print("Fishing zones assigned successfuly.")
  }

  fdi[is.na(fishing_zone) | fishing_zone=="",unique(sub_region)]
  fwrite(fdi[sub_region=="nk",.(nrows=.N),by=.(country,year,confidential,totwghtlandg,totvallandg,valid)],paste0(outPath,"/",ms,"_Table.H.missing.subregion.csv"))

  #Remove rows with sub_region = NK and remove BSAs
  fdi<-fdi[!sub_region %in% c("nk","bsa")]
  #Check if all rows have a fishing zone assigned
  unique(fdi[is.na(fishing_zone),.(sub_region)])

  #Remove  incorrect data
  fdi<-fdi[valid=="Y"]
  unique(fdi$rectangle_type)
  fdi[is.na(rectangle_type),rectangle_type:="05*05"]
  fdi.rectangle.na.csq<-fdi[is.na(rectangle_type)]

  #Create id for each lon/lat combination in the fdi data
  fdi <- mutate(fdi,
                rect_id = paste(as.character(longitude),as.character(latitude),sep = '/'))
  #Create id containing lon and lat of the centroid for each ICES rectangle
  icesr <- mutate(icesr,rect_id=paste(as.character(ices_x),as.character(ices_y),sep = '/'))

  #Join fdi data with ices rectangles dataset
  fdi <- left_join(fdi,icesr,by="rect_id")
  fdi <- select(fdi,country:icesname)

  #Keep the the data with the ICES rectangles assigned in a separate dataset
  fdi.ices<-filter(fdi,!is.na(icesname))
  sum(fdi.ices$totwghtlandg)

  #Join the fdi.ices dataset with c-squares dataset. Warning! Fishing days will be doubled.
  fdi.ices<-left_join(fdi.ices,csq05,by="icesname")
  #Divide fishing days by 2 (each ICES rectangle has 2 c-squares)
  fdi.ices<-mutate(fdi.ices,totwghtlandg=totwghtlandg/2)
  #Check if the totat weight of landings remained the same
  sum(fdi.ices$totwghtlandg)
  fdi.ices<-select(fdi.ices,country:icesname,cscode)

  #Keep the the data with the ICES rectangles NOT assigned in a separate dataset
  fdi.not.ices<-filter(fdi,is.na(icesname))
  fdi.not.ices.05.1<-filter(fdi.not.ices,rectangle_type=="05*1")
  #Handle 0.5x1 rectangles outside ICES area
  #Create the ids containing lon and lat of the centre left and centre right of the csquare
  sum(fdi.not.ices.05.1$totwghtlandg)
  csq05<-mutate(csq05,
                rect_id=paste(as.character(w_csq),as.character(csq_y),sep='/'))#rect_id = centre/left
  fdi.not.ices.05.1.left<-left_join(fdi.not.ices.05.1,csq05,by="rect_id") %>%
    mutate(totwghtlandg=totwghtlandg/2)

  csq05<-mutate(csq05,
                rect_id=paste(as.character(e_csq),as.character(csq_y),sep='/'))#rect_id = centre/right
  fdi.not.ices.05.1.right<-left_join(fdi.not.ices.05.1,csq05,by="rect_id") %>%
    mutate(totwghtlandg=totwghtlandg/2)
  fdi.not.ices.05.1<-rbind(fdi.not.ices.05.1.left,fdi.not.ices.05.1.right)
  fdi.not.ices.05.1<-select(fdi.not.ices.05.1,country:icesname.x,cscode)
  fdi.not.ices.05.1<-rename(fdi.not.ices.05.1,icesname=icesname.x)
  sum(fdi.not.ices.05.1$totwghtlandg)

  print(paste0("fdi.not.ices.05.1 number of rows without cscode: ",nrow(filter(fdi.not.ices.05.1,is.na(cscode)))))
  temp<-filter(fdi.not.ices.05.1,is.na(cscode)) %>%
    select(country:icesname)
  fdi.not.ices.05.1<-filter(fdi.not.ices.05.1,!is.na(cscode))

  # Check what rectangle types are present in the rest of the data
  fdi.not.ices %>%
    filter(rectangle_type!="05*1") %>%
    group_by(rectangle_type) %>%
    summarise(n=n())

  # The rest of the data has 05*05, 1*1 and 5*5 rectangle_type

  # Handle 05*05 rectangles
  fdi.csq.05.05 <- filter(fdi.not.ices,rectangle_type=="05*05") %>%
    rename(csq_c_id=rect_id)

  # Create the id containing lon and lat of the centroid for each c-square
  csq05<-mutate(csq05,
                csq_c_id=paste(as.character(csq_x),as.character(csq_y),sep='/'))


  # Join the fdi.csq.c dataset with c-squares dataset.
  fdi.csq.05.05<-left_join(fdi.csq.05.05,csq05,by="csq_c_id")
  fdi.csq.05.05<-select(fdi.csq.05.05,country:icesname.x,cscode)

  # Check if there are any rows without csquare assigned
  print(paste0("fdi.csq.05.05 number of rows without cscode: ",nrow(filter(fdi.csq.05.05,is.na(cscode)))))

  # Handle 1*1 rectangles
  fdi.csq.1.1 <- filter(fdi.not.ices,rectangle_type=="1*1") %>%
    rename(csq_c_id=rect_id)

  csquares.1.1 <- select(fdi.csq.1.1,csq_c_id,longitude,latitude) %>%
    mutate(key=1) %>%
    distinct()
  grid.for.1.1 <- data.frame(lon_diff = c(0,0.5,0.5,0),
                             lat_diff = c(0,0,0.5,0.5),
                             key= c(1))
  csquares.1.1 <- inner_join(csquares.1.1,grid.for.1.1,by="key") %>%
    mutate(bl_lon = longitude - lon_diff,
           bl_lat = latitude - lat_diff) %>%
    select(csq_c_id, bl_lon, bl_lat)

  sum(fdi.csq.1.1$totwghtlandg)
  fdi.csq.1.1 <- fdi.csq.1.1 %>%
    inner_join(csquares.1.1,by="csq_c_id") %>%
    mutate(totwghtlandg=totwghtlandg/4,
           rect_id=paste(as.character(bl_lon),as.character(bl_lat),sep='/'))
  sum(fdi.csq.1.1$totwghtlandg)

  # create bottom-left id of csquare
  csq05<-mutate(csq05,
                rect_id=paste(as.character(w_csq),as.character(s_csq),sep='/'))
  fdi.csq.1.1<-left_join(fdi.csq.1.1,csq05,by="rect_id")
  fdi.csq.1.1<-select(fdi.csq.1.1,country:fishing_zone,rect_id,icesname.x,cscode)
  print(paste0("fdi.csq.1.1 number of rows without cscode: ",nrow(filter(fdi.csq.1.1,is.na(cscode)))))

  # Handle 5*5 rectangles
  fdi.csq.5.5 <- filter(fdi.not.ices,rectangle_type=="5*5") %>%
    rename(csq_c_id=rect_id)

  csquares.5.5 <- select(fdi.csq.5.5,csq_c_id,longitude,latitude) %>%
    mutate(key=1) %>%
    distinct()

  grid.for.5.5 <- inner_join(data.frame(lon_diff=seq(-2, by=0.5, length.out = 10), key=1),
                             data.frame(lat_diff=seq(-2, by=0.5, length.out = 10), key=1),
                             by="key", relationship = "many-to-many")

  csquares.5.5 <- inner_join(csquares.5.5,grid.for.5.5,by="key") %>%
    mutate(bl_lon = longitude - lon_diff,
           bl_lat = latitude - lat_diff) %>%
    select(csq_c_id, bl_lon, bl_lat)

  sum(fdi.csq.5.5$totwghtlandg)
  fdi.csq.5.5 <- fdi.csq.5.5 %>%
    inner_join(csquares.5.5,by="csq_c_id") %>%
    mutate(totwghtlandg=totwghtlandg/100,
           rect_id=paste(as.character(bl_lon),as.character(bl_lat),sep='/'))
  sum(fdi.csq.5.5$totwghtlandg)

  # create bottom-left id of csquare
  csq05<-mutate(csq05,
                rect_id=paste(as.character(w_csq),as.character(s_csq),sep='/'))
  fdi.csq.5.5<-left_join(fdi.csq.5.5,csq05,by="rect_id")
  fdi.csq.5.5<-select(fdi.csq.5.5,country:fishing_zone,rect_id,icesname.x,cscode)
  print(paste0("fdi.csq.5.5 number of rows without cscode: ",nrow(filter(fdi.csq.5.5,is.na(cscode)))))

  fdi.ices<-rename(fdi.ices,geo_id=rect_id)
  fdi.not.ices.05.1<-rename(fdi.not.ices.05.1,geo_id=rect_id)
  fdi.csq.05.05<-rename(fdi.csq.05.05,geo_id=csq_c_id,icesname=icesname.x)
  fdi.csq.1.1<-rename(fdi.csq.1.1,geo_id=rect_id,icesname=icesname.x)
  fdi.csq.5.5<-rename(fdi.csq.5.5,geo_id=rect_id,icesname=icesname.x)

  result<-fdi.ices %>%
    rbind(fdi.csq.05.05) %>%
    rbind(fdi.not.ices.05.1) %>%
    rbind(fdi.csq.1.1) %>%
    rbind(fdi.csq.5.5) %>%
    as.data.frame()

  print(paste0("number of rows without cscode: ",nrow(filter(result,is.na(cscode)))))

  result <- as.data.table(result)
  result <-
    result[, sum(totwghtlandg,na.rm=T), by = .(
      country,
      year,
      quarter,
      gear_typeN,
      specon_tech,
      sub_region,
      fishing_zone,
      icesname,
      confidential,
      cscode
    )]
  result[,`:=`(totwghtlandg = V1,
               V1 = NULL)]


  result<-left_join(result,csq05,by="cscode") %>%
    select(country:totwghtlandg,geometry) %>%
    rename(icesname=icesname.x)


  print(paste0("Are total landings (weight) correct? : ",round(sum(result$totwghtlandg),6)==round(sum(fdi$totwghtlandg),6)))

  result_sf<-st_sf(result)
  #setwd(outPath)
  st_write(result_sf,layer=paste0(outPath,"/",ms,"_spatial_landings.shp"),dsn=".",driver="ESRI Shapefile", delete_layer = TRUE)
  save(result,file=paste0(outPath,"/",ms,'_spatial_landings.RData'))

  if (!file.exists(paste0(outPath,"/landings"))) { dir.create(paste0(outPath,"/landings")) }
  if (!file.exists(paste0(outPath,"/landings/areas"))) { dir.create(paste0(outPath,"/landings/areas")) }
  if (!file.exists(paste0(outPath,"/landings/errors"))) { dir.create(paste0(outPath,"/landings/errors")) }
  if (!file.exists(paste0(outPath,"/landings/gears"))) { dir.create(paste0(outPath,"/landings/gears")) }
  if (!file.exists(paste0(outPath,"/landings/specons"))) { dir.create(paste0(outPath,"/landings/specons")) }

  fList <- list.files(path=outPath,pattern=glob2rx('*H*.csv'))
  file.copy(fList,paste0(outPath,'/landings/'),overwrite = T)
  file.remove(fList)

  #-------------------------------------------------------------------------------
  # 2_EWG-FDI_mapping_H(rev-SK)
  # Script to clean, analyse and map the spatial effort and spatial landings
  # datasets of the FDI EWG22-10 20220912 - 20220916
  # Tor 3 team : Maciej, Maksims, Maurizio, Stefanos. Every
  # contribution is highlighted.
  # Contact: maurizio.gibin@gmail.com
  #
  # Date: 2022-09-12 - 2022-09-16
  #
  #
  #-------------------------------------------------------------------------------


  # #########
  # #Maksims#
  # #########

  #setwd(codePath)
  source(paste0(codePath,'CSquareToLonLat.R'))
  world <- map_data('world')

  #setwd(fshzn)
  fz_limits <- read.csv(paste0(fshzn,"fishing_zones_limits.csv"), sep=',')

  # ---------------- LANDINGS MAPS ----------------
  # setwd(outPath)

  load(file = paste0(outPath,"/",ms,"_spatial_landings.RData"))
  unique(result$specon_tech)
  result_sf <- result
  result_sf <- as.data.table(result_sf)

  result_sf[totwghtlandg<=1, totwghtlandg := totwghtlandg+1]

  cc <- result_sf[, CSquare2LonLat(result_sf$cscode, 0.5)]
  result_sf <- cbind(result_sf, cc)
  result_sf <- result_sf[, .(lat = SI_LATI, lon = SI_LONG), by = .(country,year,quarter,gear_typeN,specon_tech,sub_region,
                                                                   fishing_zone,icesname,confidential,cscode, totwghtlandg)]

  #Group and sum fishing days per fishing zone, gear type, specon_lo
  result_sum <- result_sf[, .(totwghtlandg=sum(totwghtlandg,na.rm = T), value=log(sum(totwghtlandg))),
                            by = .(fishing_zone, gear_typeN, specon_tech, year, confidential,
                                   cscode, lon, lat)]


  #setwd(outPath)
  # #Plot all fishing areas
  # # OBS !!! is not working
  #
  # for(i in fz_limits$id){
  #   xmin <- fz_limits$lonMin[i]
  #   xmax <- fz_limits$lonMax[i]
  #   ymin <- fz_limits$latMin[i]
  #   ymax <- fz_limits$latMax[i]
  #   pl <- ggplot(if(fz_limits$fishing_zone[i]=="Earth"){
  #     result_plot <- result_sum[, .(value=log(sum(totwghtlandg,na.rm=T))), by = .(year, lon, lat)]}
  #     else result_plot <- result_sum[fishing_zone==fz_limits$fishing_zone[i]])+
  #     theme_bw() +
  #     geom_tile(aes(lon, lat, fill = value)) +
  #     coord_quickmap(xlim = c(xmin,xmax), ylim = c(ymin, ymax))+
  #     geom_polygon(data=world, aes(long, lat, group=group), fill = "#909191")+
  #     scale_fill_continuous(low = "yellow", high = "red",na.value = NA) +
  #     labs(x = NULL, y = NULL, fill = "Logarithm of \nLandings")
  #   d1 <- pl + facet_wrap( ~ year, nrow = 2) + theme(strip.text.x = element_text(size = 6),
  #                                                    axis.text = element_text(size = 6),
  #                                                    legend.title = element_text(size = 7),
  #                                                    legend.position= "bottom",
  #                                                    # legend.key.size = unit(0.3, "cm"),
  #                                                    legend.text = element_text(size = 7))
  #   fname <- paste(outPath,"/landings/areas/", gsub(' ','_',fz_limits$fishing_zone[i]), "_log_of_landings_GRC.png", sep = "")
  #   ggsave(filename=fname, plot=d1)
  # }


  #Plot specons
  result_lo <- result_sf %>%
    group_by(year, specon_tech, confidential, lon, lat) %>%
    summarise(totwghtlandg=sum(totwghtlandg)) %>%
    mutate(value=log(totwghtlandg))

  specons <- unique(result_lo$specon_tech)
  specons <- unique(result_lo$specon_tech)
  specons <- specons[!is.na(specons)]

  for(i in specons){
    r <- result_lo %>% filter(specon_tech==i)
    #r <- result_lo[specon_tech==i]
    xmin <- min(r$lon)
    xmax <- max(r$lon)
    ymin <- min(r$lat)
    ymax <- max(r$lat)
    pl <- ggplot(r)  +
      theme_bw() +
      geom_tile(aes(lon, lat, fill = value)) +
      coord_quickmap(xlim = c(xmin,xmax), ylim = c(ymin, ymax))+
      geom_polygon(data=world, aes(long, lat, group=group), fill = "#909191")+
      scale_fill_continuous(low = "yellow", high = "red") +
      labs(x = NULL, y = NULL, fill = "Logarithm of \nLandings")
    d1 <- pl + facet_wrap( ~ year, nrow = 2) + theme(strip.text.x = element_text(size = 6),
                                                     axis.text = element_text(size = 6),
                                                     legend.title = element_text(size = 7),
                                                     legend.position= "bottom",
                                                     # legend.key.size = unit(0.3, "cm"),                                                   legend.key.size = unit(0.3, "cm"),
                                                     legend.text = element_text(size = 7))
    fname <- paste(outPath,"/landings/specons/", i, "_log_of_landings_",ms,".png", sep = "")
    ggsave(filename=fname, plot=d1)
  }

  lstpolys <- lapply(1:nrow(fz_limits), function(x) {
    dt <- data.table(
      lon = c(
        fz_limits[x,]$lonMin,
        fz_limits[x,]$lonMin,
        fz_limits[x,]$lonMax,
        fz_limits[x,]$lonMax,
        fz_limits[x,]$lonMin
      ),
      lat = c(
        fz_limits[x,]$latMin,
        fz_limits[x,]$latMax,
        fz_limits[x,]$latMax,
        fz_limits[x,]$latMin,
        fz_limits[x,]$latMin
      ),
      group = x,
      order = 1:5
    )
    # poly <- st_polygon(list(as.matrix(dt)))
  })
  lstpolysdf <- rbindlist(lstpolys)


  # #Plot errors
  # # OBS !!! is not working
  # for(i in fz_limits$id){
  #   if(fz_limits$fishing_zone[i]=="Earth") next
  #   else r <- result_sum %>% filter(fishing_zone==fz_limits$fishing_zone[i])
  #   #r <- result_sum[fishing_zone==fz_limits$fishing_zone[i]]
  #   xmin <- min(r$lon)
  #   xmax <- max(r$lon)
  #   ymin <- min(r$lat)
  #   ymax <- max(r$lat)
  #   pl <- ggplot(r)  +
  #     theme_bw() +
  #     geom_tile(aes(lon, lat, fill = value)) +
  #     coord_quickmap(xlim = c(xmin,xmax), ylim = c(ymin, ymax))+
  #     geom_polygon(data=world, aes(long, lat, group=group), fill = "#909191")+
  #     geom_polygon(data = lstpolysdf[lstpolysdf$group ==i], aes(lon, lat, group=group), fill = NA, colour = 'black')+
  #     scale_fill_continuous(low = "yellow", high = "red") +
  #     labs(x = NULL, y = NULL, fill = "Logarithm of \nLandings")
  #   d1 <- pl + facet_wrap( ~ year, nrow = 2) + theme(strip.text.x = element_text(size = 6),
  #                                                    axis.text = element_text(size = 6),
  #                                                    legend.title = element_text(size = 7),
  #                                                    legend.position= "bottom",
  #                                                    # legend.key.size = unit(0.3, "cm"),                                                   legend.key.size = unit(0.3, "cm"),
  #                                                    legend.text = element_text(size = 7))
  #   fname <- paste(outPath,"/landings/errors/", gsub(' ','_',fz_limits$fishing_zone[i]), "_errors_log_of_landings_GRC.png", sep = "")
  #   ggsave(filename=fname, plot=d1)
  # }

  #Plot gear types
  result_gr <- result_sf %>%
    group_by(gear_typeN, year, confidential, lon, lat) %>%
    summarise(totwghtlandg=sum(totwghtlandg)) %>%
    mutate(value=log(totwghtlandg))
  gr <- unique(result_gr$gear_typeN)
  for(i in gr){
    r <- result_gr %>% filter(gear_typeN==i)
    #r <- result_gr[gear_typeN==i]
    xmin <- min(r$lon)
    xmax <- max(r$lon)
    ymin <- min(r$lat)
    ymax <- max(r$lat)
    pl <- ggplot(r)  +
      theme_bw() +
      geom_tile(aes(lon, lat, fill = value)) +
      coord_quickmap(xlim = c(xmin,xmax), ylim = c(ymin, ymax))+
      geom_polygon(data=world, aes(long, lat, group=group), fill = "#909191")+
      scale_fill_continuous(low = "yellow", high = "red") +
      labs(x = NULL, y = NULL, fill = "Logarithm of \nLandings")
    d1 <- pl + facet_wrap( ~ year, nrow = 2) + theme(strip.text.x = element_text(size = 6),
                                                     axis.text = element_text(size = 6),
                                                     legend.title = element_text(size = 7),
                                                     legend.position= "bottom",
                                                     # legend.key.size = unit(0.3, "cm"),                                                   legend.key.size = unit(0.3, "cm"),
                                                     legend.text = element_text(size = 7))
    fname <- paste(outPath,"/landings/gears/", i, "_log_of_landings_",ms,".png", sep = "")
    ggsave(filename=fname, plot=d1)
  }
  # zipfile <- tempfile()

  zipfile <- paste0(tmp,"/", stringi::stri_rand_strings(1, 10, pattern = "[A-Za-z0-9]"), ".zip")
  while (file.exists(zipfile)) {
    zipfile <- paste0(tmp,"/", stringi::stri_rand_strings(1, 10, pattern = "[A-Za-z0-9]"), ".zip")
  }

  # List all files in the directory and its subdirectories
  filestozip <- list.files(tmp, recursive = TRUE, full.names = TRUE)

  # Create the zip file without including the leading "./" in the paths
  zip::zip(zipfile, files = filestozip, root = tmp)

  # zip::zip(zipfile, ".", root = tmp)

  val <- readBin(zipfile, "raw", n=file.info(zipfile)$size)
  if (file.exists(tmp)) {
    unlink(tmp, recursive=T)
  }
  if (file.exists(zipfile)) {
    unlink(zipfile)
  }
  as_attachment(val, "fdiextrah-results.zip")

