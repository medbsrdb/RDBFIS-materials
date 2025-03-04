bioindexdocker/app
library(plumber)
library(BioIndex)
library(RoME)
library(dplyr)
library(zip)
library(utils)
library(stringr)

#* @param f:file
#* @param country
#* @param species
#* @param area
#* @param recThreshold
#* @param spawThreshold
#* @param haulThreshold
#* @param sex
#* @param depth
#* @param longitude
#* @param latitude
#* @param buffer
#* @param resolution
#* @param depthLines
#* @serializer octet
#* @post /bioindex
function(f,country, species, area, recThreshold, spawThreshold, haulThreshold, sex, depth, longitude, latitude, buffer, resolution, depthLines) {
  tmp <- paste0("/tmp/", stringi::stri_rand_strings(1, 7, pattern = "[A-Za-z0-9]"))
  while (file.exists(tmp)) {
    tmp <- paste0("/tmp/", stringi::stri_rand_strings(1, 7, pattern = "[A-Za-z0-9]"))
  }
  dir.create(tmp)
  dir.create(file.path(tmp, "wd"))
  wd <- file.path(tmp, "wd")
  setwd(wd)

  zipfile <- paste0(tmp,"/", stringi::stri_rand_strings(1, 10, pattern = "[A-Za-z0-9]"), ".zip")
  while (file.exists(zipfile)) {
    zipfile <- paste0(tmp,"/", stringi::stri_rand_strings(1, 10, pattern = "[A-Za-z0-9]"), ".zip")
  }

  map_lim <- vector()
  for (lon in longitude) {
    map_lim <- c(map_lim, lon)
  }
  for (lat in latitude) {
    map_lim <- c(map_lim, lat)
  }
  depths <- vector()
  for (d in depth) {
    print(d)
    depths <- c(depths, d)
  }
  print(depths)
  depth_lines <- vector()
  for (dl in depthLines) {
    print(dl)
    depth_lines <- c(depth_lines, dl)
  }

  tadata <- rawToChar(f[[1]])
  text_con_ta <- textConnection(tadata)
  ta <- read.table(text_con_ta,sep=",",header=T)

  tbdata <- rawToChar(f[[2]])
  text_con_tb <- textConnection(tbdata)
  tb <- read.table(text_con_tb,sep=",",header=T)

  tcdata <- rawToChar(f[[3]])
  text_con_tc <- textConnection(tcdata)
  tc <- read.table(text_con_tc,sep=",",header=T)

  ta <- RoME::headers.conversion(ta,"TA")
  tb <- RoME::headers.conversion(tb,"TB")
  tc <- RoME::headers.conversion(tc,"TC")

  # dataframes defining the stratification for the given GSA and country/countries. The analysis of data from non-MEDITS countries requires dedicated stratiication dataframes
  strata=BioIndex::strata_scheme
  stratification_tab = BioIndex::stratification

  BioIndex(ta, tb, tc, species,rec_threshold=recThreshold, spaw_threshold=spawThreshold, haul_threshold=haulThreshold, sexes=sex, depth=depths, GSA=area, country=country, map_lim=map_lim,depth_lines=depth_lines, strata=strata, stratification_tab = stratification_tab, resolution=resolution, buffer=buffer, wd=wd, zip=TRUE, save=TRUE, verbose=TRUE)
  print('Completed Bioindex')

  zip::zip(zipfile, "wd", root = tmp)
  val <- readBin(zipfile, "raw", n=file.info(zipfile)$size)
  if (file.exists(wd)) {
    unlink(wd, recursive=T)
  }
  if (file.exists(zipfile)) {
    unlink(zipfile)
  }
  as_attachment(val, "bioindex-results.zip")
}


bioindexdocker/dockerfile
FROM rstudio/plumber:next
MAINTAINER Docker User <docker@user.org>

RUN mkdir ./private_pkgs
RUN mkdir ./app
COPY ./bioindex.R ./app/bioindex.R


RUN mkdir /home/data

RUN apt-get update && apt-get install pandoc libudunits2-dev tcl-dev tk-dev libgdal-dev libx11-dev libxt6 libgeos-dev libproj-dev libssl-dev libjemalloc-dev libglpk40 libcairo2-dev libxt-dev libgtk2.0-dev xvfb xauth xfonts-base libtiff5-dev -y
ENV LD_PRELOAD /usr/lib/x86_64-linux-gnu/libjemalloc.so
RUN R -e "install.packages('remotes')"
RUN R -e "remotes::install_git('https://github.com/COISPA/BioIndex')"
RUN R -e "remotes::install_git('https://github.com/COISPA/RoME')"
RUN R -e "install.packages('ggplot2')"
RUN R -e "install.packages('TMB')"
RUN R -e "install.packages('zip')"
RUN R -e "install.packages('tiff')"
RUN R -e "install.packages('devtools')"
RUN R -e "install.packages('stringr')"
RUN R -e "install.packages('timeDate')"
RUN R -e "install.packages('rnaturalearth')"
RUN R -e "install.packages('rnaturalearthdata')"
RUN R -e "install.packages('maps')"
RUN R -e "install.packages('mapproj')"
RUN R -e "install.packages('sp')"

CMD ["/app/bioindex.R"]

src/app/surveys/medits-components/bioindex/bioindex.component.html
<div class="row">
    <div>
        <div class="card card-body">
            <!--form [formGroup]="dataForm" class="row g-3 m-2"  (ngSubmit)="onSubmit()"-->
            <form #bioIndexForm="ngForm" novalidate >
                <div class="border mt-4 p-4">
                    <div class="row">
                        <!-- Country -->
                        <div class="col-md-2">
                            <label class="col-form-label-sm">Country:</label>
                            <select name="country" [(ngModel)]="country" class="form-select" required #countryField="ngModel" (change)="onCountryAreaChange()">
                                <option *ngFor="let country of countries" [value]="country.countryCd">{{ country.countryName }}</option>
                            </select>
                            <div *ngIf="countryField.invalid && (countryField.dirty || countryField.touched)"
                                 class="text-danger col-form-label-sm small">Country is required</div>
                        </div>
                        <!-- Area -->
                        <div class="col-md-2">
                            <label class="col-form-label-sm">Area:</label>
                            <select name="area" [(ngModel)]="area" class="form-select" required #areaField="ngModel" (change)="onCountryAreaChange()">
                                <option *ngFor="let area of areas" [value]="area">{{ area }}</option>
                            </select>
                            <div *ngIf="areaField.invalid && (areaField.dirty || areaField.touched)"
                                 class="text-danger col-form-label-sm">Area is required</div>
                        </div>
                        <!-- Species -->
                        <div class="col-md-2">
                            <label class="col-form-label-sm">Species:</label>
                            <select name="species" [(ngModel)]="selectedSpecies" class="form-select" required #speciesField="ngModel">
                                <option *ngFor="let spec of species" [value]="spec.meditsCd">{{ spec.scientificName }}</option>
                            </select>
                            <div *ngIf="speciesField.invalid && (speciesField.dirty || speciesField.touched)"
                                 class="text-danger col-form-label-sm">Species is required</div>
                        </div>
                        <div class="col-md-2">
                            <label class="col-form-label-sm">Start Year:</label>
                            <select name="startYear" [(ngModel)]="startYear" class="form-select" required #startYearField="ngModel">
                                <option *ngFor="let year of startEndYears" [value]="year">{{ year }}</option>
                            </select>
                            <div *ngIf="startYearField.invalid && (startYearField.dirty || startYearField.touched)"
                                 class="text-danger col-form-label-sm">Start Year is required</div>
                        </div>
                        <div class="col-md-2">
                            <label class="col-form-label-sm">End Year:</label>
                            <select name="endYear" [(ngModel)]="endYear" class="form-select" required #endYearField="ngModel">
                                <option *ngFor="let year of startEndYears" [value]="year">{{ year }}</option>
                            </select>
                            <div *ngIf="endYearField.invalid && (endYearField.dirty || endYearField.touched)"
                                 class="text-danger col-form-label-sm">End Year is required</div>
                        </div>
                        <!-- Depth lower -->
                        <div class="col-md-2">
                            <label class="col-form-label-sm">Depth Lower:</label>
                            <input type="text" appNumeric name="depthLower" [(ngModel)]="depthLower" required appLessThan="depthUpperField.control"  class="form-control" [min]="10" [max]="800" #depthLowerField="ngModel">
                            <div *ngIf="depthLowerField.invalid && (depthLowerField.dirty || depthLowerField.touched)"
                                 class="text-danger col-form-label-sm">Depth lower limit must be a number between 10 and 800 and less that the upper limit. </div>
                        </div>
                        <!-- Depth upper -->
                        <div class="col-md-2">
                            <label class="col-form-label-sm">Depth Upper:</label>
                            <input type="text" appNumeric name="depthUpper" [(ngModel)]="depthUpper" required  class="form-control" [min]="10" [max]="800" #depthUpperField="ngModel">
                            <div *ngIf="depthUpperField.invalid && (depthUpperField.dirty || depthUpperField.touched)"
                                 class="text-danger col-form-label-sm">Depth upper limit must be a number between 10 and 800</div>
                        </div>
                        <!-- Depth Lines -->
                        <div class="col-md-2">
                            <label class="col-form-label-sm">Depth Lines:</label>
                            <select name="depthLines" [(ngModel)]="depthLines" class="form-select" required multiple #depthLinesField="ngModel">
                                <option *ngFor="let depth of depthLinesSpectrum" [value]="depth">{{ depth }}</option>
                            </select>
                            <div *ngIf="depthLinesField.invalid && (depthLinesField.dirty || depthLinesField.touched)" class="text-danger col-form-label-sm">
                                Depth lines must be set
                            </div>
                        </div>
                        <!--Latitude Lower-->
                        <div class="col-md-2">
                            <label class="col-form-label-sm">Latitude lower limit:</label>
                            <input type="text" appNumeric name="latitudeLower" [(ngModel)]="latitudeLower" appLessThan="latitudeUpperField.control" required class="form-control" [min]="minLatitude" [max]="maxLatitude" #latitudeLowerField="ngModel" >
                            <div *ngIf="latitudeLowerField.invalid && (latitudeLowerField.dirty || latitudeLowerField.touched)"
                                 class="text-danger col-form-label-sm">Latitude lower limit must be a number within the limits of the area </div>
                        </div>
                        <!-- Latitude upper -->
                        <div class="col-md-2">
                            <label class="col-form-label-sm">Latitude lower limit:</label>
                            <input type="text" appNumeric name="latitudeUpper" [(ngModel)]="latitudeUpper" required  class="form-control" [min]="minLatitude" [max]="maxLatitude" #latitudeUpperField="ngModel" >
                            <div *ngIf="latitudeUpperField.invalid && (latitudeUpperField.dirty || latitudeUpperField.touched)"
                                 class="text-danger col-form-label-sm">Latitude upper limit must be a number within the limits of the area</div>
                        </div>
                        <!-- Longitude lower -->
                        <div class="col-md-2">
                            <label class="col-form-label-sm">Longitude lower limit:</label>
                            <input type="text" appNumeric name="longitudeLower" [(ngModel)]="longitudeLower" required appLessThan="longitudeUpperField.control"  class="form-control" [min]="minLongitude" [max]="maxLongitude" #longitudeLowerField="ngModel">
                            <div *ngIf="longitudeLowerField.invalid && (longitudeLowerField.dirty || longitudeLowerField.touched)"
                                 class="text-danger col-form-label-sm">Longitude lower limit must be a number within the limits of the area<</div>
                        </div>
                        <!-- Longitude upper -->
                        <div class="col-md-2">
                            <label class="col-form-label-sm">Longitude upper limit:</label>
                            <input type="text" appNumeric name="longitudeUpper" [(ngModel)]="longitudeUpper" required  class="form-control" [min]="minLongitude" [max]="maxLongitude" #longitudeUpperField="ngModel">
                            <div *ngIf="longitudeUpperField.invalid && (longitudeUpperField.dirty || longitudeUpperField.touched)"
                                 class="text-danger col-form-label-sm">Longitude upper limit must be a number within the limits of the area<</div>
                        </div>

                        <!-- Rec threshold -->
                        <div class="col-md-2">
                            <label class="col-form-label-sm">Rec threshold:</label>
                            <input type="text" appNumeric name="recThreshold" value="200" [(ngModel)]="recThreshold" required  class="form-control" #recThresholdField="ngModel">
                            <div *ngIf="recThresholdField.invalid && (recThresholdField.dirty || recThresholdField.touched)"
                                 class="text-danger col-form-label-sm">Rec threshold must be a number</div>
                        </div>

                        <!-- Spaw threshold -->
                        <div class="col-md-2">
                            <label class="col-form-label-sm">Spaw threshold:</label>
                            <input type="text" appNumeric name="spawThreshold" value="210" [(ngModel)]="spawThreshold" required  class="form-control" #spawThresholdField="ngModel">
                            <div *ngIf="spawThresholdField.invalid && (spawThresholdField.dirty || spawThresholdField.touched)"
                                 class="text-danger col-form-label-sm">Spaw threshold must be a number</div>
                        </div>

                        <!-- Haul threshold -->
                        <div class="col-md-2">
                            <label class="col-form-label-sm">Haul threshold:</label>
                            <input type="text" appNumeric name="haulThreshold" value="30" [(ngModel)]="haulThreshold" required  class="form-control" #haulThresholdField="ngModel">
                            <div *ngIf="haulThresholdField.invalid && (haulThresholdField.dirty || haulThresholdField.touched)"
                                 class="text-danger col-form-label-sm">Haul threshold must be a number</div>
                        </div>

                        <div class="col-md-2">
                            <label class="col-form-label-sm">&nbsp; </label>
                            <button type="button" class="btn btn-primary form-control" [disabled] = "runningBioIndex"  (click)="runBioindex()">Run BioIndex</button>
                        </div>
                    </div>
                </div>
            </form>
        </div>
    </div>
</div>
<div class="row">
    <div class="card card-body" [ngStyle]="{'color': divColor}">
         {{ message }}
    </div>
</div>

src/app/surveys/medits-components/bioindex/bioindex.component.ts
import {Component, OnInit, ViewChild} from '@angular/core';
import {AreasService} from "../../../shared-services/areas.service";
import {CountriesService} from "../../../shared-services/countries.service";
import {ParametricTablesStomachContentsService} from "../../../shared-services/parametric-tables-stomach-contents.service";
import {HttpParams, HttpResponse} from "@angular/common/http";
import {BehaviorSubject} from "rxjs";
import {NgForm} from "@angular/forms";
import {JwtHelperService} from "@auth0/angular-jwt";
import {FileSaverService} from "ngx-filesaver";
import {HttpService} from "../../../shared-services/HttpService";

@Component({
  selector: 'app-bioindex',
  templateUrl: './bioindex.component.html',
  styleUrls: ['./bioindex.component.scss']
})
export class BioindexComponent implements OnInit {

  country: string | null = null;
  area: number | null = null;
  countries: any [] = [];
  areas: any [] = [];
  species: any [] = [];
  selectedSpecies: string = "";
  startYear: number = 2020;
  endYear: number = 2020;
  latitudeLower: number = 34.00;
  latitudeUpper: number = 45.70;
  longitudeLower: number = -5.5;
  longitudeUpper: number = 35.00;
  depthLower: number = 10;
  depthUpper: number = 800;
  recThreshold: number = 200;
  spawThreshold: number = 210;
  haulThreshold: number = 30;
  buffer: number = 0.1;
  resolution: number = 1;
  depthLines: number[] = [200, 500, 800];
  depthLinesSpectrum: number[] = [ 50, 100, 200, 500, 800];

  countryUser: string = "";
  isAdmin: boolean = false;
  sex: string = "all";
  startEndYears: number[] = Array.from(new Array(45), (x, i) => i + 1991);
  message: string = "";
  runningBioIndex: boolean = false;
  divColor = 'green';

  minLatitude: number = 30.00;
  maxLatitude: number = 45.70;
  minLongitude: number = -5.60;
  maxLongitude: number = 35.00;

    csvData: any[] = [
        { area: 1, country: 'ESP', minLongitude: -5.599999905, maxLongitude: -1, minLatitude: 36, maxLatitude: 37.586222 },
        { area: 2, country: 'ESP', minLongitude: -3.333333254, maxLongitude: -2.666666746, minLatitude: 35.75, maxLatitude: 36.08333206 },
        { area: 5, country: 'ESP', minLongitude: 0.5, maxLongitude: 6, minLatitude: 38, maxLatitude: 40.5 },
        { area: 6, country: 'ESP', minLongitude: -1, maxLongitude: 6.000000003, minLatitude: 37, maxLatitude: 42.42947046 },
        { area: 7, country: 'FRA', minLongitude: 2.951167, maxLongitude: 8.000000008, minLatitude: 41.33000184, maxLatitude: 43.784583 },
        { area: 8, country: 'FRA', minLongitude: 7.6277549, maxLongitude: 9.750000009, minLatitude: 41.29999923, maxLatitude: 43.25 },
        { area: 9, country: 'ITA', minLongitude: 7.525000087, maxLongitude: 13.00458127, minLatitude: 41.29999923, maxLatitude: 44.427111 },
        { area: 10, country: 'ITA', minLongitude: 11.00000008, maxLongitude: 16.22125, minLatitude: 37.034583, maxLatitude: 41.29999946 },
        { area: 11, country: 'ITA', minLongitude: 6.000000003, maxLongitude: 11, minLatitude: 37.99999999, maxLatitude: 41.78454638 },
        { area: 15, country: 'MLT', minLongitude: 13.5, maxLongitude: 15.30000019, minLatitude: 35, maxLatitude: 36.5 },
        { area: 16, country: 'ITA', minLongitude: 10.99999997, maxLongitude: 15.30000019, minLatitude: 35, maxLatitude: 37.99999999 },
        { area: 17, country: 'ITA', minLongitude: 12.141194, maxLongitude: 18.765417, minLatitude: 41.92224482, maxLatitude: 45.794556 },
        { area: 17, country: 'SVN', minLongitude: 13.34, maxLongitude: 13.82, minLatitude: 45.42, maxLatitude: 45.72 },
        { area: 17, country: 'HRV', minLongitude: 13, maxLongitude: 18.8, minLatitude: 42, maxLatitude: 45.67 },
        { area: 18, country: 'ITA', minLongitude: 15.16979797, maxLongitude: 20.186167, minLatitude: 39.60821022, maxLatitude: 42.47187939 },
        { area: 19, country: 'ITA', minLongitude: 15.082944, maxLongitude: 19.16666603, minLatitude: 35, maxLatitude: 40.520417 },
        { area: 20, country: 'GRC', minLongitude: 19.16666603, maxLongitude: 23.225389, minLatitude: 35, maxLatitude: 39.88214874 },
        { area: 22, country: 'GRC', minLongitude: 22.527028, maxLongitude: 29, minLatitude: 34, maxLatitude: 41.012056 },
        { area: 23, country: 'GRC', minLongitude: 23, maxLongitude: 26.5, minLatitude: 34, maxLatitude: 36 },
        { area: 25, country: 'CYP', minLongitude: 32, maxLongitude: 35, minLatitude: 34, maxLatitude: 35.78333282 },
        { area: 29, country: 'BGR', minLongitude: 27.1, maxLongitude: 29.2, minLatitude: 41.7, maxLatitude: 43.9 },
        { area: 29, country: 'ROU', minLongitude: 28, maxLongitude: 30.6, minLatitude: 43.53, maxLatitude: 45.4 }
    ];

  @ViewChild('bioIndexForm', { static: false }) bioIndexForm: NgForm;

  constructor(private areasService: AreasService, private countriesService: CountriesService, protected fileSaverService: FileSaverService,
              private parametricService:ParametricTablesStomachContentsService, private jwtHelper: JwtHelperService, protected service: HttpService) {
      this.filterStartEndYears();
  }

  validateFormFields() {
      // Mark all form fields as touched
      Object.keys(this.bioIndexForm.controls).forEach(field => {
          const control = this.bioIndexForm.form.get(field);
          control.markAsTouched({onlySelf: true});
      });
  }
  ngOnInit(): void {
      this.userAccessControl();
      this.getCountries()
          .then(() => {
              if (this.isAdmin == true) {
                  return this.getAreas('');
              } else {
                  this.countries = this.countries.filter(country => country.countryCd === this.countryUser);
                  return this.getAreas("?country=" + this.countryUser);
              }
          }).then(() => {
              this.getSpecies();
      });
  }

  onCountryAreaChange() {
      this.filterStartEndYears();
      const selectedArea = this.area;
      const selectedCountry = this.country;

      const matchingRecords = this.csvData.filter(record => record.area == selectedArea && record.country == selectedCountry);

      if (matchingRecords.length > 0) {
          const record = matchingRecords[0];
          this.minLatitude = record.minLatitude;
          this.maxLatitude = record.maxLatitude;
          this.minLongitude = record.minLongitude;
          this.maxLongitude = record.maxLongitude;
          this.longitudeLower = this.minLongitude;
          this.longitudeUpper = this.maxLongitude;
          this.latitudeLower = this.minLatitude;
          this.latitudeUpper = this.maxLatitude
      }
  }

    filterStartEndYears() {
        if (this.country && this.area) {
            this.service.getRangeOfYearsForBioindex(this.country, this.area).subscribe(years => {
                    this.startEndYears = years;
                }
            );
        } else {
            // If no country or area is selected, reset to the full range
            this.startEndYears = Array.from(new Array(45), (x, i) => i + 1991);
        }
    }

    userAccessControl () {
        const token = sessionStorage.getItem('token');
        if (token) {
            const decodedToken = this.jwtHelper.decodeToken(token);
            const authorities: String [] = decodedToken.AUTHORITIES.split(",");
            this.countryUser = authorities.find(element => element.startsWith("COUNTRY")).split("_")[1];
            const bool = authorities.find(element => element.startsWith("ADMIN")).split("_").pop();
            if (bool === "TRUE") {
                this.isAdmin = true;
            } else {
                this.isAdmin = false;
            }
        }
    }

  getAreas(urlParam: string)
  {
    this.areasService.getRecordsFromAPI(urlParam)
        .subscribe(records => {
          this.areas = records.filter(
              (item, i, arr) => arr.findIndex(t => t.gsa === item.gsa) === i
          );
          this.areas.sort((a,b) => (a.gsa).localeCompare(b.gsa,undefined, { numeric: true }));
          this.areas = this.areas.map(area => area.gsa);
        });
  }

  getCountries(): Promise<void> {
    return new Promise<void>((resolve, reject) => {
      this.countriesService.getRecordsFromAPI()
          .subscribe(records => {
                this.countries = records;
                this.countries.sort((a, b) => a.countryCd.localeCompare(b.countryCd));
                resolve();
              },
              error => reject(error)
          );
    });
  }
  getSpecies(): Promise<void> {
    return new Promise<void>((resolve, reject) => {
        this.parametricService.getRecordsFromAPI("specieslistmedits")
            .subscribe(records => {
                    this.species = records['content'];
                    resolve();
                },
                error => reject(error)
            );
    });
  }

  runBioindex() {
    this.runningBioIndex = true;
      this.divColor = 'green';
    this.message = "Running BioIndex analysis. Please wait."
    this.validateFormFields();
    if (this.bioIndexForm.valid) {
        this.service.runBioIndex(this.country, this.startYear, this.endYear, this.selectedSpecies, this.area, this.latitudeLower, this.latitudeUpper, this.longitudeLower, this.longitudeUpper,
            this.depthLower, this.depthUpper, this.recThreshold, this.spawThreshold, this.haulThreshold, this.buffer, this.resolution, this.depthLines, this.sex).subscribe (
            (event: any) => {
                if (event instanceof HttpResponse) {
                    if (event.status == 200) {
                        const fileName: string = "BioIndex_Report.zip";
                        if (event.body != null)
                            this.fileSaverService.save(event.body, fileName);
                        this.message = "BioIndex report generated successfully.";
                    }
                    else if (event.status == 204) {
                        this.message = "No data exist for selected parameters";
                    }
                    this.runningBioIndex = false;

                }
            },
            (err: any) => {
                console.log(err);
                this.divColor = 'red';
                this.message = "Error while running R code.";
                this.runningBioIndex = false;
            }
        );
    }
    else {
        this.message = "The form is not valid."
        this.runningBioIndex = false;
    }
  }

    protected readonly Number = Number;
}

