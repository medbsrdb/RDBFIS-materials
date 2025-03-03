# Load the CSV files
tfpi <- read.csv("../tools4MCDA/outcomes1/FPIq.csv")
tsp <- read.csv("../tools4MCDA/outcomes1/sample_FPIq_sp.csv")

# Extract unique values from the 'year' column in each table
unique_values_tfpi <- unique(paste0(tfpi$year, tfpi$quarter, tfpi$gear_type, tfpi$vessel_length))
unique_values_tsp <- unique(paste0(tsp$year, tsp$quarter, tsp$gear_type, tsp$vessel_length))

#year	quarter	FPI	gear_type	vessel_length

# Check if the unique values are identical
if (identical(sort(unique_values_tfpi), sort(unique_values_tsp))) {
  print("The unique values of 'year, quarter, gear_type, vessel_length' in the tables fpi and species are identical")
} else {
  print("The unique values of 'year, quarter, gear_type, vessel_length' in the tables fpi and species are not identical")
  
  # Display the differences
  print("Values in fpi but not in tsp:")
  print(setdiff(unique_values_tfpi, unique_values_tsp))
  
  print("Values in sp but not in fpi:")
  print(setdiff(unique_values_tfpi, unique_values_tsp))
  # return exit status code (-1)
  quit(status = -1)
}
