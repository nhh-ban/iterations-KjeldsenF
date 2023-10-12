# This file contains tests to be applied to 
# the Vegvesen stations-data *after* being transformed
# to a data frame. 
# 
# All tests are packed in a function test_stations_metadata that apples
# all the aforementioned tests


# define a function with df as agrument
test_stations_metadata_colnames <- 
  function(df) {
    
    # Create a vector 'expected_colnames' containing the expected column names.
    expected_colnames <- c("id", "name", "latestData", "lat", "lon") 
    
    # If the column names match, print a "PASS" message indicating that the data has the correct columns.
    if (all(colnames(df) == expected_colnames) == TRUE) {
      print("PASS: Data has the correct columns")
      
      # If the column names do not match, print a "FAIL" message indicating that the columns do not match the correct specification
    } else{
      print("FAIL: Columns do not match the correct specification")
      
    }
  }

test_stations_metadata_nrows <-
  function(df) {
    
    min_expected_rows <- 5000 #minimum expected rows
    max_expected_rows <- 10000 # maximum expected rows
    
    # "PASS" if the n of rows are between 5 000 and 10 000
    if (nrow(df) > min_expected_rows & nrow(df) < max_expected_rows) {
      print("PASS: Data has a reasonable number of rows") 
    } 
    # "FAIL" if n of rows are less than 5 000
    else if (nrow(df) <= min_expected_rows) {
      print("FAIL: Data has suspiciously few rows") 
    } 
    # "Fail" if n of rows are more than 10 000
    else { 
      print("FAIL: Data has suspiciously many rows") 
    }
  }

test_stations_metadata_coltypes <-
  function(df) {
   
     #Creating a vector, expected coltypes
    expected_coltypes <-
      c("character", "character", "double", "double", "double") 
    
    # Check if the data types of each column in the data frame 'df' match the expected data types.
    if (all(df %>%
            map_chr( ~ typeof(.)) == expected_coltypes) == TRUE) {
      
      # Prints "PASS" if all columns have the correct specifications.
      print("PASS: All cols have the correct specifications") 
      
    } else{
      
      # If the data types do not match, print a "FAIL" message indicating that the columns do not have the correct specification.
      print("FAIL: Columns do not have the correct specification")   
      
    }
  }
  

# Define the function from the df argument
test_stations_metadata_nmissing <-
  function(df) {
    
    # Set the max number of missing values = 200
    max_miss_vals <- 200
    
    
    # Check if the sum of of missing values in each column of the data frame 'df' is less than the maximum allowed value.
    if (df %>% map_int( ~ sum(is.na((.)))) %>% sum(.) < max_miss_vals) {
      
      # If the number of missing values is less than 200, print a "PASS" message.
      print("PASS: Amount of missing values is reasonable")
    } else {
      
      # If there are too many missing values, print a "FAIL" message.
      print("FAIL: Too many missing values in data set")
    }
  }

# Define the function from the df argument

test_stations_metadata_latestdata_timezone <-
  function(df) {
    
    # Check if the timezone in the dataframe df = UTC
    if (attr(df$latestData,"tzone")=="UTC") {
      
      # If yes, print: 
      print("PASS: latestData has UTC-time zone")
    } else {
      
      # If not, print:
      print("FAIL: latestData does not have expected UTC-time zone")
    }
  }


# Define a comprehensive testing function called 'test_stations_metadata' 
test_stations_metadata <- 
  function(df){
    test_stations_metadata_colnames(df)
    test_stations_metadata_coltypes(df)
    test_stations_metadata_nmissing(df)
    test_stations_metadata_nrows(df)
    test_stations_metadata_latestdata_timezone(df)
  }

# It checks column names, data types, 
# missing values, the number of rows, and the time zone of 
# the 'latestData' column.



