library(dplyr)
library(purrr)
library(tidyr)
library(lubridate)

transform_metadata_to_df <- function(stations_metadata) {
  df <- stations_metadata[[1]] %>%
    map(as_tibble) %>%
    list_rbind() %>%
    mutate(latestData = map_chr(latestData, 1, .default=NA_character_)) %>% 
    mutate(latestData = as_datetime(latestData, tz = "UTC")) %>% 
    unnest_wider(location) %>% 
    unnest_wider(latLon)
  
  return(df)
}



to_iso8601 <- function(datetime_var, offset_days) {
  # Ensure the datetime_var is in POSIXct format
  datetime_var <- as_datetime(datetime_var)
  
  # Add the offset
  new_datetime <- datetime_var + days(offset_days)
  
  # Convert to ISO8601 format with a "Z" appended
  iso_datetime <- format(new_datetime, format = "%Y-%m-%dT%H:%M:%SZ")
  
  return(iso_datetime)
}

to_iso8601(as_datetime("2016-09-01 10:11:12"),0)
to_iso8601(as_datetime("2016-09-01 10:11:12"),-4)



# Task 5

transform_volumes <- function(api_response) {
  # Navigate to the 'edges' section of the response
  edges <- api_response$trafficData$volume$byHour$edges
  
  # Extract necessary information from each node
  df <- purrr::map_dfr(edges, function(edge) {
    tibble(
      from = as_datetime(edge$node$from),
      to = as_datetime(edge$node$to),
      volume = edge$node$total$volumeNumbers$volume
    )
  })
  
  return(df)
}







