transform_metadata_to_df <- function(data){
  df <- data[[1]] %>% 
  map(as_tibble) %>%  
  list_rbind() %>%  
  mutate(latestData = map_chr(latestData, 1, .default = NA_character_)) %>%  
  mutate(latestData = as_datetime(latestData, tz = "UTC"))  %>%  
  unnest_wider(location) %>%  
  unnest_wider(latLon)
}



to_iso8601 <- function(time_var, days){
  date_time <- iso8601(time_var + days(days))
  date_time <- paste0(date_time, "Z")
  return(date_time)
}
  
  

transform_volumes <- function(json){
  df <- fromJSON(json)
  return(df)
}
