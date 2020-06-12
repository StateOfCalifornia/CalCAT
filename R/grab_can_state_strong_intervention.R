grab_can_state_strong_intervention <- function(State = state_name){
  
  can.ca <- jsonlite::fromJSON(paste0("https://data.covidactnow.org/latest/us/states/",get_state_abbrv(State),".STRONG_INTERVENTION.timeseries.json"))$timeseries %>% 
    as.data.frame() %>% 
    dplyr::mutate(date = as.Date(date),
           fips_char = paste0(get_state_fips(State, type = "character"),"000"),
           fips = as.integer(get_state_fips(State, type = "integer")*1000))
  return(can.ca)  
  
}