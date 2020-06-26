grab_epi_forecasts <- function(State = state_name){

  url <-"https://raw.githubusercontent.com/epiforecasts/covid-regional/master/united-states/regional-summary/rt.csv"

  
  if ( as.character(url_file_exists(url)[1]) == "TRUE" ) {
    epi_forecast <- read.csv(url, stringsAsFactors = FALSE) %>% filter(region == State) %>% mutate(date = as.Date(date))
    
    
    
    msg <- paste0("Successfully download data from Epi Forecasts for ", State, " on ", Sys.Date())
    
  } else {
    msg <- paste0("Problem with Epi Forecasts link to file updates. Check URL.")
  }
  
  print(msg)
  
  return(epi_forecast)
}
