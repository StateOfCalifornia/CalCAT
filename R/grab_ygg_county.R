grab_ygg_county <- function(State = state_name){
  
  
  url <-paste0("https://github.com/youyanggu/covid19_projections/raw/master/projections/combined/latest_subregion.csv")
  
  if (as.character(url_file_exists(url)[1]) == "TRUE" ) {
    
    yu.cnty <- read.csv(url, stringsAsFactors = FALSE)
    yu.cnty <- yu.cnty %>% filter(region == get_state_abbrv(State))
    yu.cnty$date <- as.Date(yu.cnty$date, format ="%Y-%m-%d")
    
    msg <- paste0("Successfully download data from Yugang Gu for ", State, " on ", Sys.Date())
    
  } else {
    msg <- paste0("Problem with Yugang Gu link to file updates. Check URL.")
  }
  
  print(msg)
  return(yu.cnty)
  
}