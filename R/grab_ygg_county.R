grab_ygg_county <- function(State = state_name){
  
  
  url <-paste0("https://github.com/youyanggu/covid19_projections/raw/master/projections/combined/latest_subregion.csv")
  
  if (as.character(url_file_exists(url)[1]) == "TRUE" ) {
    
    gu.cnty <- read.csv(url, stringsAsFactors = FALSE)
    gu.cnty <- gu.cnty %>% filter(region == get_state_abbrv(State))
    gu.cnty$date <- as.Date(gu.cnty$date, format ="%Y-%m-%d")
    
    msg <- paste0("Successfully download data from Youyang Gu for ", State, " on ", Sys.Date())
    
  } else {
    msg <- paste0("Problem with Youyang Gu link to file updates. Check URL.")
  }
  
  print(msg)
  return(gu.cnty)
  
}