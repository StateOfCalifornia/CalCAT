grab_ygg_state  <- function(State = state_name){   
  url <-paste0("https://github.com/youyanggu/covid19_projections/raw/master/projections/combined/latest_us.csv")
  
  if (as.character(url_file_exists(url)[1]) == "TRUE" ) {
    
    yu <- read.csv(url, stringsAsFactors = FALSE) %>% 
      filter(region == get_state_abbrv(State))   
    yu$date <- as.Date(yu$date, format ="%Y-%m-%d")
   
    
    msg <- paste0("Successfully download data from Yugang Gu for ", State, " on ", Sys.Date())
    
  } else {
    msg <- paste0("Problem with Yugang Gu link to file updates. Check URL.")
  }
  
  print(msg)
  return(yu)
}
  



