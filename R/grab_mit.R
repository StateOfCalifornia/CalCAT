
grab_mit <- function(State = state_name){
  url <-paste0("https://raw.githubusercontent.com/COVIDAnalytics/website/master/data/predicted/Global.csv")
  
  if (as.character(url_file_exists(url)[1]) == "TRUE" ) {
    
    mit <- read.csv(url, stringsAsFactors = FALSE) %>% filter(Province == State)
    mit$date <- as.Date(mit$Day)

    
    msg <- paste0("Successfully download data from MIT for ", State, " on ", Sys.Date())
    
  } else {
    msg <- paste0("Problem with MIT link to file updates. Check URL.")
  }
  
  print(msg)
  return(mit)
  
}
