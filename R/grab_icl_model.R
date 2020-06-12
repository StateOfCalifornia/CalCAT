
grab_icl_model <- function(State = state_name){
  
  url <-paste0("https://mrc-ide.github.io/covid19usa/downloads/data-model-estimates.csv")
  
  if (as.character(url_file_exists(url)[1]) == "TRUE" ) {
    
    icl_model <- read.csv(url, stringsAsFactors = FALSE) %>% filter(state == get_state_abbrv(State))
    icl_model$date <- as.Date(icl_model$date)
    
    msg <- paste0("Successfully download data from Imperial College London for ", State, " on ", Sys.Date())
    
  } else {
    msg <- paste0("Problem with ICL link to file updates. Check URL.")
  }
  
  print(msg)
  return(icl_model)
}