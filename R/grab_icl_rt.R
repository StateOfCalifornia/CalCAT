grab_icl_rt <- function(ST = state_abbrv, State = state_name){

  url <-paste0("https://mrc-ide.github.io/covid19usa/downloads/time-varying-reproduction-number-scenarios.csv")
  
  if (as.character(url_file_exists(url)[1]) == "TRUE" ) {
    
    icl_rt <- read.csv(url, stringsAsFactors = FALSE) %>% filter(state == ST)
    icl_rt$date <- as.Date(icl_rt$date)
    fwrite(icl_rt, paste0(data_path,"icl_rt.csv"))
    
    msg <- paste0("Successfully download data from Imperial College London for ", state_name, " on ", Sys.Date())
    
  } else {
    msg <- paste0("Problem with ICL link to file updates. Check URL.")
  }
  
  print(msg)
  return(icl_rt)
  
}
  