grab_ucla_state <- function(State = state_name){
  url <-paste0("https://gist.githubusercontent.com/knowzou/ecacd65ab863979a9aea0f19a75252c3/raw/us_rt.json")
  
  if (as.character(url_file_exists(url)[1]) == "TRUE" ) {
    
    ucla <- jsonlite::fromJSON(url)
    place_holder <- paste0("US-",get_state_abbrv(State))
    
    if(is.null(ucla$Rt[place_holder]) == TRUE){
      
      errorCondition("UCLA does not provide estimates for your State")
      
    } else { 
      ucla_state <- rbindlist(ucla$Rt[place_holder])
      ucla_state <- ucla_state %>% gather(date, Rt, 1:137)
      ucla_state$date <- as.Date(ucla_state$date, format = "%m/%d/%y")
      msg <- paste0("Successfully downloaded Rt data from UCLA for ", State, " on ", Sys.Date())
     
      
    }
    rm(ucla)
    
    
  } else {
    msg <- paste0("Problem with MIT link to file updates. Check URL.")
  }
  
  print(msg)
  return(ucla_state)
  
  
}
