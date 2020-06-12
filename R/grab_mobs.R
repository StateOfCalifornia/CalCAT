
grab_mobs <- function(State = state_name){
  url <- paste0("https://data-tracking-api-dot-mobs-2019-ncov-web.appspot.com/data?state=",State,"&frequency=daily")
  
  
    tryCatch({
      mobs <- jsonlite::fromJSON(url)
      mobs <- unique(mobs)
      msg <- paste0("Successfully download data from MOBS for ", State, " on ", Sys.Date())
      print(msg)
      return(mobs)
      
      }, error=function(e){ 
        
        mobs <- jsonlite::fromJSON(paste0("saved_old_versions/mobs_",State,"_data.json"))
        mobs <- unique(mobs)
        msg <- paste0("Problem with MOBS link to file update. Grabbing old copy (June 6, 2020).")
     
     print(msg)
     return(mobs)
      })
    
  

}

