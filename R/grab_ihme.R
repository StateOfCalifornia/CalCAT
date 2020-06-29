
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param State PARAM_DESCRIPTION, Default: state_name
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname grab_ihme
#' @export 
grab_ihme <- function(State = state_name){ 
  
  url <- "https://ihmecovid19storage.blob.core.windows.net/latest/ihme-covid19.zip"
    
    if ( as.character(url_file_exists(url)[1]) == "TRUE" ) {
      
      temp <- tempfile()
      temp2 <- tempfile()
      download.file(url,temp)
      unzip(zipfile = temp, exdir = temp2)
      folder <- list.files(temp2)
      IHME <- fread(file.path(temp2, paste0(folder,"/Hospitalization_all_locs.csv") ))
      unlink(c(temp, temp2))
      msg <- paste0("File was updated; new projections for ",folder,". Current files:")
      
    } else {
      msg <- paste0("Problem with link to file updates. Check URL.")
    }
    
    IHME <- IHME[which(IHME$location_name == State),]
    IHME <- rename(IHME, county = location_name) %>% select(-1)
    IHME$date <- as.Date(IHME$date)
    IHME[,3:23] <-round(IHME[,3:23],1)
    
    print(msg)
    
    return(IHME)
    
}
