#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION

#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[httr]{GET}},\code{\link[httr]{stop_for_status}}
#'  \code{\link[data.table]{rbindlist}},\code{\link[data.table]{dcast.data.table}}
#' @rdname grab_reich_lab_deaths
#' @export 
#' @importFrom httr GET stop_for_status
#' @importFrom data.table rbindlist dcast.data.table
grab_reich_lab_deaths <- function(){

    library(httr)
    req <- httr::GET("https://api.github.com/repos/reichlab/covid19-forecast-hub/git/trees/master?recursive=1")
    httr::stop_for_status(req)
    filelist<- unlist(lapply(content(req)$tree, "[", "path"), use.names = F)
    filelist <- grep("data-processed/", filelist, value = TRUE, fixed = TRUE)
    filelist <- grep(".csv", filelist, value = TRUE, fixed = TRUE)
    
    
    filelist <- lapply(filelist, function(x) paste0("https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/",x))
  
    
    foo <- data.table::rbindlist(lapply(filelist, read_reich))   # combine all the reichlab death csvs
    
    foo <- foo %>%
      data.table::dcast.data.table(target_end_date + target  + model_team + forecast_date ~ measure,value.var = "value") %>%
      .[, maxdate :=max(forecast_date), by=.(model_team)] %>%
      .[forecast_date == maxdate & as.Date(target_end_date) <= Sys.Date() + 30, 
        .(model_team, pointNA, target_end_date)] 
    
    
    return(foo)
}


