
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param State PARAM_DESCRIPTION, Default: state_name
#' @param ST PARAM_DESCRIPTION, Default: state_abbrv
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname grab_rt_live
#' @export 
grab_rt_live <- function(State = state_name, ST = state_abbrv){

  url <- "https://d14wlfuexuxgcm.cloudfront.net/covid/rt.csv"
 
  if ( as.character(url_file_exists(url)[1]) == "TRUE" ) {
    rt_live <- read.csv("https://d14wlfuexuxgcm.cloudfront.net/covid/rt.csv") %>% 
      filter(region == ST) %>% 
      mutate(date = as.Date(as.character(date)),
             region = as.character(region))
    
    msg <- paste0("Successfully download data from Rt.live for ", State, " on ", Sys.Date())
    
  } else {
    msg <- paste0("Problem with Rt.live link to file updates. Check URL.")
  }
  
  print(msg)
  return(rt_live)
}
