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
#' @seealso 
#'  \code{\link[jsonlite]{toJSON, fromJSON}}
#'  \code{\link[dplyr]{mutate}}
#' @rdname grab_can_state_strong_intervention
#' @export 
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr mutate
grab_can_state_strong_intervention <- function(State = state_name){
  
  can.ca <- jsonlite::fromJSON(paste0("https://data.covidactnow.org/latest/us/states/",get_state_abbrv(State),".STRONG_INTERVENTION.timeseries.json"))$timeseries %>% 
    as.data.frame() %>% 
    dplyr::mutate(date = as.Date(date),
           fips_char = paste0(get_state_fips(State, type = "character"),"000"),
           fips = as.integer(get_state_fips(State, type = "integer")*1000))
  return(can.ca)  
  
}
