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
#'  \code{\link[dplyr]{mutate}}
#' @rdname grab_can_county_strong_intervention
#' @export 
#' @importFrom dplyr mutate
grab_can_county_strong_intervention <- function(State = state_name){
  
  list_of_fips_char <- make_fips_list()
  
  can.ca.co  <- rbindlist(lapply(list_of_fips_char, function(x) loop_can_cnty_strong(x)))%>% 
    dplyr::mutate(date = as.Date(date))
  return(can.ca.co)
  
}
