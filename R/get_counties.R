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
#'  \code{\link[data.table]{fread}}
#' @rdname get_counties
#' @export 
#' @importFrom data.table fread
get_counties <- function(State = state_name){
  
  counties <- data.table::fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>% 
    .[state == State, .(fips = unique(fips)), by = .(county, state)] %>% 
    na.omit() %>%
    .[, .(county, state, fips, fips_char = ifelse(fips < 9999, paste0("0",fips),as.character(fips)))] %>%
    merge(data.table::fread("https://raw.githubusercontent.com/CDPHrusers/List-of-US-States/master/states.csv"), by.x = "state", by.y = "State")
  
  return(counties)
  
}
