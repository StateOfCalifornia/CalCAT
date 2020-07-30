
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
#' @rdname get_rand_state_number
#' @export 
get_rand_state_number <- function(State = state_name){
  states <- data.frame(state = c("Alabama",
                                 "Alaska",
                                 "Arizona",
                                 "Arkansas",
                                 "California",
                                 "Colorado",
                                 "Connecticut",
                                 "Delaware",
                                 "District of Columbia",
                                 "Florida",
                                 "Georgia",
                                 "Hawaii",
                                 "Idaho",
                                 "Illinois",
                                 "Indiana",
                                 "Iowa",
                                 "Kansas",
                                 "Kentucky",
                                 "Louisiana",
                                 "Maine",
                                 "Maryland",
                                 "Massachusetts",
                                 "Michigan",
                                 "Minnesota",
                                 "Mississippi",
                                 "Missouri",
                                 "Montana",
                                 "Nebraska",
                                 "Nevada",
                                 "New Hampshire", 
                                 "New Jersey",
                                 "New Mexico",
                                 "New York",
                                 "North Carolina",
                                 "North Dakota",
                                 "Ohio",
                                 "Oklahoma", 
                                 "Oregon",
                                 "Pennsylvania",
                                 "Rhode Island",
                                 "South Carolina",
                                 "South Dakota",
                                 "Tennessee",
                                 "Texas",
                                 "Utah",
                                 "Vermont",
                                 "Virginia",
                                 "Washington", 
                                 "West Virginia",
                                 "Wisconsin",
                                 "Wyoming"), rand_num = 1:51)
  
  return(as.integer(states$rand_num[which(states$state == State)]))
  
}
