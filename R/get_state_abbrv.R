get_state_abbrv <- function(State = state_name){
  
  if (!inherits(State, "character")) {
    stop("get_state_abbrv() State should be a character vector")
  }

  # Convert all to lowercase to avoid mismatch due 
  # to capitalization
  state.name <- tolower(state.name)
  State <- tolower(State)

  # Find which index the State matches to 
  # state.name are USA state names built into R
  index <- which(state.name %in% State)
  if (!inherits(index, "integer")) {
    stop("expected integer index for state match")
  }

  # Use the index to select the state abbreviation
  # state.abb are USA state abbreviations built into R
  selected_abbrv <- state.abb[index]
  
  # Check that return value is character
  if (!inherits(selected_abbrv, "character")) {
    stop("get_state_abbrv() should return character")
  }
  
  return(selected_abbrv)
}
