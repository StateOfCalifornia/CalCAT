grab_can_county_weak_intervention <- function(State = state_name){
  
  list_of_fips_char = make_fips_list()
  
  can.ca.co  <- rbindlist(lapply(list_of_fips_char, function(x) loop_can_cnty_weak(x))) %>% 
    dplyr::mutate(date = as.Date(date))
  return(can.ca.co)
  
}