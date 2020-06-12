grab_can_county_observed_intervention <- function(State = state_name){
  
  list_of_fips_char = make_fips_list()
  
  co <- counties[,.(county, fips)]
  
  can.ca.co  <- rbindlist(lapply(list_of_fips_char, function(x) loop_can_cnty_observed(x))) %>% 
    mutate(date = as.Date(date)) %>% merge(co)
   
    
  return(can.ca.co)
  
}
