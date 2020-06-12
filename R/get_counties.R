get_counties <- function(State = state_name){
  
  counties <- data.table::fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>% 
    .[state == State, .(fips = unique(fips)), by = .(county, state)] %>% 
    na.omit() %>%
    .[, .(county, state, fips, fips_char = ifelse(fips < 9999, paste0("0",fips),as.character(fips)))] %>%
    merge(data.table::fread("https://raw.githubusercontent.com/CDPHrusers/List-of-US-States/master/states.csv"), by.x = "state", by.y = "State")
  
  return(counties)
  
}
