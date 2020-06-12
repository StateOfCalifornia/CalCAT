get_county_populations <- function(State = state_name){

  goo <- get_counties(State) %>% .[, .(county, fips = as.integer(str_sub(fips, -3)))]
  
  foo <- fread("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv") %>%
    .[STNAME == State, .(fips = ifelse(COUNTY<10,paste0(get_state_fips(State = State, type = "character"),"00",COUNTY),
                                       ifelse(COUNTY<100,paste0(get_state_fips(State = State, type = "character"),"0",COUNTY),COUNTY)), pop2020 = as.double(POPESTIMATE2019))] %>% 
    .[fips == 0, county := State] %>% select(fips, pop2020) %>% tibble()
  
  return(foo)  
  
}
