loop_can_cnty_observed <- function(fips_code) {

  url <- paste0("https://data.covidactnow.org/latest/us/counties/",fips_code,".OBSERVED_INTERVENTION.timeseries.json")
  Sys.sleep(2)
  valid <- as.character(url_file_exists(url)[1])
  print(paste0(fips_code,": ",valid))
  if(valid){
    cnty <- jsonlite::fromJSON(url)$timeseries %>% as.data.frame()
    if ("date" %in% names(cnty))
      cnty <- cnty %>% mutate(date = as.Date(date))
    cnty <- mutate(cnty, fips_char = fips_code, fips = as.numeric(fips_code))
    return(cnty)
  }

}
