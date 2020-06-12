loop_can_cnty_strong <- function(fips_code) {
  
  url <- paste0("https://data.covidactnow.org/latest/us/counties/",fips_code,".STRONG_INTERVENTION.timeseries.json")
  Sys.sleep(2)
  valid <- RCurl::url.exists(url)
  print(paste0(fips_code,": ",valid))
  if(valid){
    cnty <- jsonlite::fromJSON(url)$timeseries %>% as.data.frame() %>% mutate(date = as.Date(date))
    cnty <- dplyr::mutate(cnty, fips_char = fips_code, fips = as.numeric(fips_code))
    return(cnty)
  }
  
}