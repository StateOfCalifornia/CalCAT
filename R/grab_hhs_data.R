### COVID-19 Actuals ###

#' Title grab_hhs_data
#'
#' @param state Two character state abbreviation in character format.
#'
#' @return Data frame with date, weekly hospital admissions (weekly_admits), 
#' and daily hospital census (census) columns.
#' @export
#'
#' @examples grab_hhs_data("CA")
#' 
grab_hhs_data <- function(state) {
  
HHS_actuals_state<- read.socrata(paste0("https://healthdata.gov/resource/g62h-syeh.csv?state=", state, ""), stringsAsFactors = FALSE) %>%
  mutate(date=as.Date(date)) %>%
  select(date, contains("covid")) %>%
  arrange(date) %>%
  mutate(total_census = total_adult_patients_hospitalized_confirmed_covid + total_pediatric_patients_hospitalized_confirmed_covid,
         census = zoo::rollmean(total_census, k=7, align="right", fill=NA),
         admits = previous_day_admission_adult_covid_confirmed + previous_day_admission_pediatric_covid_confirmed,
         weekly_admits = zoo::rollsum(admits, k=7, align="right", fill=NA))  %>%
  mutate(date= as.Date(date)) %>%
  select(date, admits, weekly_admits, census) %>%
  filter(date >= Sys.Date()-90) #to limit the uncertainty of our time series approaches we are going to filter the data set to the last 3 months of data; feel free to adjust length of input time series
return(HHS_actuals_state)
print(paste0("Data last updated", max(HHS_actuals_state$date)))

}
