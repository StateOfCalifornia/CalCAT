
# Generate COVID-19 Forecasts ---------------------------------------------

#Generate ARIMA data frame
#' Title generate_admits_arima
#'
#' @param covid_admits_raw A data frame containing columns corresponding to date, state, and variables of interest for forecasting.
#' @param state A character string of the state of interest, fully spelled out (e.g., "California").
#'
#' @return A data frame with forecasts and uncertainty intervals of forecasted variables for each state.
#' @export
#'
#' @examples generate_admits_arima(df, "California")
# covid_admits_raw <- covid_test
# state <- "California"
generate_admits_arima <- function(covid_admits_raw, state) {
  #Select only variables needed to calculate Covid hospital admits
  covid_admits <- as.data.frame(covid_admits_raw) %>% 
    select(date, admits) %>%
    mutate(state=paste0(state))
  
  #Sum admits in last 24 hours by date and remove NAs to calculate admits state-wide
  covid_admits_s <- covid_admits %>% group_by(date) %>% 
    summarize(admits = sum(admits, na.rm=TRUE)) %>%
    mutate(state=paste0(state)) %>%
    filter(row_number()==1) #removes any duplicate rows
  
  covid_admits <- covid_admits %>%  
    bind_rows(covid_admits_s) %>% #Combine original df with aggregated state df
    replace_na(list(admits=0)) %>%  #Replace any NA values for the admits column with 0
    arrange(state, date) %>% #arrange by state name and date
    group_by(state) %>% #smoothing: 7-day rolling mean
    mutate(rolling_admits = zoo::rollsum(admits, k = 7, align = "right", fill = NA)) %>%
    filter(date > Sys.Date()-90) #filter for dates after 3 months prior to current date

  #Store unique counties as list object
  geo_admits <- as.list(unique(covid_admits$state))
  
  #Loops over each state in geo_admits and runs covid_admits_arima(), then combines into list
  rbindlist(lapply(geo_admits, function(x) try(covid_admits_arima(covid_admits, x))))
}

#' Title covid_admits_arima
#'
#' @param df A data frame containing aggregated counts for forecasting variable of interest by state and date.
#' @param geos  A list of unique states in specified df.
#'
#' @return A data frame with forecasting results and uncertainty intervals by state.
#' @export
#'
#' @examples

covid_admits_arima <- function(df, geos=geo_admits){
  #Filter dataframe for counties found in the df resulting from generate_admits_arima
  df <- df %>% filter(state == geos) %>% as.data.frame()
  #Store min and max dates
  max_date <- max(df$date)
  #Convert data frame to time series data type for auto.arima() function (need to convert dates to decimal_date for)
  # Removed "start=lubridate::decimal_date(min_date)," because ts() function requires a full number of cycles
  train <- ts(df$rolling_admits, end = lubridate::decimal_date(max_date), frequency = 365)
  #Apply auto.arima() function to time series object
  mods <- auto.arima(train)
  #Apply forecast() function to mods object
  fc <- forecast(mods, h=33)
  #Format results
  hosp_admits <- cbind(seq(max_date+1, by = "day", length.out = 33), 
                       data.frame(fc$mean),
                       data.frame(fc$lower),
                       data.frame(fc$upper))
  hosp_admits$state <- geos
  hosp_admits$model <- "arima"
  names(hosp_admits) <- c("date","hosp_admits","hosp_admits_l20","hosp_admits_l5","hosp_admits_u80","hosp_admits_u95","state", "model")
  
  return(hosp_admits)
}


#' Title generate_admits_damped_holts
#'
#' @param covid_admits_raw A data frame containing columns corresponding to date, state, and variables of interest for forecasting.
#' @param state A character string of the state of interest, fully spelled out (e.g., "California").
#'
#' @return A data frame with forecasts and uncertainty intervals of forecasted variables for each state.
#' @export
#'
#' @examples generate_admits_damped_holts(df, "California")
#' 
generate_admits_damped_holts <- function(covid_admits_raw, state) {
  #Select only variables needed to calculate Covid hospital admits
  covid_admits <- as.data.frame(covid_admits_raw) %>% 
    select(date, admits) %>%
    mutate(state=paste0(state))
  
  #Sum admits in last 24 hours by date and remove NAs to calculate admits state-wide
  covid_admits_s <- covid_admits %>% group_by(date) %>% 
    summarize(admits= sum(admits, na.rm=TRUE)) %>%
    mutate(state=paste0(state)) %>%
    filter(row_number()==1) #removes any duplicate rows
  
  covid_admits <- covid_admits %>%  
    bind_rows(covid_admits_s) %>% #Combine original df with aggregated state df
    replace_na(list(admits=0)) %>%  #Replace any NA values for the admits column with 0
    arrange(state, date) %>% #arrange by state name and date
    group_by(state) %>% #smoothing: 7-day rolling mean
    mutate(rolling_admits= zoo::rollsum(admits, k = 7, align = "right", fill = NA)) %>%
    filter(date > Sys.Date()-90) #filter for dates after 3 months prior to current date
  
  #Store unique counties as list object
  geo_admits <- as.list(unique(covid_admits$state))
  
  #Loops over each state in geo_admits and runs covid_admits_arima(), then combines into list
  rbindlist(lapply(geo_admits, function(x) try(covid_admits_damped_holts(covid_admits, x))))
}

#' Title covid_admits_damped_holts
#'
#' @param df A data frame containing aggregated counts for forecasting variable of interest by state and date.
#' @param geos A list of unique states in specified df.
#'
#' @return A data frame with forecasting results and uncertainty intervals by state.
#' @export
#'
#' @examples
covid_admits_damped_holts <- function(df, geos=geo_admits){
  #Filter dataframe for counties found in the df resulting from generate_admits_arima
  df <- df %>% filter(state == geos) %>% as.data.frame()
  #Store max date
  max_date <- max(df$date)
  #Convert data frame to time series data type for auto.arima() function (need to convert dates to decimal_date for)
  # Removed "start=lubridate::decimal_date(min_date)," because ts() function requires a full number of cycles
  train <- ts(df$rolling_admits, end = lubridate::decimal_date(max_date), frequency = 365)
  #Apply auto.arima() function to time series object
  mods <- holt(train, damped=TRUE, phi=0.8, h=33)
  #Apply forecast() function to mods object
  fc <- forecast(mods, h=33)
  #Format results
  hosp_admits <- cbind(seq(max_date+1, by = "day", length.out = 33), 
                       data.frame(fc$mean),
                       data.frame(fc$lower),
                       data.frame(fc$upper))
  hosp_admits$state <- geos
  hosp_admits$model <- "dampedholts"
  names(hosp_admits) <- c("date","hosp_admits","hosp_admits_l20","hosp_admits_l5","hosp_admits_u80","hosp_admits_u95","state", "model")
  
  return(hosp_admits)
}

#' Title generate_admits_holts
#'
#' @param covid_admits_raw A data frame containing columns corresponding to date, state, and variables of interest for forecasting.
#' @param state A character string of the state of interest, fully spelled out (e.g., "California").
#'
#' @return A data frame with forecasts and uncertainty intervals of forecasted variables for each state.
#' @export
#'
#' @examples generate_admits_holts(df, "California")
generate_admits_holts <- function(covid_admits_raw, state){
  #Select only variables needed to calculate Covid hospital admits
  covid_admits <- as.data.frame(covid_admits_raw) %>% 
    select(date, admits) %>%
    mutate(state=paste0(state))
  
  #Sum admits in last 24 hours by date and remove NAs to calculate admits state-wide
  covid_admits_s <- covid_admits %>% group_by(date) %>% 
    summarize(admits= sum(admits, na.rm=TRUE)) %>%
    mutate(state=paste0(state)) %>%
    filter(row_number()==1) #removes any duplicate rows
  
  covid_admits <- covid_admits %>%  
    bind_rows(covid_admits_s) %>% #Combine original df with aggregated state df
    replace_na(list(admits=0)) %>%  #Replace any NA values for the admits column with 0
    arrange(state, date) %>% #arrange by state name and date
    group_by(state) %>% #smoothing: 7-day rolling mean
    mutate(rolling_admits = zoo::rollsum(admits, k = 7, align = "right", fill = NA)) %>%
    filter(date > Sys.Date()-90) #filter for dates after 3 months prior to current date
  
  #Store unique counties as list object
  geo_admits <- as.list(unique(covid_admits$state))
  
  #Loops over each state in geo_admits and runs covid_admits_arima(), then combines into list
  rbindlist(lapply(geo_admits, function(x) try(covid_admits_holts(covid_admits, x))))
}

#' Title covid_admits_holts
#'
#' @param df A data frame containing aggregated counts for forecasting variable of interest by state and date.
#' @param geos A list of unique counties in specified df.
#'
#' @return A data frame with forecasting results and uncertainty intervals by state.
#' @export
#'
#' @examples
covid_admits_holts <- function(df, geos = geo_admits) {
  #Filter dataframe for counties found in the df resulting from generate_admits_arima
  df <- df %>% filter(state == geos) %>% as.data.frame()
  #Store max date
  max_date <- max(df$date)
  #Convert data frame to time series data type for auto.arima() function (need to convert dates to decimal_date for)
  # Removed "start=lubridate::decimal_date(min_date)," because ts() function requires a full number of cycles
  train <- ts(df$rolling_admits, end = lubridate::decimal_date(max_date), frequency = 365)
  #Apply auto.arima() function to time series object
  mods <- holt(train, h=33)
  #Apply forecast() function to mods object
  fc <- forecast(mods, h=33)
  #Format results
  hosp_admits <- cbind(seq(max_date+1, by = "day", length.out = 33), 
                       data.frame(fc$mean),
                       data.frame(fc$lower),
                       data.frame(fc$upper))
  hosp_admits$state <- geos
  hosp_admits$model <- "holts"
  names(hosp_admits) <- c("date","hosp_admits","hosp_admits_l20","hosp_admits_l5","hosp_admits_u80","hosp_admits_u95","state", "model")
  
  return(hosp_admits)
}



#' Title generate_admits_NNETAR
#'
#' @param covid_admits_raw A data frame containing columns corresponding to date, state, and variables of interest for forecasting.
#' @param state A character string of the state of interest, fully spelled out (e.g., "California").
#'
#' @return A data frame with forecasts and uncertainty intervals of forecasted variables for each state.
#' @export
#'
#' @examples generate_admits_nn(df, "California")
generate_admits_NNETAR <- function(covid_admits_raw, state) {
  #Select only variables needed to calculate Covid hospital admits
  covid_admits <- as.data.frame(covid_admits_raw) %>% 
    select(date, admits) %>%
    mutate(state=paste0(state))
  
  #Sum admits in last 24 hours by date and remove NAs to calculate admits state-wide
  covid_admits_s <- covid_admits %>% group_by(date) %>% 
    summarize(admits= sum(admits, na.rm=TRUE)) %>%
    mutate(state=paste0(state)) %>%
    filter(row_number()==1) #removes any duplicate rows
  
  covid_admits <- covid_admits %>%  
    bind_rows(covid_admits_s) %>% #Combine original df with aggregated state df
    replace_na(list(admits=0)) %>%  #Replace any NA values for the admits column with 0
    arrange(state, date) %>% #arrange by state name and date
    group_by(state) %>% #smoothing: 7-day rolling mean
    mutate(rolling_admits= zoo::rollsum(admits, k = 7, align = "right", fill = NA)) %>%
    filter(date > Sys.Date()-90) #filter for dates after 3 months prior to current date
  
  #Store unique counties as list object
  geo_admits <- as.list(unique(covid_admits$state))
  
  #Loops over each state in geo_admits and runs covid_admits_arima(), then combines into list
  rbindlist(lapply(geo_admits, function(x) try(covid_admits_NNETAR(covid_admits, x))))
}


#' Title covid_admits_NNETAR
#'
#' @param df A data frame containing aggregated counts for forecasting variable of interest by state and date.
#' @param geos A list of unique counties in specified df.
#'
#' @return A data frame with forecasting results and uncertainty intervals by state.
#' @export
#'
#' @examples
covid_admits_NNETAR <- function(df, geos=geo_admits){
  #Filter dataframe for counties found in the df resulting from generate_admits_arima
  df <- df %>% filter(state == geos) %>% as.data.frame()
  #Store max date
  max_date <- max(df$date)
  #Convert data frame to time series data type for auto.arima() function (need to convert dates to decimal_date for)
  # Removed "start=lubridate::decimal_date(min_date)," because ts() function requires a full number of cycles
  train <- ts(df$rolling_admits, end = lubridate::decimal_date(max_date), frequency = 365)
  #Apply auto.arima() function to time series object
  mods <- nnetar(train, PI=TRUE) #For NNETAR, must define prediction intervals prior to forecast step
  # Admits_nnetar<- nnetar(Admits_ts, PI=c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99))
  #Apply forecast() function to mods object
  fc <- forecast(mods, PI=TRUE, h=33)
  #Format results
  hosp_admits <- cbind(seq(max_date+1, by = "day", length.out = 33), 
                       data.frame(fc$mean),
                       data.frame(fc$lower),
                       data.frame(fc$upper))
  hosp_admits$state <- geos
  hosp_admits$model <- "nnetar"
  names(hosp_admits) <- c("date","hosp_admits","hosp_admits_l20","hosp_admits_l5","hosp_admits_u80","hosp_admits_u95","state", "model")
  
  return(hosp_admits)
}


