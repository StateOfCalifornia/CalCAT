#### Import Actuals ####

#############################################################
##### Dummy Code for importing actuals for other states #####
##### Uses NYT data to approximate Hosp/ICU numbers     #####
#############################################################

grab_dummy_data <- function(s) {
  nyt_c <- read.csv("https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv", stringsAsFactors = F) %>% 
    filter(state == s) %>%
    mutate(COVID.19.Positive.Patients =  round(cases*.025,0),
           ICU.COVID.19.Positive.Patients = round(COVID.19.Positive.Patients*0.34,0)) %>%
    rename(Total.Count.Deaths = deaths,
           Most.Recent.Date = date,
           County.Name = county) %>%
    select(Most.Recent.Date, COVID.19.Positive.Patients,ICU.COVID.19.Positive.Patients, Total.Count.Deaths, County.Name) %>%
    arrange(County.Name,Most.Recent.Date)
  nyt_s <- read.csv("https://github.com/nytimes/covid-19-data/raw/master/us-states.csv", stringsAsFactors = F) %>% 
    filter(state == s) %>%
    mutate(COVID.19.Positive.Patients =  round(cases*.025,0),
           ICU.COVID.19.Positive.Patients = round(COVID.19.Positive.Patients*0.34,0),
           County.Name = state) %>%
    rename(Total.Count.Deaths = deaths,
           Most.Recent.Date = date) %>%
    select(Most.Recent.Date, COVID.19.Positive.Patients,ICU.COVID.19.Positive.Patients, Total.Count.Deaths, County.Name) %>%
    arrange(Most.Recent.Date)
  nyt <- rbind(nyt_c,nyt_s)
  return(nyt)
}