##################################################
##### A gift from California with love. ##########
#### “Together, all things are possible.”   ######
###################### -- Cesar Chavez ###########
##################################################


library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs) # Facilitate the icon navigation
#library(DT)
library(shiny)
library(readr)
library(stringr)
library(pool)
library(DBI)
#library(odbc)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(viridis)
library(lubridate)
library(data.table)
library(dygraphs)
library(xts)
#library(ggiraph)
#library(sf)
library(plotly)
library(scales)
#library(httr)
library(jsonlite)
#library(curl)
#library(openssl)
#Support Functions

# sapply(list.files("R/", full.names = T), source)
sapply(list.files("R", full.names = T), source)
state_name <- "Georgia"

counties <- get_counties()
state_abbrv <- get_state_abbrv(State = state_name)
state_fips <- get_state_fips(type = "integer")
state_fips_char <- get_state_fips(type = "character")

data_path <- paste0("data/",state_abbrv,"/")

date_updated <- "June 7, 2020"

#### Supporting Data ####

#### read in County and associated FIPS codes

cnty.list <- sort(c(unique(as.character(counties$county))))
cnty.list<- c(state_name,cnty.list)

fipslist <- make_fips_list()

#### Read in population numbers

cnty.pop <- get_county_populations()

#### County Bed Data ###

# cnty.beds <- read in data on the number of available hospital beds here

#### Actuals ####
##### You will need to replace this data with real data from your own state #####
##### For demonstration purposes, we produce dummy data based on the New York Times Repository #####
##### https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv ######

covid <- grab_dummy_data(state_name)
covid$Most.Recent.Date <- as.Date(covid$Most.Recent.Date)

#### Nowcast/Forecast Data ####

### rt.live ###

rt_live <- fread(paste0(data_path, "rt_live.csv")) %>% mutate(date = as.Date(date))
  
### COVIDActNow Reff ###

can.state.observed <- fread(paste0(data_path,"can_state_reff_table.csv")) %>% mutate(date = as.Date(date))
can.county.observed <-  fread(paste0(data_path,"can_full_reff_table.csv")) %>% mutate(date = as.Date(date))


### Covid Act Now is not produced for every county, here we see which exist for your state ####
temp <- data.table(can.county.observed) %>% .[!is.na(RtIndicator), .(fips = unique(fips)), by =.(county)]
canfipslist <- as.list(temp$fips)
names(canfipslist) <- temp$county
rm(temp)

### Epiforecasts ###

epi_forecast <- fread(paste0(data_path,"/epi_forecast.csv")) %>% mutate(date = as.Date(date))

### ICL Rt ###

icl <- fread(paste0(data_path,"/icl_rt.csv")) %>% mutate(date = as.Date(date))

### Youyang Gu Group ### 
# https://github.com/youyanggu/covid19_projections/raw/master/projections/combined/latest_us.csv
yu <- fread(paste0(data_path,"/ygg.csv")) %>% mutate(date = as.Date(date)) 

### IHME Proj. ###

IHME <- fread(paste0(data_path,"/ihme.csv")) %>% mutate(date = as.Date(date))

####  Reich Lab 

reich_lab <- fread(paste0(data_path,"/reich_data.csv")) %>% mutate(target_end_date = as.Date(target_end_date))

### MOBS ### 

mobs <- fread(paste0(data_path,"/mobs.csv"))%>% mutate(date = as.Date(date)) 

### MIT ###
#https://www.covidanalytics.io/projections
#mit <- read.csv("data/covid_analytics_projections.csv", stringsAsFactors = FALSE) %>% filter(Province == "California")
mit <- fread(paste0(data_path,"/mit.csv")) %>% mutate(date = as.Date(Day)) 

### UCLA ###
# "https://gist.githubusercontent.com/knowzou/ecacd65ab863979a9aea0f19a75252c3/raw/us_rt.json"

ucla_state <- fread(paste0(data_path, "/ucla_state.csv")) %>% mutate(date = as.Date(date)) 

### Youyang Gu Group ### 
# https://github.com/youyanggu/covid19_projections/raw/master/projections/combined/latest_us.csv
yu.cnty <- fread(paste0(data_path, "ygg_county.csv")) %>% mutate(date = as.Date(date))

####
#### Scenarios ####
####

#### Imperial College London ####

icl_model <- fread(paste0(data_path, "/icl_model.csv")) %>% mutate(date = as.Date(date)) 


#### COVIDActNow ####

can.weak <-  fread(paste0(data_path,"/can_weak_scenario.csv")) %>% 
             mutate(date = as.Date(date),
                    intervention =  "weakDistancingNow") %>% 
             rename(infected = cumulativeInfected,
                    hospitalizations = hospitalBedsRequired,
                    beds = ICUBedsInUse,
                    deaths = cumulativeDeaths) %>%
             left_join(counties, by = c("fips" = "fips")) %>% 
             #left_join(cnty.pop, by = c("county" = "county")) %>%
             select(fips, date, intervention, infected, hospitalizations, beds, deaths, county) #%>%pop2020, 
             #rename(totalPopulation = pop2020)


can.strong <-  fread(paste0(data_path,"/can_strong_scenario.csv")) %>% 
               mutate(date = as.Date(date),
                      intervention = "strictDistancingNow") %>% 
               rename(infected = cumulativeInfected,
                       hospitalizations = hospitalBedsRequired,
                       beds = ICUBedsInUse,
                       deaths = cumulativeDeaths) %>%
               left_join(counties, by = c("fips" = "fips")) %>% 
               #left_join(cnty.pop, by = c("county" = "county")) %>%
               select(fips, date, intervention, infected, hospitalizations, beds, deaths,county) #%>% pop2020, 
               #rename(totalPopulation = pop2020)

CAN_aws <- rbind(can.weak,can.strong)
CAN_aws$county[is.na(CAN_aws$county)] <- state_name
can_counties <- unique(CAN_aws$county) %>% na.omit()


