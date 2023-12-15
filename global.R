##################################################
##### A gift from California with love. ##########
####   Together, all things are possible.   ######
###################### -- Cesar Chavez ###########
##################################################

# Copyright 2020, State of California, Department of Public Health

library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs) # Facilitate the icon navigation
#library(DT)
library(shiny)
library(shinyWidgets)
library(tibble)
library(purrr)
library(readr)
library(stringr)
library(DBI)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(viridis)
library(lubridate)
library(data.table)
library(xts)
library(plotly)
library(scales)
library(httr)
library(jsonlite)
library(RSocrata)
library(forecast)

#Support Functions

sapply(list.files("R", full.names = T), source)
state_name <- "California"

#counties <- get_counties()
state_abbrv <- get_state_abbrv(State = state_name)
state_fips <- get_state_fips(type = "integer")
state_fips_char <- get_state_fips(type = "character")

data_path <- paste0("data/",state_abbrv,"/")

date_updated <- "December 14, 2023"

#### Supporting Data ####

#### read in County and associated FIPS codes

#cnty.list <- sort(c(unique(as.character(counties$county))))
#cnty.list<- c(state_name,cnty.list)

#fipslist <- make_fips_list()

#### Read in population numbers

#cnty.pop <- get_county_populations()

#### County Bed Data ###

# cnty.beds <- read in data on the number of available hospital beds here

#### Actuals ####
##### You will need to replace this data with real data from your own state #####
##### For demonstration purposes, we produce dummy data based on the New York Times Repository #####
##### https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv ######

# covid <- grab_dummy_data(state_name)
# covid$Most.Recent.Date <- as.Date(covid$Most.Recent.Date)
covid_actuals <- read.csv(paste0(data_path, "covid_actuals.csv"))

#### Nowcast/Forecast Data ####

nowcasts <- bind_rows(lapply(list.files(file.path(data_path, "nowcasts"), full.names = T), readRDS))
forecasts <- bind_rows(lapply(list.files(file.path(data_path, "forecasts"), full.names = T), readRDS))
