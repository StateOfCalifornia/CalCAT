
read_hhs_covid <- function(url){
#https://data.chhs.ca.gov/dataset/6882c390-b2d7-4b9a-aefa-2068cee63e47/resource/6cd8d424-dfaa-4bdd-9410-a3d656e1176e/download/covid19data.csv
  covid <- read.csv(url)
  covid <- covid[,1:8]
  covid[,1:2] <- lapply(covid[,1:2], as.character)
  covid$Most.Recent.Date <- lubridate::mdy(covid$Most.Recent.Date)
  covid$total.hospital <- covid$COVID.19.Positive.Patients + covid$Suspected.COVID.19.Positive.Patients
  covid$total.icu <-  covid$ICU.COVID.19.Positive.Patients + covid$ICU.COVID.19.Suspected.Patients
  covid_s <- aggregate(covid[,3:10],by=list(covid$Most.Recent.Date), sum, na.rm = TRUE)
  covid_s <- covid_s %>% rename(Most.Recent.Date = Group.1)
  covid_s$County.Name <- "California"
  covid <- rbind(covid,covid_s[,c(10,1:9)])
  return(covid)
}
