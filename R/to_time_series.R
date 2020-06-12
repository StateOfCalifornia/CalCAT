to_xts_JHU <- function(df,c,measure) {
  d <- df
  j <- d[which(county==c),c(2:10,12)] %>%
       gather(key, value, hosp_occup_mean:cum_deaths_mean) %>%
       spread("intervention", value) %>% as.data.frame()
  
  j$date <- as.Date(as.POSIXct(j$date), tz = "")
  
  j.ts <- xts(j[which(j$key == measure), -2], 
              j[which(j$key == measure), 1])
  return(j.ts)
}

to_xts_awsJHU <- function(df,c,measure) {
  d <- df
  j <-  d[which(county==c),c(1:30)] %>%
    gather(key, value, hosp_occup_mean:cum_deaths_q75 ) %>%
    spread("intervention", value) %>% as.data.frame()
  
  j.ts <- xts(j[which(j$key == measure), -1], 
              j[which(j$key == measure), 1])
  j.ts <- j.ts[,-1]
  return(j.ts)
}

