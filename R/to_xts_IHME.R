to_xts_IHME <- function(df,c,measure) {
  d <- df
  j <- d[which(d$county==c),c(2,3:29)] 
  
  #j$date <- as.Date(as.POSIXct(j$date), tz = "")
  
  j.ts <- xts(j[[measure]], 
              j[[1]])
  return(j.ts)
}
