###You will need to adjust this function based on the structure of your states actuals
to_xts_COVID <- function(df,c) {
  d <- df
  j <- d[which(d$County.Name==c),c(1:4)] 
  
  #j$date <- as.Date(as.POSIXct(j$date), tz = "")
  
  j.ts <- xts(j[,-1], 
              j[[1]])
  return(j.ts)
}
