to_xts_CAN <- function(df,c,measure) {
  d <- df
  j <- d[which(d$county==c),c(2,3,5:7)] %>%
    gather(key, value, hospitalizations:deaths) %>%
    spread("intervention", value) %>% as.data.frame()
  
  j.ts <- xts(j[which(j$key == measure), -1], 
              j[which(j$key == measure), 1])
  j.ts <- j.ts[,-1]
  
  return(j.ts)
}