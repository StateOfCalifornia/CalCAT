
yucolname_to_date <- function( key)
{
  datestr <- str_replace( key, "^Predicted Deaths( Intervals)* by ", "" )
  # The column headings don't include the year so we have to guess its the same as this year
  # except right at year end
  thisy <- as_date(fast_strptime( paste(datestr, year(today())), "%b %d %Y"))
  nexty <- as_date(fast_strptime( paste(datestr, year(today())+1), "%b %d %Y"))
  
  as_date(ifelse( thisy>=today()-7, thisy, nexty))
}

grab_yugroup <- function(State = state_name){
  
  url <- paste0( "https://docs.google.com/spreadsheets/d/1ZSG7o4cV-G0Zg3wlgJpB2Zvg-vEN1i_76n2I-djL0Dk")
  
  if (as.character(url_file_exists(url)[1]) == "TRUE" ) {
    gs4_deauth()
    
    # There are notes at the top so the header starts at row 3
    state_abbrv <- get_state_abbrv(State = state_name)
    yu <- read_sheet(url, range = cell_rows(c(3,NA))) %>% filter(StateName==state_abbrv)

    msg <- paste0("Successfully download data from Yu Group on ", Sys.Date())
    
  } else {
    msg <- paste0("Problem with Yu Group link to file updates. Check URL.")
  }
  
  print(msg)
  
  # Make the data tall instead of wide, different dates are in different headers
  # Also massage a string which represents the high/low range into numeric columns
  predcols <- colnames(yu)[str_detect(colnames(yu), "^Predicted Deaths")]
  
  yutall <- gather(yu, key, value, all_of(predcols )) %>% 
    mutate( type=if_else(str_detect(key, "Interval"), "range", "predicted_deaths")) %>%
    mutate( date=yucolname_to_date(key)) %>%  select( -key )
  
  rr.l <- yutall %>% filter( type=="range") %>% 
    mutate( type = "range_low", value =  as.numeric(str_extract(str_extract(value,"^\\(.*," ),"[0-9]+")))
  rr.h <- yutall %>% filter( type=="range") %>% 
    mutate( type = "range_high", value =  as.numeric(str_extract(str_extract(value,",.*\\)" ),"[0-9]+")))
  rr.p <- yutall %>% filter( type=="predicted_deaths") %>% mutate(value=as.numeric(value))
  
  yu  <- rbind( rr.p, rr.l,rr.h) %>% spread(type,value)
  return(yu)
}
