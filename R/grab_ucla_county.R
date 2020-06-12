grab_ucla_county <- function(State = state_name){

  url <- paste0("https://gist.githubusercontent.com/ZeroWeight/9a0c53e56c9bf846485a19a324cf74bd/raw/",str_to_lower(get_state_abbrv(State)),"_all_pred.json")
  
  if (as.character(url_file_exists(url)[1]) == "TRUE" ) {
    
    ucla <- jsonlite::fromJSON(url)
    ucla_raw <- tibble::enframe(unlist(ucla))
    n_cols_max <- ucla_raw %>% pull(name) %>% str_split("\\.") %>% purrr::map_dbl(~length(.)) %>% max()
    nms_sep <- paste0("name", 1:n_cols_max)
    ucla_cnty <- ucla_raw %>% separate(name, into = nms_sep, sep = "\\.", fill = "right" )
    names(ucla_cnty) <- c("output","type","county","date", "value")
    ucla_cnty$date <- as.Date(ucla_cnty$date, format ="%m/%d/%y")
    msg <- paste0("Successfully downloaded Rt data from UCLA for ", State, " on ", Sys.Date())
    
  } else {
    msg <- paste0("Problem with UCLA link to file updates. Data for your state may not exist. Check URL.")
  }
  
  print(msg)
  
  return(ucla_cnty)
}




