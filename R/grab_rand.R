  
grab_rand <- function(State = state_name){

  rand_scenarios <- as.data.frame(cbind(rep(0:5,3), rep(1:3,each=6)))
  rand_urls <- as.data.frame(lapply(rand_scenarios[1], function(i) {paste0("https://in93tc2ktf.execute-api.us-east-1.amazonaws.com/v1?q=(and%20location_id:%27",get_rand_state_number(State),"%27%20data_type:%27prots%27%20intervention_level:%27", as.character(rand_scenarios$V1) ,"%27%20intervention_date_id:%27", as.character(rand_scenarios$V2),"%27)&q.parser=structured&size=1000")}))
  
  # rand_urls$V1 <- as.character(rand_urls$V1)
  rand_urls <- as.list(as.character(rand_urls[,"V1"]))
  rand_out <- lapply(seq_along(rand_urls), function(i) {  rbind(jsonlite::fromJSON(rand_urls[[i]])$hits$hit$fields) })
  rand_df <- rbindlist(rand_out)
  rand_df$date <- as.Date(rand_df$date_str)
  rand_df <- rand_df %>% mutate_if(is.character,as.numeric)
  rand_df$scenario <- paste0(rand_df$intervention_level,"-",rand_df$intervention_date_id)
  
  return(rand_df)
  
}