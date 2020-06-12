read_reich <- function(file_path){
  
  # file_path <- unlist(filelist[5])      # for testing
  
  foo <- data.table::fread(file_path) %>% 
    .[location %in% c(state_name, state_fips_char, state_fips, state_abbrv)] %>%             # filter the location 
    .[, `:=` (  model_team = paste0(sapply(stringr::str_split(file_path, "/|-"),  `[`, 11),           # create new variables  
                                    ".",
                                    sapply(stringr::str_split(file_path, "/|-"),  `[`, 12)),
                new_target = paste0(sapply(stringr::str_split(target, pattern = " "), "[", 4),
                                    " ",
                                    sapply(stringr::str_split(target, pattern = " "), "[", 5)),
                measure = paste0(type, quantile))
    ] %>% 
    .[new_target == "cum death" & measure %in% c("pointNA","quantile0.025","quantile0.975"),        # filter to the estimates we want
      .(target_end_date, target, measure, value, model_team, forecast_date)]  
  
  # print(file_path)  # for testing outputs
  return(foo)
} 