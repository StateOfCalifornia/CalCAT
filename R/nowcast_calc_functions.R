est_rt_JRC <- function(ts, window_interval = 3L) {
  res <- sapply( 1:(length(ts)-window_interval), function(t) {
    ((log(ts[t+window_interval])-log(ts[t]))/window_interval)*7 + 1
  })
  return(res)
}

generate_jrc <- function(dat, state){
  
  #dat_filtered = dat[which(dat$location != "Unknown"),]
  
  #Start Date based filtering
  start_date = as.Date("2022-03-01")
  
  state_smooth <- dat %>%
    mutate(location = state) %>% 
    filter(date >= start_date) %>%
    mutate(y = admitsDeconvolved) %>% 
    filter(!is.na(y), y >=0) %>% 
    select(date, y) %>% 
    arrange(date)
  
  #State R-eff and smoothing
  out_agg = state_smooth
  rt_jrc_state  = est_rt_JRC(out_agg$y %>% setNames(out_agg$date), window_interval = 3)
  rt_jrc_df_state <- data.frame(date=as.Date(names(rt_jrc_state)), R_hat=rt_jrc_state, Method="JRC, GT=3")
  
  rt_jrc_df_state$R_hat[which(rt_jrc_df_state$R_hat < 0)] = 0
  rt_jrc_df_state$R_hat[(nrow(rt_jrc_df_state)-1):nrow(rt_jrc_df_state)] = NA #For smoother latest date related estimates
  rt_jrc_df_state <- rt_jrc_df_state %>%
    mutate(value = frollmean(R_hat, n = 7, align= "right", na.rm = T))
  
  jrc_dat_state <- tibble(
    location_level = "state", 
    location = state, 
    date = rt_jrc_df_state$date,
    metric = "reff", 
    model = "JRC", 
    value = rt_jrc_df_state$value
  ) 
  
  
  return(bind_rows(jrc_dat_state))
  
}

est_rt_Cislaghi <- function(ts, window_interval) {
  # data1 <- accelerometry::movingaves(ts, window=5)
  # names(data1) <- names(ts)[3:(length(ts)-2)]
  data1 = ts
  res <- sapply( (1+window_interval):length(data1), function(t) {
    data1[t]/data1[t-window_interval]
  })
  return(res)
  
}

generate_cislaghi <- function(dat, state){
  
 # dat_filtered = dat[which(dat$location != "Unknown"),]
  
  #Start Date based filtering
  start_date = as.Date("2021-03-01")
  
  state_smooth <- dat %>%
    filter(date >= start_date) %>%
    mutate(location = state,
           y = admitsDeconvolved) %>% 
    filter(!is.na(y)) %>% 
    select(date, y) %>% 
    arrange(date)
  
  
  gt = 3
  
  #State R-eff and smoothing
  out_agg = state_smooth
  rt_Cislaghi_state <- est_rt_Cislaghi(out_agg$y  %>% setNames(out_agg$date), window_interval=gt)
  rt_Cislaghi_df_state <- data.frame(date=as.Date(names(rt_Cislaghi_state)), R_hat=rt_Cislaghi_state, Method=" 3")
  
  rt_Cislaghi_df_state <- rt_Cislaghi_df_state%>%
    filter(R_hat > 0) 
  rt_Cislaghi_df_state$R_hat[(nrow(rt_Cislaghi_df_state)-1):nrow(rt_Cislaghi_df_state)] = NA #For smoother latest date related estimates
  rt_Cislaghi_df_state <- rt_Cislaghi_df_state %>%
    mutate(value = frollmean(R_hat, n = 7, align= "right", na.rm = T))
  
  Cislaghi_dat_state <- tibble(
    location_level = "state", 
    location = state, 
    date = rt_Cislaghi_df_state$date,
    metric = "reff", 
    model = "Cislaghi", 
    value = rt_Cislaghi_df_state$value
  ) 
  
  
  return(bind_rows(Cislaghi_dat_state))
  
}

grab_covidestim <- function(state_name)
{
  state_url <- "https://cdn.covidestim.org/latest-v2/state/estimates.csv"
  ce_state <- read_csv(state_url) %>% filter(state == state_name) %>% 
    select(location = state, date, value = r_t) %>% 
    group_by(location) %>% 
    complete(date = seq.Date(min(date), max(date), by="day")) %>% 
    mutate(value = zoo::na.approx(value, na.rm = F)) %>% 
    mutate(model = "covidestim.org",
           location_level = "state",
           metric = "reff")
  return(ce_state)
}

