
JHUoutputs <- c("Peak* Hospitalizations" = "hosp_occup", 
                "Peak* ICU Beds" = "icu_occup" ,
                #"Peak* COVID Hospital Occupancy" = "hosp_occup", 
                #"Peak* New COVID Daily Hospital Admissions" = "hosp_admit" ,
                #"Peak* COVID ICU Bed Occupancy" = "icu_occup" ,
                #"Peak* New COVID Daily ICU Admissions" = "icu_admit" ,
                #"Peak* New Daily COVID Infections" = "new_infect",
                #"Peak* New COVID Daily Deaths" = "new_deaths",
                "Peak Cumulative Deaths" = "cum_deaths" 
                )

CANoutputs <- c(#"Peak New Infections" = "infected",
                "Peak Hospitalizations" = "hospitalizations",
                #"Beds Needed?" <- "beds"
                "Peak Cumulative Deaths" = "deaths"
              )

IHMEoutputs.ts <- c("All Beds" = "allbed_mean",
                    "ICU Beds" = "ICUbed_mean",
                    "Inv. Ventitilators" = "invVen_mean",
                    "Deaths" =  "deaths_mean",
                    "Admissions" = "admis_mean",
                    "New ICU" = "newICU_mean",
                    "Total Deaths" = "totdea_mean",
                    "Beds Over" = "bedover_mean",
                    "ICU Over" = "icuover_mean"
                    )

COVIDvar    <- c( #"Total Confirmed Cases" = "Total.Count.Confirmed",
                  "Patients Positive for COVID-19" = "COVID.19.Positive.Patients",
                  #"Suspected COVID-19 Patients" = "Suspected.COVID.19.Positive.Patients",
                  "ICU Patients Positive for COVID-19"=  "ICU.COVID.19.Positive.Patients",
                  "Total Deaths, Confirmed" = "Total.Count.Deaths"
                  #"ICU Patients Suspected for COVID-19" = "ICU.COVID.19.Suspected.Patients",
                  #"Positive + Suspected Hospital Patients" = "total.hospital",
                  #"Positive + Suspected ICU Patients" = "total.icu"
                  )

COVIDvar.ts <- c( #"Total Confirmed Cases" = 20,
                  "Patients Positive for COVID-19" = 22,
                  #"Suspected COVID-19 Patients" = 23,
                  "ICU Patients Positive for COVID-19"=  24,
                  "Total Deaths, Confirmed" = 21
                  #"ICU Patients Suspected for COVID-19" = 25,
                  #"Positive + Suspected Patients" = 26,
                  #"Positive + Suspected ICU Patients" = 27
                  )

scenarios <- data.frame(
  colvar = c( 
    "strictDistancingNow", 
    "weakDistancingNow",
    "IHME_sts",
    "UK.Fixed.30_40",
    "UK.Fixed.40_50",
    "UK.Fixed.50_60",
    "UK.Fixed.60_70",
    "Continued_Lockdown",
    "Slow.paced_Reopening",
    "Moderate.paced_Reopening",
    "Fast.paced_Reopening"
    
  ),
  label = c(
    'CAN: Shelter in Place',
    'CAN: Delay/Distancing',
    'IHME Model',
    'JHU: NPIs 30-40% Effective',
    'JHU: NPIs 40-50% Effective',
    'JHU: NPIs 50-60% Effective',
    'JHU: NPIs 60-70% Effective',
    'JHU: Continued Lockdown',
    'JHU: Slow-paced Reopening',
    'JHU: Moderate-paced Reopening',
    'JHU: Fast-paced Reopening'
  ),
  group = c(
    'other',
    'other',
    'other',
    "UK",
    "UK",
    "UK",
    "UK",
    "UK",
    "UK",
    "UK",
    "UK"
  ),
  descrip = c(
    
    "Shelter-in-place or Containment/Delay: Three months of voluntary/VolunTold 'shelter-in-place' community-wide home quarantine (especially firm for high-risk groups), shutdown of non-essential businesses, close schools, ban on events over 10 people, passive monitoring, public advocacy around physical distancing and enhanced hygiene. Possibly closed borders or restricted travel. Public aid relief bill. Roll-out of free population-wide testing and quarantine, so that quarantines can be relaxed for those who are not infected. Strict physical distancing: Three month of shelter at home, reducing transmission between mildy sympotomatic individuals and the susceptible population. Treat everyone as infected. Forced community-wide home quarantine, full shutdown of all businesses, closed borders, active monitoring, full population-wide mandatory testing and aggressive quarantine.",
    "Delay/Distancing: Three months of voluntary 'shelter-in-place' for high-risk groups, ban on events over 50 people, public advocacy around “physical distancing” and enhanced hygiene, possible school closures, restricted travel, and passive monitoring. Roll-out of population-wide testing and quarantine, so that quarantines can be relaxed for those who are not infected.",
    
    "Assumes school closures, essential services closed, and Shelter in place beginning March 19th and extending indefinitely.",
    
    "Fixed UK Lockdown followed by physical distancing: This scenario has statewide school closures from March 13-19 followed by a statewide stay-at-home policy from March 19 through April 30 where individuals remain socially distanced with constant effectiveness over the 6-week period. From May 1 through March 1, 2021, there is constant physical distancing with a 30-40% effectiveness.",
    "Fixed UK Lockdown followed by physical distancing: This scenario has statewide school closures from March 13-19 followed by a statewide stay-at-home policy from March 19 through April 30 where individuals remain socially distanced with constant effectiveness over the 6-week period. From May 1 through March 1, 2021, there is constant physical distancing with a 40-50% effectiveness.",
    "Fixed UK Lockdown followed by physical distancing: This scenario has statewide school closures from March 13-19 followed by a statewide stay-at-home policy from March 19 through April 30 where individuals remain socially distanced with constant effectiveness over the 6-week period. From May 1 through March 1, 2021, there is constant physical distancing with a 50-60% effectiveness.",
    "Fixed UK Lockdown followed by physical distancing: This scenario has statewide school closures from March 13-19 followed by a statewide stay-at-home policy from March 19 through April 30 where individuals remain socially distanced with constant effectiveness over the 6-week period. From May 1 through March 1, 2021, there is constant physical distancing with a 60-70% effectiveness.",
    
    "Stay-at-home policy is in place through August 31.",
    "Stay-at-home policy is in place through May 8. Restrictions are loosened in 6-week phases with social distancing effectiveness between 50–70% from May 9–June 19 for Stage 2, 35–55% from June 20–July 31 for Stage 3, and 20–40% from August 1–31 for Stage 4.",
    "Stay-at-home policy is in place through May 8. Restrictions are loosened in 4-week phases with social distancing effectiveness between 50–70% from May 9–June 5 for Stage 2, 35–55% from June 6–July 3 for Stage 3, and 20–40% from July 4–August 31 for Stage 4.",
    "Stay-at-home policy is in place through May 8. Restrictions are loosened in 2-week phases with social distancing effectiveness between 50–70% from May 9–22 for Stage 2, 35–55% from May 23–June 5 for Stage 3, and 20–40% from June 6–August 31 for Stage 4."
  )
  
  
)

modellist <- as.list(as.character(scenarios[,"colvar"]))
names(modellist) <- scenarios[,as.character("label")]

UKlist <- as.list(as.character(scenarios[which(scenarios$group == "UK"),"colvar"]))
names(UKlist) <- scenarios[which(scenarios$group == "UK"),"label"]

otherlist <- as.list(as.character(scenarios[which(scenarios$group == "other"),"colvar"]))
names(otherlist) <- scenarios[which(scenarios$group == "other"),"label"]


