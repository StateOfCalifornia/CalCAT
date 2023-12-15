#library(devtools)
#install_github("covid-19-Re/estimateR")
library(estimateR)
library(Metrics)
deconvolve_hosp_to_onset <- function(raw_data){
  lnorm_meansd_to_meanlog <- function(mean, sd) { log(mean^2 / sqrt(mean^2 + sd^2)) }
  lnorm_meansd_to_sdlog   <- function(mean, sd) { sqrt(log(1 + (sd^2 / mean^2))) }
  
  load("data/params/onsetToHospParameters_latest.Rdata") #delay distribution generated on Nov 30, 2023 based on CA data
  
  sars_cov_2_distribution_onset_to_hospitalization <- list(
    name = onsetToHosp$distribution_type,
    meanlog = lnorm_meansd_to_meanlog(onsetToHosp$distribution_mean, onsetToHosp$distribution_sd),
    sdlog = lnorm_meansd_to_sdlog(onsetToHosp$distribution_mean, onsetToHosp$distribution_sd))
  
  define_incubation <- function(mean, sd) {
    #' SARS-CoV-2  Delay between infection and onset of symptoms (incubation period) in days
    sars_cov_2_distribution_incubation <- list(
      name = "lnorm",
      meanlog = getLogNormalParams(mean, sd)$logMean,
      sdlog = getLogNormalParams(mean,sd)$logSD)
    
    return(sars_cov_2_distribution_incubation)
  }
  
  getLogNormalParams <- function(meanParam, sdParam){
    logmeanParam <- log(meanParam^2 / sqrt(sdParam^2 + meanParam^2))
    logSDParam <- sqrt(log(1 + (sdParam^2 / meanParam^2)))
    return(list(logMean = logmeanParam, logSD = logSDParam))
  }
  
  # Incubation Period
  sars_cov_2_distribution_incubation <- define_incubation(3.1, 2.6)
  
  delay = list(sars_cov_2_distribution_incubation, 
               sars_cov_2_distribution_onset_to_hospitalization)
  x = deconvolve_incidence(
    raw_data$admits,
    deconvolution_method = "Richardson-Lucy delay distribution",
    delay,
    simplify_output = FALSE)
  
  y = data.frame(date = raw_data$date + days(x$index_offset), admitsDeconvolved = x$values)
  
  return(y)
}