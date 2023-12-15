##################################################
##### A gift from California with love. ##########
#### “Together, all things are possible.”   ######
###################### -- Cesar Chavez ###########
##################################################


# Developed by California COVID Modeling Team
# Copyright 2020, State of California, Department of Public Health
#
# John Pugliese, PhD.
# California Department of Public Health
#
# Jason Vargo, PhD.
# California Department of Public Health
#
# Alpha Version : Released 6/8/2020
#


ui <- navbarPage(

    title = paste0(state_name, " Communicable diseases Assessment Tool - Open Source"),
    id = "navbar",
    inverse=TRUE,
    selected = "Introduction",
    position = "static-top",
    theme = shinytheme("cosmo"),
    windowTitle = "CalCAT - Open",
    header = tagList( shinyWidgets::useShinydashboard(),
                      tags$head(
                          ######## Google Analytics Script Start ###############
                          HTML(
                              "<script async src='https://www.googletagmanager.com/gtag/js?id=UA-170240821-2'></script>"
                          ),
                          includeScript("g-analytics.js"))
    ),
    footer = tagList(
      fluidRow(br(),
               br(),
               hr()
      ), #End of fluidRow
      fluidRow(
        column(2,""),
        column(4,
               
               h4(paste0("Version 2.0.1 | Released ",date_updated)),
               #h4("EXPIRES: April 10, 2020"),
               HTML("<h5>CONTACT: YOUR CONTACT INFO HERE</h5>"),
               HTML("<h5><a href='https://www.cdph.ca.gov/Programs/CID/DCDC/Pages/Immunization/ncov2019.aspx' target='_blank'>CDPH COVID-19 Page</a>  |  <a href='https://covid19.ca.gov/' target='_blank'>ca.gov COVID-19 Page</a> | <a href= 'https://github.com/StateOfCalifornia/CalCAT' target='_blank'>CalCAT Open Source</a></h5> "),
               HTML("<h6>Icons provided by the nounproject.com: <a href='https://thenounproject.com/browse/?i=2683859' target='_blank'>Magnify</a> | <a href='https://thenounproject.com/browse/?i=772325' target='_blank'>Binoculars</a>")# | <a href='https://thenounproject.com/browse/?i=1630136' target='_blank'>Telescope</a>")
        ),
        column(4,
               img(src = 'calcat_long.png', align = "right"),
               br()
               
        ),
        column(2,"")
      ) #End of fluidRow
    ),

######## Google Analytics Script End ###############

#### Landing Page ####
    tabPanel("Introduction",
             fluidPage(
                       tags$head(
                          tags$style(
                                 HTML(".shiny-notification {
                                      height: 50px;
                                      width: 300px;
                                      position: fixed;
                                      top: 50%;
                                      left: 50%;
                                      margin: -70px 0 0 -170px;
                                      }"
                                 )
                             )
                         ),
                 useShinyjs(),
                 fluidRow(
                         column(3,""),
                         column(6,
                                br(),
                                HTML("<div style='text-align:center'><h1>Modeling COVID-19 to Inform State and Local Response</h1></div><br>"),
                                hr()),
                         column(3,""),
                 ), #End of fluidRow
                 br(),
                 br(),
                 fluidRow(
                         column(3, ""),
                         column(2,
                             HTML("<div style='text-align:center'><h2>Nowcasts</h2> </div><br>"),
                             img(id = "nowcast_img", src = 'nowcast_icon.jpg', align = "center", width = '100%', style="cursor:pointer;", alt = "Nowcasts"),
                             HTML("<hr><center>How fast is COVID-19 spreading right now? </center>")
                         ),
                         column(2, ""),
                         column(2,
                             HTML("<div style='text-align:center'><h2>Forecasts</h2> </div><br>"),
                             img(id = "forecast_img", src = 'forecast_icon.jpg', align = "center", width = '100%', style="cursor:pointer;", alt = "Forecasts"),
                             HTML("<hr><center>What can we expect in the next 2-4 weeks?</center>")
                         ),
                         column(3, "")
                 ) #End of fluidRow
             ) # End fluid page
    ), # End tab panel

#### Nowcast Page ####
    tabPanel( "Nowcasts",
              fluidPage(
                 h2(paste0("Current R-effective in ",state_name)),
                 h4("The effective reproduction number (R-effective) is the average number of people each infected person will pass the virus onto and represents the rate at which COVID-19 is spreading."),
                 hr(),
                 fluidRow(
                     column(3,
                         h4("Latest Average R-effective is: "),
                            fluidRow(valueBoxOutput("mean.rt.box", width = 12)),
                         actionButton("Rt_explain", "What does a R-eff of this size mean?"),
                         h4("Low/High Estimates of R-effective:"),
                            fluidRow(uiOutput("hilo_rt.box", width = 12)),
                         downloadButton("dlRt", "Download R-eff Values")
                     ),
                     column(9,
                            h4("Statewide Estimates of R-effective"),
                            h6("The effective reproductive number (R-eff) is the average number of secondary infected persons resulting from a infected person. If R-eff>1, the number of infected persons will increase. If R-eff<1, the number of infected persons will decrease.  At R-eff=1, the number of infected persons remains constant." ),
                            fluidRow(plotlyOutput("rt.plot"))
                     )
                 ),# End fluidRow
                 hr()
              ) # End fluidPage
            ), # End tabPanel

#### Forecasts Page ####
    tabPanel( "Forecasts",
             fluidPage(

                       h2(paste0("Short-term COVID-19 Forecasts in ", state_name)),
                       h4("Short-term forecasts take into account the most recent trends in hospital admissions and apply statistical models to that data to generate anticipated trends in the coming 2-4 weeks."),
                       hr(),
                       fluidRow(
                               column(3,
                                      h4("Current Weekly Hospital Admissions:"),
                                      fluidRow(
                                      valueBoxOutput("actual.hosp.box", width = 12),
                                      #uiOutput("proj.hosp.box")
                                      ),
                                      h4("30-Day Projected Total:"),
                                      fluidRow(uiOutput("mean.proj.hosp.box")  ),
                                      downloadButton("dlhosp", "Download Hospital Admissions Forecasts")
                                      ),
                                      column(9,
                                            h4("Statewide Hospital Admissions Forecasts"),
                                            h6(paste0("The black box (left) represents the current number of weekly hospitalized COVID patients in ", state_name,". The blue box
                                                represents the average forecasted number of hospitalized patients at the 30 day mark based on models for ",state_name,"." )),
                                            plotlyOutput("hosp.proj.plot", height = "100%")
                                      )

                      ), #End of fluidRow
                      hr(),
                      # h4(paste0(state_name," County Hospitalization Forecasts")),
                      # h6("Select a county to see how modeled number of hospitalizations compare with actual numbers to date and with the number of licensed hospital beds (black box)."),
                      # fluidRow(
                      #     column(3,
                      #             pickerInput(
                      #                 inputId = "select.county.hosp",
                      #                 label = NULL,
                      #                 choices = canfipslist,
                      #                 selected = "6001",
                      #                 multiple = FALSE
                      #                 ),
                      #             h4("Current Daily Hospitalizations:"),
                      #             fluidRow( valueBoxOutput("actual.cnty.hosp.box", width = 12) ),
                      #             h4("30 day Projected Total:"),
                      #             fluidRow( valueBoxOutput("mean.cnty.proj.hosp.box", width = 12) ),
                      #             downloadButton("dlhosp.cnty", "Download County Hospital Forecasts")
                      #     ),
                      #     column(9,
                      #        plotlyOutput("county.hosp.plot")
                      #     )
                      # ), #End of fluidRow
                      # hr(),
                      # hr(),


             ) #End of fluidPage
    ), #End of tabPanel Short-term forecasts

#### Technical Notes ####
    tabPanel("Technical Notes",
             fluidPage(
                fluidRow(
                 column(6,
                        h1("Nowcasts"),
                       HTML("<h4>covidestim</h4>
                              <a href = 'https://covidestim.org/' target='_blank'>covidestim.org</a><p>Covidestim calculates state-level effective reproductive numbers, taking cases, deaths and test positivity rates as inputs.  It has corrections to account for lags in diagnosis, disease duration and mortality risk.
                            <p></p>
                              <a href = 'https://github.com/covidestim/' target='_blank'>Code repository</a><br>
                              <a href = 'https://www.medrxiv.org/content/10.1101/2020.06.17.20133983v1/' target='_blank'>Modeling methods</a>
                              
                              <hr>"),
                       HTML("<h4>CDPH JRC</h4>
                 
This is the European Commission's Joint Research Center's methodology for calculating R0 and R-effective, as implemented by the CDPH.  The method is based on estimating the slope of the exponential growth curve of the time series for new cases.  CDPH uses officially reported hospital admissions data deconvolved from date of hospitalization to date of infection, and a serial interval of 3 days. The R package estimateR is used to perform the deconvolution through a variant of the Richardson-Lucy expectation-maximization algorithm and two delay distributions: (1) an estimate of the incubation period, based on <a href = 'https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9155885/'>Aguila-Mejia et al. (2022)</a> and (2) the delay from symptom onset to hospital admission, based on the California COVID-19 case registry. 
<p></p>
       
<a href = 'https://op.europa.eu/en/publication-detail/-/publication/f5ded3cd-d07a-11ea-adf7-01aa75ed71a1' target='_blank'>Modeling methods</a>

<hr>

<h4>CDPH Cislaghi</h4>
                      
This method calculates R-effective as the number of newly diagnosed cases on day <i>s</i> over the number of newly diagnosed cases on day <i>(s - T)</i>, where <i>T</i> is the incubation time. CDPH uses officially reported hospital admissions data deconvolved from date of hospitalization to date of infection (as described in the CDPH JRC section above), and an incubation period (<i>T</i>) of 3 days. To smooth the curve and to avoid strong daily variations due to noise, R-effective was calculated as a seven-day moving average.

<p></p>
       
<a href = 'https://doi.org/10.19191/ep20.5-6.s2.102' target='_blank'>Modeling methods</a>
           
<hr>"),
                       HTML("<h4>Ensemble Nowcast</h4>
                         
The ensemble nowcast takes the median of all the nowcasts available on a given date and smooths it with a three-day moving average.  The methodology aims to be robust to outliers and to avoid kinks in the median nowcast when an input source for the ensemble estimate is unavailable on a certain date.
                                             
<hr>")
                 ), # end Nowcasts Column
                 column(6,

                     h1("Forecasts"),
                     h4("ARIMA Model"),
                     HTML("<a href = 'https://cran.r-project.org/web/packages/forecast/index.html'>R forecast package</a><p>Model forecasts are the result of utilizing the forecast package's automatic ARIMA forecasting model. Note that the form of the model may vary between counties and over subsequent published forecasts.<p><a href='https://cran.r-project.org/web/packages/forecast/vignettes/JSS2008.pdf'>Modeling methods</a>"),
                     p(),
                     hr(),
                     h4("Holts Model"),
                     HTML("<a href = 'https://cran.r-project.org/web/packages/forecast/index.html'>R forecast package</a><p>The Holt's linear trend method extends simple exponential smoothing to allow the forecasting of data with a trend (<a href='https://doi.org/10.1016/j.ijforecast.2003.09.015'>Holt 2004</a>)."),
                     p(),
                     hr(),
                     h4("Damped Holts Model"),
                     HTML("<a href = 'https://cran.r-project.org/web/packages/forecast/index.html'>R forecast package</a><p>The Damped Holt's model is an exponential smoothing model designed to damp erratic trends (<a href='https://pubsonline.informs.org/doi/10.1287/mnsc.31.10.1237'>Gardner & Mckenzie 1985</a>)."),
                     p(),
                     hr(),
                     h4("NNETAR Model"),
                     HTML("<a href = 'https://cran.r-project.org/web/packages/forecast/index.html'>R forecast package</a><p>The neural network autoregression (NNETAR) model is a system of feed-forward neural networks with a single hidden layer and lagged inputs."),
                     p(),
                     hr(),
                     h4("Ensemble Forecast"),
                     HTML("The ensemble forecast takes the median of all the forecasts available on a given date and fits a smoothed spline to the trend. The methodology aims to be robust to outliers, and to avoid artifacts (i.e. abrupt kinks) when the median forecast switches from one source to another."),
                     # h4("Covid Act Now"),
                     # HTML("<a href = 'https://blog.covidactnow.org/modeling-metrics-critical-to-reopen-safely/' target='_blank'>Covid Act Now</a><p>The CovidActNow model is a SEIR model with compartments for disease severity and medical intervention.  Each county and state is calibrated separately, and R-effective is inferred using observed data."),
                     # p(),
                     # HTML("<a href = 'https://data.covidactnow.org/latest/us/states/CA.OBSERVED_INTERVENTION.timeseries.json' target='_blank'>raw data</a>"),
                     # hr(),
                     # h4("Institute for Health Metrics and Evaluation"),
                     # HTML("<a href = 'https://covid19.healthdata.org/united-states-of-america' target='_blank'>IHME</a><p>IHME is a multistage model, where the first stage fits an S-curve to historical daily deaths data, and the second stage is an SEIR compartment model.  The SEIR model's R-effective is calibrated using the output of the first stage, but it also incorporates temperature data, population density, local testing capacity, and changes in mobility data.  IHME provides projections of mortality, number of infections, and hospital utilization at the state and national level."),
                     # p(),
                     # HTML("<a href = 'https://ihmecovid19storage.blob.core.windows.net/latest/ihme-covid19.zip' target='_blank'>raw data</a>"),
                     # hr(),
                     # h4("GLEAM"),
                     # HTML("<a href = 'https://covid19.gleamproject.org/' target='_blank'>MOBS</a><p>The Global Epidemic and Mobility Model (GLEAM) uses a individual-based, stochastic spatial epidemic model.  The model uses mobility data and travel patterns to simulate spatial contact patterns.  The likely ranges of basic parameters, such as R0 and IFR, are inferred from observed data."),
                     # p(),
                     # HTML("<a href = 'https://data-tracking-api-dot-mobs-2019-ncov-web.appspot.com/data?state=California&frequency=daily'>raw data</a>"),
                     # hr(),
                     # h4("MIT Delphi"),
                     # HTML("<a href = 'https://www.covidanalytics.io/' target='_blank'>MIT</a><p>MIT DELPHI is a standard SEIR model with compartments for undetected cases and hospitalizations. R-effective is modeled as an S-curve (arctan) to reflect government interventions and social distancing."),
                     # p(),
                     # HTML("<a href = 'https://www.covidanalytics.io/projections/covid_analytics_projections.csv' target='_blank'>raw data</a>"),
                     # hr(),
                     # h4("COVID-19 Forecast Hub"),
                     # HTML("<a href = 'https://reichlab.io/covid19-forecast-hub/' target='_blank'>Reich Lab</a><p>The Reich Lab at the UMass-Amherst is an Influenza Forecasting Center of Excellence and the source for the official <a href= 'https://www.cdc.gov/coronavirus/2019-ncov/covid-data/forecasting-us.html'  target='_blank'>CDC COVID-19 Forecasting page</a>. Taking other forecasts as the input, this is arithmetic average across eligible models of cumulative deaths forecasts.  Forecasts are weekly out to 4 weeks, at the state and national level. "),
                     # p(),
                     # HTML("<a href = 'https://github.com/reichlab/covid19-forecast-hub/tree/master/data-processed' target='_blank'>raw data</a>"),
                     # hr(),
                     # h4("UC Berkeley Yu Group"),
                     # HTML("<a href = 'https://covidseverity.com/' target='_blank'>covidseverity.com</a><p>A statistical extrapolation which forecasts deaths one week in advance by county.  The extrapolation is calculated from an ensemble of linear and exponential predictors, some of which pool data across counties or use demographic data."),
                     # p(),
                     # HTML("<a href = 'https://docs.google.com/spreadsheets/d/1ZSG7o4cV-G0Zg3wlgJpB2Zvg-vEN1i_76n2I-djL0Dk' target='_blank'>raw data</a>"),
                     # hr()
                 ),
                 # column(4,
                 #        h1("Scenarios"),
                 #        h4("Covid Act Now"),
                 #        HTML("<a href = 'https://covidactnow.org/us/ca/' target='_blank'>Covid Act Now</a><p>The CovidActNow model is a SEIR model with compartments for disease severity and medical intervention.  Parameters such as R0, infectious period and IFR are model inputs.  Different scenarios parameterize future values of R-effective."),
                 #        p(),
                 #        HTML("<a href = 'https://github.com/covid-projections/covid-data-model/blob/master/api/README.V1.md' target='_blank'>raw data</a>"),
                 #        hr(),
                 # 
                 #        h4("Institute for Health Metrics and Evaluation"),
                 #        HTML("<a href = 'https://covid19.healthdata.org/united-states-of-america' target='_blank'>IHME</a><p>IHME is a multistage model, where the first stage fits an S-curve to historical daily deaths data, and the second stage is an SEIR compartment model.  The SEIR model's R-effective is calibrated using the output of the first stage, but it also incorporates temperature data, population density, local testing capacity, and changes in mobility data.  IHME provides projections of mortality, number of infections, and hospital utilization at the state and national level."),
                 #        p(),
                 #        HTML("<a href = 'https://ihmecovid19storage.blob.core.windows.net/latest/ihme-covid19.zip'>raw data</a>"),
                 #        hr()
                 # )
                ) #End of fluidRow

              
             ) #End of fluidPage
    ), #End of tabPanel for Technical Notes


                tags$script(HTML(paste0("var header = $('.navbar> .container-fluid');
                       header.append('<div style=\"float:right;color:white\"><h5>Version 2.0.1 | Released ",date_updated,"</h5></div>');
                       console.log(header)")))

)
