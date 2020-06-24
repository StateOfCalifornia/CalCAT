##################################################
##### A gift from California with love. ##########
#### “Together, all things are possible.”   ######
###################### -- Cesar Chavez ###########
##################################################


# Developed by California COVID Modeling Team
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
        
    title = paste0(state_name, " COVID Assessment Tool - Open Source"),
    id = "navbar",
    inverse=TRUE,
    selected = "Introduction",
    position = "static-top",
    theme = shinytheme("cosmo"),
    windowTitle = "CalCAT - Open",
    header = tagList( shinyWidgets::useShinydashboard(),
                      tags$head(
                          tags$script("src" = "func.js"),
                          ######## Google Analytics Script Start ###############
                          HTML(
                              "<script async src='https://www.googletagmanager.com/gtag/js?id=UA-170240821-2'></script>"
                          ),
                          includeScript("g-analytics.js"))
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
                         column(2, ""),
                         column(2,
                             HTML("<div style='text-align:center'><h2>Nowcasts</h2> </div><br>")
                         ),
                         column(1, ""),
                         column(2,
                             HTML("<div style='text-align:center'><h2>Forecasts</h2> </div><br>")
                         ),
                         column(1, ""),
                         column(2,
                             HTML("<div style='text-align:center'><h2>Scenarios</h2> </div><br>")
                         ),
                         column(2, "")
                 ), #End of fluidRow
                 fluidRow(
                         column(2, ""),
                         column(2,
                             img(id = "nowcast_img", src = 'nowcast_icon.jpg', align = "center", width = '100%', style="cursor:pointer;", alt = "Nowcasts"),
                             HTML("<hr><center>How fast is COVID-19 spreading right now? </center>")
                         ),
                         column(1, ""),
                         column(2,
                             img(id = "forecast_img", src = 'forecast_icon.jpg', align = "center", width = '100%', style="cursor:pointer;", alt = "Forecasts"),
                             HTML("<hr><center>What can we expect in the next 2-4 weeks?</center>")
                         ),
                         column(1, ""),
                         column(2,
                             img(id = "epimodel_img", src = 'scenario_icon.jpg', align = "center", width = '100%', style="cursor:pointer;", alt = "Scenarios"),
                             HTML("<hr><center>What are the long-term impacts under different scenarios?</center>")
                         ),
                         column(2, "")
                 ), #End of fluidRow
                 
                 # FOOTER #
                 fluidRow(br(), 
                         br(), 
                         hr()
                ), #End of fluidRow
                fluidRow(
                      column(2,""),
                      column(4,
                            
                             h4(paste0("Alpha Version | Released ",date_updated)),
                             #h4("EXPIRES: April 10, 2020"),
                             HTML("<h5>CONTACT: YOUR CONTACT INFO HERE</h5>"),
                                                         HTML("<h5><a href='https://www.cdph.ca.gov/Programs/CID/DCDC/Pages/Immunization/ncov2019.aspx' target='_blank'>CDPH COVID-19 Page</a>  |  <a href='https://covid19.ca.gov/' target='_blank'>ca.gov COVID-19 Page</a> | <a href= 'https://github.com/StateOfCalifornia/CalCAT' target='_blank'>CalCAT Open Source</a></h5> "),
                             HTML("<h6>Icons provided by the nounproject.com: <a href='https://thenounproject.com/browse/?i=2683859' target='_blank'>Magnify</a> | <a href='https://thenounproject.com/browse/?i=772325' target='_blank'>Binoculars</a> | <a href='https://thenounproject.com/browse/?i=1630136' target='_blank'>Telescope</a>")
                     ),
                     column(4,
                            img(src = 'calcat_logo.gif', align = "right"),
                            br()
                           
                     ),
                     column(2,"")
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
                         actionButton("Rt_explain", "What does a Reff of this size mean?"),
                         h4("Low/High Estimates of R-effective:"),
                            fluidRow(uiOutput("hilo_rt.box", width = 12)),
                         downloadButton("dlRt", "Download R-eff Values")
                     ), 
                     column(9, 
                            h4("Statewide Estimates of R-effective"),
                            h6("The effective reproductive number (R) is the average number of secondary infected persons resulting from a infected person. If R>1, the number of infected persons will increase. If R<1, the number of infected persons will decrease.  At R=1, the number of infected persons remains constant." ), 
                            fluidRow(plotlyOutput("rt.plot"))
                     )
                 ),# End fluidRow
                 hr(),
                 fluidRow(
                     column(6, 
                            div(style="display: inline-block;vertical-align:top; width: 200px;", 
                                pickerInput(inputId = "select.county.rt",
                                                label = NULL, 
                                                choices = canfipslist,
                                                selected = "6001",
                                                #options = list(
                                                #    `actions-box` = TRUE), 
                                                multiple = FALSE
                                                )
                            ),
                            div(style="display: inline-block;vertical-align:top; width: 200px;", 
                                    downloadButton("dlRt.indv.cnty", "Download County R-eff Trend")
                            ),
                            h4("R-effective Trends by County"),
                            h6("Select a county to see how R-effective has changed over time"),
                            plotlyOutput("county.rt.plot")
                        ),
                     
                     column(6,
                            downloadButton("dlRt.cnty", "Download R-eff for Counties"),
                            h4(paste0("Latest R-effective in ",state_name," Counties")),
                            h6("Using estimates and uncertainty intervals from COVID Act Now, the following graph presents the averaged R-eff over the last 7 days by county; (<1 decreasing spread, >1 increasing spread)"),
                            plotlyOutput("rt.dot.plot")
                        )
                 ), # End fluidRow
                 
                 # FOOTER #
                 fluidRow(br(), 
                          br(), 
                          hr()
                 ), #End of fluidRow
                 fluidRow(
                     column(2,""),
                     column(4,
                            
                            h4(paste0("Alpha Version | Released ",date_updated)),
                            #h4("EXPIRES: April 10, 2020"),
                            HTML("<h5>CONTACT: YOUR CONTACT INFO HERE</h5>"),
                                                        HTML("<h5><a href='https://www.cdph.ca.gov/Programs/CID/DCDC/Pages/Immunization/ncov2019.aspx' target='_blank'>CDPH COVID-19 Page</a>  |  <a href='https://covid19.ca.gov/' target='_blank'>ca.gov COVID-19 Page</a> | <a href= 'https://github.com/StateOfCalifornia/CalCAT' target='_blank'>CalCAT Open Source</a></h5> ")
                     ),
                     column(4,
                            img(src = 'calcat_logo.gif', align = "right"),
                            br()
                            
                     ),
                     column(2,"")
                 ) #End of fluidRow
              ) # End fluidPage
            ), # End tabPanel

#### Forecasts Page ####
    tabPanel( "Forecasts",
             fluidPage(

                       h2(paste0("Short-term COVID-19 Forecasts in ", state_name)),
                       h4("Short-term forecasts take into account the most recent trends in cases, hospitalizations and deaths and apply statistical models to that data to generate anticipated trends in the coming 2-4 weeks."),
                       hr(),
                       fluidRow(
                               column(3,
                                      h4("Current Daily Hospitalizations:"),
                                      fluidRow(
                                      valueBoxOutput("actual.hosp.box", width = 12),
                                      #uiOutput("proj.hosp.box")
                                      ),
                                      h4("30-Day Projected Total:"),
                                      fluidRow(uiOutput("mean.proj.hosp.box")  ),
                                      downloadButton("dlhosp", "Download Hospital Forecasts")
                                      ),
                                      column(9,
                                            h4("Statewide Hospitalization Forecasts"),
                                            h6(paste0("The black box (left) represents the current number of hospitalized COVID patients in ", state_name,". The blue box 
                                                represents the average forecasted number of hospitalized patients at the 30 day mark based on models for ",state_name,"." )), 
                                            plotlyOutput("hosp.proj.plot", height = "100%")
                                      )

                      ), #End of fluidRow
                      hr(),
                      h4(paste0(state_name," County Hospitalization Forecasts")),
                      h6("Select a county to see how modeled number of hospitalizations compare with actual numbers to date and with the number of licensed hospital beds (black box)."),
                      fluidRow(
                          column(3,
                                  pickerInput(
                                      inputId = "select.county.hosp",
                                      label = NULL, 
                                      choices = canfipslist,
                                      selected = "6001",
                                      multiple = FALSE
                                      ),
                                  h4("Current Daily Hospitalizations:"),
                                  fluidRow( valueBoxOutput("actual.cnty.hosp.box", width = 12) ),
                                  h4("30 day Projected Total:"),
                                  fluidRow( valueBoxOutput("mean.cnty.proj.hosp.box", width = 12) ),
                                  downloadButton("dlhosp.cnty", "Download County Hospital Forecasts")
                          ),
                          column(9,
                             plotlyOutput("county.hosp.plot")
                          )
                      ), #End of fluidRow
                      hr(),
                      fluidRow(
                          column(3,
                                 h4("Current Total Deaths:"),
                                 fluidRow(
                                     valueBoxOutput("actual.cdeath.box", width = 12)                                 #
                                 ),
                                 h4("Projected Total:"),
                                 fluidRow(uiOutput("mean.proj.cdeaths.box")  ),
                                 downloadButton("dlDeath", "Download Total Death Forecasts")
                          ),
                          column(9,
                                 h4("Statewide Total Death Forecast"),
                                 h6(paste0("The black box (left) represents the current number of total COVID deaths in",state_name,". The blue box 
                                                represents the average forecasted number of total deaths at the 30 day mark based on models for",state_name,"." )), 
                                 plotlyOutput("cdeath.proj.plot", height = "100%")
                          )
                          
                      ), #End of fluidRow
                      hr(),
                      h4(paste0(state_name," County Death Forecasts")),
                      h6("Select a county to see how modeled number of cumulative deaths compare with actual numbers to date (black box)."),
                      fluidRow(
                          column(3,
                                 pickerInput(
                                     inputId = "select.county.death",
                                     label = NULL, 
                                     choices = canfipslist,
                                     selected = "6001",
                                     multiple = FALSE
                                 ),
                                 h4("Current Total Deaths:"),
                                 fluidRow( valueBoxOutput("actual.cnty.death.box", width = 12) ),
                                 h4("30 day Projected Total:"),
                                 fluidRow( valueBoxOutput("mean.cnty.proj.death.box", width = 12) ),
                                 downloadButton("dlDeath.cnty", "Download Total Death Forecasts")
                          ),
                          column(9,
                                 plotlyOutput("county.death.plot")
                          )
                      ), #End of fluidRow
                      
                      # FOOTER #
                      fluidRow(br(), 
                               br(), 
                               hr()
                      ), #End of fluidRow
                      fluidRow(
                          column(2,""),
                          column(4,
                                 
                                 h4(paste0("Alpha Version | Released ",date_updated)),
                                 #h4("EXPIRES: April 10, 2020"),
                                 HTML("<h5>CONTACT: YOUR CONTACT INFO HERE</h5>"),
                                                             HTML("<h5><a href='https://www.cdph.ca.gov/Programs/CID/DCDC/Pages/Immunization/ncov2019.aspx' target='_blank'>CDPH COVID-19 Page</a>  |  <a href='https://covid19.ca.gov/' target='_blank'>ca.gov COVID-19 Page</a> | <a href= 'https://github.com/StateOfCalifornia/CalCAT' target='_blank'>CalCAT Open Source</a></h5> ")
                          ),
                          column(4,
                                 img(src = 'calcat_logo.gif', align = "right"),
                                 br()
                                 
                          ),
                          column(2,"")
                      ) #End of fluidRow
                      
             ) #End of fluidPage
    ), #End of tabPanel Short-term forecasts

#### Epi Models ####
    tabPanel( "Scenarios",
              fluidPage(
                  includeCSS("www/dygraph_custom.css"),
                  h2("Long term Scenarios"),
                  h4("Long-term scenarios estimate the effect of various non-pharmaceutical interventions (NPIs)"),
                  hr(),
                  fluidRow(
                      wellPanel( 
                          div(style="display: inline-block;vertical-align:top; width: 200px;",
                              selectInput(
                                  inputId = "county_ts",
                                  label = "Please select Geography:",
                                  choices = cnty.list,
                                  selected = state_name
                              )
                              
                          ),
                          div(style="display: inline-block;vertical-align:top; width: 200px;",
                              selectInput("selected_crosswalk",
                                          "Select a Model Output:",
                                          choices = c("Hospitalizations" = 1,
                                                      "ICU Beds" = 2,
                                                      "Cumulative Deaths" = 3
                                          ),
                                          selected = 1)
                          ),
                          
                          div(style="display: inline-block;vertical-align:top; width: 200px; padding: 20px;",
                              checkboxInput("actuals","Include Actuals?",TRUE)
                          ),
                          div(style="display: inline-block;vertical-align:top; width: 300px;",
                              uiOutput("epi_covid_select")
                          ),
                          HTML("<p><b>Note:</b> Detailed model scenario descriptions can be found below the graph or on the Technical Notes tab.</p>")
                      )
                  ),
                  
                  tabsetPanel(
                      tabPanel("Interactive",
                               fluidRow(
                                   column(3,
                                          br(),
                                          uiOutput("physical.select"),
                                          conditionalPanel(condition = "input.include_JHU_UKKC.includes('UK.Fixed.30_40') |
                                                                        input.include_JHU_UKKC.includes('UK.Fixed.40_50') |
                                                                        input.include_JHU_UKKC.includes('UK.Fixed.50_60') |
                                                                        input.include_JHU_UKKC.includes('UK.Fixed.60_70') |
                                                                        input.include_JHU_UKKC.includes('Continued_Lockdown') |
                                                                        input.include_JHU_UKKC.includes('Slow.paced_Reopening') |
                                                                        input.include_JHU_UKKC.includes('Moderate.paced_Reopening') |
                                                                        input.include_JHU_UKKC.includes('Fast.paced_Reopening')",
                                                           h5("Johns Hopkins Model Options:"),
                                                           radioGroupButtons(
                                                               inputId = "physical.mmd",
                                                               label = NULL,
                                                               choices = c("Mean" = "M",
                                                                           "Median" = "MD"),
                                                               selected = "MD",
                                                               justified = TRUE,
                                                               status = "primary",
                                                               size = 'sm',
                                                               checkIcon = list(
                                                                   yes = icon("ok", lib = "glyphicon"))
                                                           ),
                                                           switchInput("physical.iqr", label = "JHU IQRs", value = FALSE,
                                                                       onStatus = "success", offStatus = "danger",
                                                                       labelWidth = "150px",
                                                                       inline = TRUE, size = 'normal', width = "auto") 
                                          ),
                                          conditionalPanel(condition = paste0("input.include_JHU_UKKC.includes('IHME_sts') & input.county_ts == '",state_name,"'"),
                                                           h5("IHME Model Options:"),
                                                           switchInput("IHME.iqr", label = "IHME Interval", value = FALSE,
                                                                       onStatus = "success", offStatus = "danger",
                                                                       labelWidth = "150px",
                                                                       inline = TRUE, size = 'normal', width = "auto") 
                                          ),
                                          h5("Legend"),
                                          textOutput("legendDivID2"),
                                          br(),
                                          downloadButton("dlScenario", "Download Scenarios in Graph")
                                   ),
                                   column(9,
                                          dygraphOutput("physical.graph", height = "450px"),
                                          h6("Note: Use slide to adjust date range; will also adjust date range of static plot.")
                                          
                                   )
                               ), #End of fuidRow
                               fluidRow(
                                   hr(),
                                   h4("Selected Model Descriptions - See Technical Notes tab for more information"),
                                   uiOutput("model.descrip.ts"),
                                   hr()
                               )
                      ),
                      tabPanel("Static"
                               ,
                               column(2,
                                      checkboxInput("drop_hline","Include Phase 1 Capacity Reference Line",TRUE)
                               ),
                               column(8,
                                      br(),
                                      plotOutput("physical.graph.static", width = "1000px", height = "600px")
                               ),
                               column(2)
                      ) #End of tabPanel
                  ), #End of TabsetPanel
                  
                  # FOOTER #
                  fluidRow(
                      column(2,""),
                      column(4,
                             
                             h4(paste0("Alpha Version | Released ",date_updated)),
                             #h4("EXPIRES: April 10, 2020"),
                             HTML("<h5>CONTACT: YOUR CONTACT INFO HERE</h5>"),
                                                         HTML("<h5><a href='https://www.cdph.ca.gov/Programs/CID/DCDC/Pages/Immunization/ncov2019.aspx' target='_blank'>CDPH COVID-19 Page</a>  |  <a href='https://covid19.ca.gov/' target='_blank'>ca.gov COVID-19 Page</a> | <a href= 'https://github.com/StateOfCalifornia/CalCAT' target='_blank'>CalCAT Open Source</a></h5> ")
                      ),
                      column(4,
                             img(src = 'calcat_logo.gif', align = "right"),
                             br()
                             
                      ),
                      column(2,"")
                  ) #End of fluidRow
                  
              ) #End of fluidPage
            ), # End tabPanel for Long-term Epi Models

#### Technical Notes ####
    tabPanel("Technical Notes",
             fluidPage(
                fluidRow(
                 column(4,
                        h1("Nowcasts"),
                        h4("Rt.live"),
                        HTML("<a href = 'https://rt.live/' target='_blank'>rt.live</a><p>Rt.live provides a state-level estimate of R-effective, taking the number of cases and the input.  It accounts for the delay from infection to onset of symptoms and changes in the amount of testing done."),
                        p(),
                        HTML("<a href = 'https://d14wlfuexuxgcm.cloudfront.net/covid/rt.csv' target='_blank'>raw data</a>"),
                        hr(),
                        h4("Covid Act Now"),
                        HTML("<a href = 'https://blog.covidactnow.org/modeling-metrics-critical-to-reopen-safely/' target='_blank'>Covid Act Now</a><p>Covid Act Now provides statewide and county level estimates of R-effective, taking mortality and confirmed cases as inputs.  Because of potential reporting delays and errors in the data, they perform smoothing, and require 10 preceding days of data.  County-level estimates on the 'Nowcasts'' tab show average R-effective for the last 7 days."),
                        p(),
                        HTML("<a href = 'https://data.covidactnow.org/latest/us/states/CA.OBSERVED_INTERVENTION.timeseries.json' target='_blank'>raw data</a>"),
                        hr(),
                        h4("Epi Forecasts"),
                        HTML("<a href = 'https://epiforecasts.io/covid/posts/national/united-states/' target='_blank'>Epi Forecasts</a><p>Epi Forecasts provides national and state-level estimates of R-effective, taking the number of cases as an input.  It accounts for the delay from infection to onset of symptoms."),
                        p(),
                        HTML("<a href = ' https://github.com/epiforecasts/covid-regional/raw/ada5b4ec0a5e786712c630708aaf85de663e2dde/united-states/regional-summary/rt.csv' target='_blank'>raw data</a>"),
                        hr(),
                        h4("COVID-19 Projections (covid19-projections.com)"),
                        HTML("<a href = 'https://covid19-projections.com/' target='_blank'>COVID-19 Projections (Youyang Gu)</a><p>
                                   COVID-10 Projections fits a parameterized S-curve (logistic function) for R-effective to minimize the mean-squared error of historical daily mortality data.  COVID-19 Projections provides state-level estimates for R-effective, mortality and the infected population."),
                        p(),
                        HTML("<a href = 'https://github.com/youyanggu/covid19_projections/raw/master/projections/combined/latest_us.csv' target='_blank'>raw data</a>"),
                        hr(),
                        h4("UCLA  Machine Learning Lab"),
                        HTML("<a href = 'https://covid19.uclaml.org/' target='_blank'>UCLA ML Lab</a><p>UCLA ML Lab provides state and California county projections of mortality and the number of confirmed cases.  At the the state level, they publish current R-effective.  UCLA ML uses a modified SEIR model with a compartment for undetected cases.  The model parameters are selected to minimize the historical prediction error of the number of current cases and number of recovered patients."),
                        p(),
                        HTML("<a href = 'https://gist.githubusercontent.com/knowzou/ecacd65ab863979a9aea0f19a75252c3/raw/us_rt.json' target='_blank'>raw data</a>"),
                        hr(),
                        h4("Imperial College London (ILC)"),
                        HTML("<a href = 'https://www.imperial.ac.uk/mrc-global-infectious-disease-analysis/covid-19/report-23-united-states/' target='_blank'>Imperical College London COVID-19 State-Level Tracking</a><p>Description is forthcoming."),
                        p(),
                        HTML("<a href = 'https://mrc-ide.github.io/covid19usa/#/download' target='_blank'>raw data</a>"),
                 ), # end Nowcasts Column
                 column(4,
                        
                     h1("Forecasts"),
                     h4("Covid Act Now"),
                     HTML("<a href = 'https://blog.covidactnow.org/modeling-metrics-critical-to-reopen-safely/' target='_blank'>Covid Act Now</a><p>The CovidActNow model is a SEIR model with compartments for disease severity and medical intervention.  Each county and state is calibrated separately, and R-effective is inferred using observed data."),
                     p(),
                     HTML("<a href = 'https://data.covidactnow.org/latest/us/states/CA.OBSERVED_INTERVENTION.timeseries.json' target='_blank'>raw data</a>"),
                     hr(),
                     h4("Institute for Health Metrics and Evaluation"),
                     HTML("<a href = 'https://covid19.healthdata.org/united-states-of-america' target='_blank'>IHME</a><p>IHME is a multistage model, where the first stage fits an S-curve to historical daily deaths data, and the second stage is an SEIR compartment model.  The SEIR model's R-effective is calibrated using the output of the first stage, but it also incorporates temperature data, population density, local testing capacity, and changes in mobility data.  IHME provides projections of mortality, number of infections, and hospital utilization at the state and national level."),
                     p(),
                     HTML("<a href = 'https://ihmecovid19storage.blob.core.windows.net/latest/ihme-covid19.zip' target='_blank'>raw data</a>"),
                     hr(),
                     h4("GLEAM"),
                     HTML("<a href = 'https://covid19.gleamproject.org/' target='_blank'>MOBS</a><p>The Global Epidemic and Mobility Model (GLEAM) uses a individual-based, stochastic spatial epidemic model.  The model uses mobility data and travel patterns to simulate spatial contact patterns.  The likely ranges of basic parameters, such as R0 and IFR, are inferred from observed data."),
                     p(),
                     HTML("<a href = 'https://data-tracking-api-dot-mobs-2019-ncov-web.appspot.com/data?state=California&frequency=daily'>raw data</a>"),
                     hr(),
                     h4("MIT Delphi"),
                     HTML("<a href = 'https://www.covidanalytics.io/' target='_blank'>MIT</a><p>MIT DELPHI is a standard SEIR model with compartments for undetected cases and hospitalizations. R-effective is modeled as an S-curve (arctan) to reflect government interventions and social distancing."),
                     p(),
                     HTML("<a href = 'https://www.covidanalytics.io/projections/covid_analytics_projections.csv' target='_blank'>raw data</a>"),
                     hr(),
                     h4("COVID-19 Forecast Hub"),
                     HTML("<a href = 'https://reichlab.io/covid19-forecast-hub/' target='_blank'>Reich Lab</a><p>The Reich Lab at the UMass-Amherst is an Influenza Forecasting Center of Excellence and the source for the official <a href= 'https://www.cdc.gov/coronavirus/2019-ncov/covid-data/forecasting-us.html'  target='_blank'>CDC COVID-19 Forecasting page</a>. Taking other forecasts as the input, this is arithmetic average across eligible models of cumulative deaths forecasts.  Forecasts are weekly out to 4 weeks, at the state and national level. "),
                     p(),
                     HTML("<a href = 'https://github.com/reichlab/covid19-forecast-hub/tree/master/data-processed' target='_blank'>raw data</a>"),
                     hr()
                 ),
                 column(4,
                        h1("Scenarios"),
                        h4("Covid Act Now"),
                        HTML("<a href = 'https://covidactnow.org/us/ca/' target='_blank'>Covid Act Now</a><p>The CovidActNow model is a SEIR model with compartments for disease severity and medical intervention.  Parameters such as R0, infectious period and IFR are model inputs.  Different scenarios parameterize future values of R-effective."),
                        p(),
                        HTML("<a href = 'https://github.com/covid-projections/covid-data-model/blob/master/api/README.V1.md' target='_blank'>raw data</a>"),
                        hr(),
                        
                        h4("Institute for Health Metrics and Evaluation"),
                        HTML("<a href = 'https://covid19.healthdata.org/united-states-of-america' target='_blank'>IHME</a><p>IHME is a multistage model, where the first stage fits an S-curve to historical daily deaths data, and the second stage is an SEIR compartment model.  The SEIR model's R-effective is calibrated using the output of the first stage, but it also incorporates temperature data, population density, local testing capacity, and changes in mobility data.  IHME provides projections of mortality, number of infections, and hospital utilization at the state and national level."),
                        p(),
                        HTML("<a href = 'https://ihmecovid19storage.blob.core.windows.net/latest/ihme-covid19.zip'>raw data</a>"),
                        hr()
                 )
                ), #End of fluidRow 
                
                # FOOTER #
                fluidRow(br(), 
                         br(), 
                         hr()
                ), #End of fluidRow
                fluidRow(
                    column(2,""),
                    column(4,
                           
                           h4(paste0("Alpha Version | Released ",date_updated)),
                           #h4("EXPIRES: April 10, 2020"),
                           HTML("<h5>CONTACT: <a href = 'mailto:covmodeling@cdph.cd.gov?subject=ModelDataBook'>covmodeling@cdph.ca.gov</a></h5>"),
                                                       HTML("<h5><a href='https://www.cdph.ca.gov/Programs/CID/DCDC/Pages/Immunization/ncov2019.aspx' target='_blank'>CDPH COVID-19 Page</a>  |  <a href='https://covid19.ca.gov/' target='_blank'>ca.gov COVID-19 Page</a> | <a href= 'https://github.com/StateOfCalifornia/CalCAT' target='_blank'>CalCAT Open Source</a></h5> ")
                    ),
                    column(4,
                           img(src = 'calcat_logo.gif', align = "right"),
                           br()
                           
                    ),
                    column(2,"")
                ) #End of fluidRow
             ) #End of fluidPage
    ), #End of tabPanel for Technical Notes


                tags$script(HTML(paste0("var header = $('.navbar> .container-fluid');
                       header.append('<div style=\"float:right;color:white\"><h5>Alpha Version | Released ",date_updated,"</h5></div>');
                       console.log(header)")))  

)
