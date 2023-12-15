# Developed by California COVID Modeling Team
# Copyright 2020, State of California, Department of Public Health
#
# Mugdha Thakur, PhD.
# California Department of Public Health
#
# Phoebe Lu, MPH
# California Department of Public Health
#
# John Pugliese, PhD.
# California Department of Public Health
#
# Jason Vargo, PhD.
# California Department of Public Health
#
# Latest Version: Released 12/15/2023
#
# Alpha Version : Released 6/8/2020
#


##################################################
##### A gift from California with love. ##########
#### “Together, all things are possible.”   ######
###################### -- Cesar Chavez ###########
##################################################

library(shiny)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #### Carousel Navigation ####
  
  shinyjs::onclick("nowcast_img",  updateTabsetPanel(session, inputId="navbar", selected= "Nowcasts"))
  shinyjs::onclick("forecast_img",  updateTabsetPanel(session, inputId="navbar", selected= "Forecasts"))
  shinyjs::onclick("epimodel_img",  updateTabsetPanel(session, inputId="navbar", selected= "Scenarios"))
  
  ### Nowcasts of R Effective ####
  
  #Data Prep
  rt.ts <- reactive({
    
    ensemble_df <- nowcasts %>% 
      group_by(date) %>% 
      summarize( n_model = length(value), value = median(value, na.rm=T)) %>%
      filter(n_model > 2) %>% 
      select(-n_model) %>% 
      mutate(model = "Ensemble",
             location_level = "state",
             location = state_name,
             metric = "reff") %>% 
      ungroup()
    return(bind_rows(nowcasts, ensemble_df) %>% filter(!is.na(value)))
    
  })
  
  #Value Boxes
  output$mean.rt.box <- renderValueBox({
    df <- rt.ts() %>% 
      filter(model == "Ensemble")
    cdt <- max(df$date) 
    current.rt <- round(df$value[which(df$date == cdt)], digits = 2)
    valueBox(current.rt, subtitle = paste0(ifelse(current.rt >= 1.4,
                                                  "Spread of COVID-19 is very likely increasing",
                                                  ifelse(current.rt < 1.4 & current.rt >= 1.1,
                                                         "Spread of COVID-19 may be increasing",
                                                         ifelse(current.rt < 1.1 & current.rt >= 0.9,
                                                                "Spread of COVID-19 is likely stable",
                                                                "Spread of COVID-19 is likely decreasing"
                                                         )
                                                  )
    )
    ), 
    color = "blue"
    ) #End valuBox
  })
  
  observeEvent(input$Rt_explain, {
    sendSweetAlert(
      session = session,
      title = "What does a R-eff of this size mean?",
      text = HTML("<p>If the R-effective is greater than 1, COVID-19 will spread <b>exponentially</b>. If R-effective is less than 1, COVID-19 will spread more slowly and cases will decline. The higher the value of R-effective, the faster an epidemic will progress.
                         The following graph illustrates the change in growth as R-effective increases.</p>
                         <img src='reff_cuml_infection.jpg' alt='Infections increase faster with larger values of R-effective' width='450px'/>
                         <p><a href='https://www.cebm.net/covid-19/when-will-it-be-over-an-introduction-to-viral-reproduction-numbers-r0-and-re/' target='_blank'>Source: CEBM</a></p>"
      ),
      html = TRUE,
      type = NULL
    )
  })
  
  output$hilo_rt.box <- renderUI({
    
    df <- rt.ts()
    df <- df %>% filter(date < Sys.Date()) %>%
      pivot_wider(names_from = model, values_from = value) %>% 
      filter(!is.na(Ensemble)) %>% 
      arrange(date) %>% 
      slice(n()) %>% 
      select(-date, -location, -location_level, -metric)
    
    rt.min <- as.numeric( min(df, na.rm = T) )
    rt.max <- as.numeric( max(df, na.rm = T) )
    
    name.min <- names(which.min(as.list(df)))
    
    name.max<- names(which.max(as.list(df)))
    
    tagList(valueBox( paste0( round(rt.min,digits = 2)," - ", round(rt.max,digits = 2)) , paste0(name.min," - ",name.max), color = "navy", width = 12) )
    
  })
  
  #Graph
  output$rt.plot <- renderPlotly({
    
    df <- rt.ts() %>% filter(date < Sys.Date() & date >= (Sys.Date() - 80) )
    
    p <-  plot_ly(df %>% filter(model != "Ensemble"),
                  hoverinfo = 'text') %>%
      add_trace(x =~date, 
                y =~value, 
                name =~model,
                type = 'scatter',
                mode = "lines",
                line = list( dash = 'dot', opacity = 0.5),
                text = paste0(df$date[which(df$model != "Ensemble")],
                              "<br>",
                              df$model[which(df$model != "Ensemble")], " estimated R-eff: ", round(df$value[which(df$model != "Ensemble")], digits=2)
                )) %>%
      add_trace(data = df %>% filter(model == "Ensemble"),
                x = ~date,
                y =~value, 
                name = "Ensemble",
                type = 'scatter',
                mode = "lines", 
                hoverinfo = 'text',
                line = list(color = '#2b8cbe', width = 5),
                text = paste0(df$date[which(df$model == "Ensemble")],
                              "<br>",
                              "Mean estimated R-eff: ", round(df$value[which(df$model == "Ensemble")], digits=2), 
                              "<br>",
                              ifelse(round(df$value[which(df$model == "Ensemble")], digits=2) >= 1.4,
                                     "Spread of COVID-19 is very likely increasing",
                                     ifelse(round(df$value[which(df$model == "Ensemble")], digits=2) < 1.4 & round(df$value[which(df$model == "Ensemble")], digits=2) >= 1.1,
                                            "Spread of COVID-19 may be increasing",
                                            ifelse(round(df$value[which(df$model == "Ensemble")], digits=2) < 1.1 & round(df$value[which(df$model == "Ensemble")], digits=2) >= 0.9,
                                                   "Spread of COVID-19 is likely stable",
                                                   "Spread of COVID-19 is likely decreasing"
                                            )
                                     )
                              )
                )
                
                
      ) %>%
      layout(
        title = NULL,
        xaxis = list(title = NULL, showgrid = FALSE, zeroline = FALSE ),
        yaxis = list(title = "R-Eff", showline = TRUE, showgrid = FALSE, zeroline = FALSE ),
        margin = list(l = 100),
        showlegend = TRUE, 
        shapes = list(
          type = "line", 
          x0 = 0, 
          x1 = 1, 
          xref = "paper",
          y0 = 1, 
          y1 = 1, 
          yref = "y",
          line = list(color = "gray50", dash= "dash", opacity = 0.3))
      ) 
    
    return(p)
    
  })
  
  #Downloadable file of Statewide Reff Values
  output$dlRt <- downloadHandler(
    
    filename = function() { paste("R_eff_Nowcasts_",Sys.Date(),'.csv', sep='') },
    
    content = function(file) {
      
      # Title
      t <- c(paste("R-Effective Model and Ensemble Time Series", sep = ""),"","","","","","")
      #Subtitle
      tt <-  c(paste("Communicable diseases Assessment Tool - Downloaded on",Sys.Date(), sep = " "),"","","","","","")
      #Column labels
      l <- names(rt.ts())
      
      df <- rt.ts() %>% 
        filter(date < Sys.Date() & date > Sys.Date() -80) %>% 
        mutate(value = round(value, 2))
      
      df[] <- lapply(df, as.character)
      
      #Source
      s <- c("Please see the Technical Notes tab of the application for data sources.","","","","","","")
      p <- c(paste0("Prepared by: ",state_name," Department of Public Health"),"","","","","","")
      
      dlm <- rbind(t, tt, l, df, s, p)
      write.table(dlm, file, row.names = F, col.names = F, quote = F, na= "NA", sep = ",")
    })   
  
  
  #### County Rt Nowcasts ####
  
  # #Data Prep
  # county.rt <- reactive({
  #   progress <- Progress$new()
  #   # Make sure it closes when we exit this reactive, even if there's an error
  #   on.exit(progress$close())
  #   progress$set(message = "Gathering R Effective Nowcasts", value = 0)
  #   
  #   c <- names(canfipslist[match(input$select.county.rt,canfipslist)])
  #   cnty <- input$select.county.rt
  #   
  #   progress$inc(3/4)
  #   # out <- lapply(cnty[1], function(x) get_can_cnty(x))
  #   out <- filter(can.county.observed, fips ==  cnty)
  #   
  #   # cnty.rt <- do.call("rbind",out)
  #   cnty.rt <- out %>% select(date,RtIndicator) %>% as.data.frame() #,RtIndicatorCI90
  #   cnty.rt$date <- as.Date(cnty.rt$date)
  #   progress$inc(1/4)
  #   
  #   df <- xts(cnty.rt[,-1],cnty.rt$date)
  #   if( c %in% unique(gu.cnty$subregion)) { 
  #     cnty.gu <- gu.cnty %>% filter(subregion == c) %>% select(date, r_values_mean) 
  #     gu.xts <- xts(cnty.gu[,-1],cnty.gu$date)
  #     df <- merge(df,gu.xts)
  #   }
  #   
  #   # if (c %in% unique(ucla_cnty_rt$county) ) { cnty.ucla <- ucla_cnty_rt %>% filter(county == c) %>% select(date, Rt)       
  #   # ucla.xts <- xts(cnty.ucla[,-1],cnty.ucla$date)
  #   # df <- merge(df,ucla.xts)
  #   # }
  #   
  #   if (ncol(df) > 1) {df$mean.proj <- rowMeans(df[,1:ncol(df)], na.rm = TRUE)} 
  #   
  #   df <- as.data.table(df) %>% as.data.frame() %>% filter(index < Sys.Date())
  #   
  #   return(df)
  #   
  # })
  # 
  # #Graph
  # output$county.rt.plot <- renderPlotly({
  #   
  #   df <- county.rt()
  #   
  #   c <- names(canfipslist[match(input$select.county.rt,canfipslist)])
  #   
  #   #df$ymin <- df$RtIndicator - (df$RtIndicatorCI90)
  #   #df$ymax <- df$RtIndicator + (df$RtIndicatorCI90)
  #   
  #   p <-  plot_ly(df,
  #                 x = df[[1]], 
  #                 y = df[[2]], 
  #                 name = "COVIDActNow",
  #                 type = 'scatter',
  #                 mode = "lines", 
  #                 line = list(color="blue", dash = 'dot', opacity = 0.5),
  #                 hoverinfo = 'text',
  #                 text = paste0(df[[1]],
  #                               "<br>",
  #                               "COVIDActNow estimated Reff: ", round(df[[2]], digits=2) )
  #   )
  #   if (c %in% unique(gu.cnty$subregion)   ) {p <- p %>% add_trace(x = df[[1]], 
  #                                                                  y = df[["gu.xts"]], 
  #                                                                  name = "covid19-projections.com",
  #                                                                  type = 'scatter',
  #                                                                  mode = "lines", 
  #                                                                  line = list(color="red", dash = 'dot', opacity = 0.5),
  #                                                                  hoverinfo = 'text',
  #                                                                  text = paste0(df[[1]],
  #                                                                                "<br>",
  #                                                                                "covid19-projections.com estimated Reff: ", round(df[["gu.xts"]], digits=2) )
  #   ) 
  #   }
  #   # if (c %in% unique(ucla_cnty_rt$county) ) {p <- p %>% add_trace(x = df[[1]], 
  #   #                                                                y = df[["ucla.xts"]], 
  #   #                                                                name = "UCLA",
  #   #                                                                type = 'scatter',
  #   #                                                                mode = "lines", 
  #   #                                                                line = list(color="grey", dash = 'dot', opacity = 0.5),
  #   #                                                                hoverinfo = 'text',
  #   #                                                                text = paste0(df[[1]],
  #   #                                                                              "<br>",
  #   #                                                                              "UCLA estimated Reff: ", round(df[["ucla.xts"]], digits=2) )
  #   # )
  #   # }
  #   if (ncol(df) > 2) {p <- p %>% add_trace(x = df[[1]], 
  #                                           y = df[["mean.proj"]], 
  #                                           name = "Mean Reff",
  #                                           type = 'scatter',
  #                                           mode = "lines", 
  #                                           hoverinfo = 'text',
  #                                           text = paste0(df[[1]],
  #                                                         "<br>",
  #                                                         "Mean estimated Reff: ", round(df[["mean.proj"]], digits=2),
  #                                                         "<br>",
  #                                                         ifelse(round(df[["mean.proj"]], digits=2) >= 1.4,
  #                                                                "Spread of COVID-19 is very likely increasing",
  #                                                                ifelse(round(df[["mean.proj"]], digits=2) < 1.4 & round(df[["mean.proj"]], digits=2) >= 1.1,
  #                                                                       "Spread of COVID-19 may be increasing",
  #                                                                       ifelse(round(df[["mean.proj"]], digits=2) < 1.1 & round(df[["mean.proj"]], digits=2) >= 0.9,
  #                                                                              "Spread of COVID-19 is likely stable",
  #                                                                              "Spread of COVID-19 is likely decreasing"
  #                                                                       )
  #                                                                )
  #                                                         )
  #                                           ),
  #                                           inherit = FALSE,
  #                                           line = list(color = '#2b8cbe', width = 5),
  #                                           linetype = "solid"
  #   )
  #   }
  #   # add_ribbons(x = df[[1]],
  #   #             ymax =  df[[5]],
  #   #             ymin =  df[[4]],
  #   #             opacity = 0.5,
  #   #             inherit = TRUE,
  #   #             line = list(color = '#2b8cbe' ),
  #   #             fillcolor = '#2b8cbe',
  #   #             name = '90% CI'
  #   # ) %>%
  #   p <- p %>% layout(  legend = list(orientation = 'h'),
  #                       title = as.character(counties[match(input$select.county.rt, counties$fips),"county"]),
  #                       xaxis = list(title = NULL, showgrid = FALSE, zeroline = FALSE ),
  #                       yaxis = list(title = "R-Eff", showline = TRUE, showgrid = FALSE, zeroline = FALSE ),
  #                       margin = list(l = 100),
  #                       showlegend = TRUE, 
  #                       shapes = list(
  #                         type = "line", 
  #                         x0 = 0, 
  #                         x1 = 1, 
  #                         xref = "paper",
  #                         y0 = 1, 
  #                         y1 = 1, 
  #                         yref = "y",
  #                         line = list(color = "gray50", dash= "dash", opacity = 0.3)
  #                       )
  #   ) 
  #   
  #   return(p)
  #   
  # })
  
  #Download file of individual COUNTY Reff Values
  # output$dlRt.indv.cnty <- downloadHandler(
  #   
  #   filename = function() { paste("Rt_Nowcasts_",names(canfipslist[match(input$select.county.rt,canfipslist)]),"_",Sys.Date(),'.csv', sep='') },
  #   
  #   content = function(file) {
  #     
  #     c <- names(canfipslist[match(input$select.county.rt,canfipslist)])
  #     # Title
  #     t <- c(paste("R-Effective County Model Time Series for ",c, sep = ""),"","","","")
  #     #Subtitle
  #     tt <-  c(paste("COVID Assessment Tool - Downloaded on",Sys.Date(), sep = " "),"","","","")
  #     
  #     df <- county.rt() %>% as.data.frame()
  #     if ( ncol(df) > 2 ) { df[,2:ncol(df)] <- lapply(df[,2:ncol(df)],function(x) round(x,2)) } else { df[,2] <- round(df[,2],2)  } 
  #     df[is.na(df)] <- 0
  #     df[] <- lapply(df, as.character)
  #     
  #     #Column labels
  #     
  #     l <- c("Date","COVIDActNow")
  #     
  #     if ( c %in% unique(gu.cnty$subregion) ) {  l <- c(l, c("covid19-projections.com")) }
  #     if ( c %in% unique(ucla_cnty_rt$county) ) {  l <- c(l, c("UCLA")) }
  #     if ( length(l) > 2 ) { l <- c(l, c("Mean Reff") ) }
  #     
  #     #Source
  #     s <- c("Please see the Technical Notes tab of the application for data sources.","","","","")
  #     p <- c(paste0("Prepared by: ",state_name," Department of Public Health"),"","","","")
  #     
  #     dlm <- rbind(t, tt, l, df, s, p)
  #     write.table(dlm, file, row.names = F, col.names = F, quote = F, na= "NA", sep = ",")
  #   })   
  # 
  
  
  #Download file of ALL COUNTY 7-day average Reff Values
  # output$dlRt.cnty <- downloadHandler(
  #   
  #   filename = function() { paste("Rt_Nowcasts_7DayAvg_Counties",Sys.Date(),'.csv', sep='') },
  #   
  #   content = function(file) {
  #     
  #     # Title
  #     t <- c(paste("R-Effective 7 Day Averages for Counties", sep = ""),"","","","")
  #     #Subtitle
  #     tt <-  c(paste("COVID Assessment Tool - Downloaded on",Sys.Date(), sep = " "),"","","","")
  #     
  #     df <- cnty.7.day.rt %>% as.data.frame()
  #     if ( ncol(df) > 2 ) { df[,2:ncol(df)] <- lapply(df[,2:ncol(df)],function(x) round(x,2)) } else { df[,2] <- round(df[,2],2)  } 
  #     df[is.na(df)] <- 0
  #     df[] <- lapply(df, as.character)
  #     
  #     #Column labels
  #     
  #     l <- c("County","COVIDActNow - 7 Day Avg", "LL", "UL")
  #     
  #     #Source
  #     s <- c("Please see the Technical Notes tab of the application.","","","","")
  #     p <- c(paste0("Prepared by: ",state_name," Department of Public Health"),"","","","")
  #     u <- c("Source: COVIDActNow - https://blog.covidactnow.org/modeling-metrics-critical-to-reopen-safely/","","","","")
  #     
  #     dlm <- rbind(t, tt, l, df, s, p, u)
  #     write.table(dlm, file, row.names = F, col.names = F, quote = F, na= "NA", sep = ",")
  #   })   
  # 
  # 
  
  #### Hospitalization Projections ####
  
  #Data Prep
  hosp.proj.ts <- reactive({
    
    #Select data
    min_hosp <- min(forecasts$date)
    hosp_actuals <- covid_actuals %>% select(date, weekly_admits) %>% mutate(date=as.Date(date, format="%Y-%m-%d")) %>% filter(lubridate::wday(date, label=TRUE)=="Sat")
    hosp_forecasts <- forecasts %>% select(date,hosp_admits, model) %>% filter(forecasts$state == state_name) %>% as.data.frame() %>% filter(lubridate::wday(date, label=TRUE)=="Sat")
    
    #Filter data for plot display
    arima.hosp.proj <- hosp_forecasts %>% filter(model=="arima") %>% filter(min_hosp <= date & date <= Sys.Date() + 30)
    holts.hosp.proj <- hosp_forecasts %>% filter(model=="holts") %>% filter(min_hosp <= date & date <= Sys.Date() + 30)
    dampedholts.hosp.proj <- hosp_forecasts %>% filter(model=="dampedholts") %>% filter(min_hosp <= date & date <= Sys.Date() + 30)
    nnetar.hosp.proj <- hosp_forecasts %>% filter(model=="nnetar") %>% filter(min_hosp <= date & date <= Sys.Date() + 30)
    
    #Combine projections and calculate median
    ##Combine all forecast model estimate rows
    median.proj <- rbind(arima.hosp.proj, holts.hosp.proj, dampedholts.hosp.proj, nnetar.hosp.proj)
    ##Group by forecast date and calculate median for each date
    median.proj <- median.proj %>% 
      group_by(date) %>% 
      mutate(median=median(hosp_admits), model="median") %>% 
      filter(row_number()==1) %>% 
      ungroup() %>% 
      select(date, median, model)
    ##Convert to dataframe for conversion to time series object (xts)
    median.proj <- as.data.frame(median.proj)
    
    #Convert to time series objects
    covid.xts <- xts(hosp_actuals[, c("weekly_admits")], as.Date(hosp_actuals$date, format="%Y-%m-%d"))
    arima.xts <- xts(arima.hosp.proj[, c("hosp_admits")], arima.hosp.proj$date)
    holts.xts <- xts(holts.hosp.proj[, c("hosp_admits")], holts.hosp.proj$date)
    dampedholts.xts <- xts(dampedholts.hosp.proj[, c("hosp_admits")], dampedholts.hosp.proj$date)
    nnetar.xts <- xts(nnetar.hosp.proj[, c("hosp_admits")], nnetar.hosp.proj$date)
    median.xts <- xts(median.proj[, c("median")], median.proj$date)
    
    #Merge datasets
    df <- merge(covid.xts, arima.xts, holts.xts, dampedholts.xts, nnetar.xts, median.xts)
    # df <- merge(hosp_actuals, arima.hosp.proj, holts.hosp.proj, dampedholts.hosp.proj, nnetar.hosp.proj)
    # df$mean.proj <- rowMeans(df[, c("arima.xts", "holts.xts", "dampedholts.xts", "nnetar.xts")], na.rm=T)
    # df$mean.proj <- ifelse(!is.na(df$covid.xts), NA, df$mean.proj) #comment out so ensemble line is aligned
    
    #Line types for plotly plots
    df$period <- ifelse(!is.na(df$covid.xts), "solid", "dot")
    df$type <- ifelse(!is.na(df$covid.xts), "Est.", "Proj.")
    df <- as.data.frame(df)
    df$date <- as.Date(row.names(df), format="%Y-%m-%d")
    
    # can.hosp.proj <- can.state.observed %>% select(date, hospitalBedsRequired) %>% filter(min_hosp <= date & date <= Sys.Date() + 30)
    # IHME.hosp.proj <- IHME %>% select(date, allbed_mean) %>% filter(min_hosp <= date & date <= Sys.Date() + 30)
    # mobs.hosp.proj <- mobs %>% select(2,8) %>% filter(min_hosp <= date & date <= Sys.Date() + 30)
    # mit.hosp.proj <- mit %>% select(11,7) %>% filter(min_hosp <= date & date <= Sys.Date() + 30)
    
    # covid.xts <- xts(hosp[,-1],hosp$Most.Recent.Date)
    # can.proj.xts <- xts(can.hosp.proj[,-1],can.hosp.proj$date)
    # ihme.proj.xts <- xts(IHME.hosp.proj[,-1],IHME.hosp.proj$date)
    # mobs.proj.xts <- xts(mobs.hosp.proj[,-1],mobs.hosp.proj$date)
    # mit.proj.xts <- xts(mit.hosp.proj[,-1],mit.hosp.proj$date)
    
    # df <- merge(covid.xts,can.proj.xts,ihme.proj.xts,mobs.proj.xts,mit.proj.xts)
    # 
    # df$mean.proj <- rowMeans(df[,2:5], na.rm = TRUE)
    # df$mean.proj <- ifelse(!is.na(df$covid.xts), NA, df$mean.proj)
    # 
    # df <- as.data.table(df) %>% as.data.frame()
    return(df)
     
  })
  
  #Value Boxes
  output$actual.hosp.box <- renderValueBox({
    
    cdt <- max(covid_actuals$date)
    current.hosp <- covid_actuals[which(covid_actuals$date==cdt), "weekly_admits"]
    # current.hosp <- as.character(covid_actuals[which(covid_actuals$date == cdt & covid_actuals$state == state_name),"hosp_admits"])
    valueBox(# "Actuals Go Here",
             format(as.numeric(current.hosp), big.mark = ","),
             paste0("Actuals: ",cdt), color = "black")
    
  })
  
  output$mean.proj.hosp.box <- renderUI({

    cdt <- max(forecasts[which(forecasts$date <= Sys.Date()+30),]$date)
    # cdt.ihme <- max( IHME[which(IHME$date <= Sys.Date() + 30),]$date  )
    mean.proj <-  hosp.proj.ts() %>% slice(n()) %>% select(6)
    valueBox(format(round(mean.proj, digits = 0), big.mark = ","), paste0("Mean 30-day forecast through ", cdt), color = "blue", width = 12)

  })
  
  #Graphs
  output$hosp.proj.plot <- renderPlotly({
    
    df <- hosp.proj.ts()  
    
    cdt <- max(covid_actuals$date)
    
    p <-  plot_ly(df,
                  hoverinfo = 'text') %>%
      add_trace(x=df$date,
                y = df$covid.xts,
                name = "Actuals",
                type = 'scatter',
                mode = "lines+markers", 
                hoverinfo = 'text',
                text = paste0(df$date,
                              "<br>",
                              "Actual Hospitalization: ", format(round(df$covid.xts,0), big.mark = ",") ),
                line = list(color = "black"),
                marker = list(color = "black",  symbol= "circle")) %>%
      add_trace(x = df$date, 
                y = df$arima.xts, 
                name = "ARIMA",
                type = 'scatter',
                mode = "lines", 
                inherit = TRUE,
                line = list(color="orange"),
                linetype = I("dot"),
                hoverinfo = 'text',
                text = paste0(df$date,
                              "<br>",
                              "ARIMA Estimate: ", format(round(df$arima.xts,0), big.mark = ","))) %>%
      add_trace(x = df$date, 
                y = df$holts.xts, 
                name = "Holts",
                type = 'scatter',
                mode = "lines", 
                inherit = TRUE,
                line = list(color="navy"),
                linetype = I("dot"),
                hoverinfo = 'text',
                text = paste0(df$date,
                              "<br>",
                              "Holts Estimate: ", format(round(df$holts.xts,0), big.mark = ","))) %>%
      add_trace(x = df$date, 
                y = df$dampedholts.xts, 
                name = "Damped Holts",
                type = 'scatter',
                mode = "lines", 
                inherit = TRUE,
                line = list(color="red"),
                linetype = I("dot"),
                hoverinfo = 'text',
                text = paste0(df$date,
                              "<br>",
                              "Damped Holts Estimate: ", format(round(df$nnetar.xts,0), big.mark = ","))) %>%
      add_trace(x = df$date,
                y = df$nnetar.xts,
                name = "NNETAR",
                type = 'scatter',
                mode = "lines",
                inherit = TRUE,
                line = list(color="green"),
                linetype = I("dot"),
                hoverinfo = 'text',
                text = paste0(df$date,
                              "<br>",
                              "NNETAR Estimate: ", format(round(df$nnetar.xts,0), big.mark = ","))) %>%
      add_trace(x = df$date,
                y = df$median.xts,
                name = "Ensemble",
                type = 'scatter',
                mode = "lines",
                inherit = TRUE,
                linetype = I("solid"),
                hoverinfo = 'text',
                text = paste0(df$date,
                              "<br>",
                              "Ensemble Projection: ", format(round(df$median.xts,0), big.mark = ",") ),
                line = list(color = '#2b8cbe', width = 5)
      ) %>%
    layout(
      title = NULL,
      xaxis = list(title = NULL, showline = TRUE, showgrid = FALSE, zeroline = FALSE),
      yaxis = list(title = "Weekly COVID-19 Hospital Admissions", showline = TRUE, showgrid = FALSE, zeroline = FALSE ),
      margin = list(l = 100),
      showlegend = TRUE,
      shapes = list(type = "line", 
                    y0 = 0, 
                    y1 = 1, 
                    yref = "paper",
                    x0 = cdt, 
                    x1 = cdt, 
                    line = list(color = "black", dash = 'dash')
      )
    ) 
    return(p)
    
  })
  
  #Download file of Statewide Hospitalization Forecasts
  output$dlhosp <- downloadHandler(
    
    filename = function() { paste("Hospital_Forecasts_",Sys.Date(),'.csv', sep='') },
    
    content = function(file) {
      
      # Title
      t <- c(paste("Statewide Hospitalization Forecasts", sep = ""),"","","","","","")
      #Subtitle
      tt <-  c(paste("Communicable diseases Assessment Tool - Downloaded on",Sys.Date(), sep = " "),"","","","","","")
      #Column labels
      l <- c("Date","Actuals", "ARIMA","Holts","Damped Holts","NNETAR","Ensemble")
      
      df <- hosp.proj.ts()%>% as.data.frame()
      df <- df %>% select(date, names(df)[1:6])
      df[,2:7] <- lapply(df[,2:7],function(x) round(x,2))
      df[is.na(df)] <- 0
      df[] <- lapply(df, as.character)
      
      #Source
      s <- c("Please see the Technical Notes tab of the application for data sources.","","","","","","")
      p <- c("Prepared by: California Department of Public Health - COVID Modeling Team","","","","","","")
      
      dlm <- rbind(t, tt, l, df, s, p)
      write.table(dlm, file, row.names = F, col.names = F, quote = F, na= "NA", sep = ",")
    })   
  
  
  
  #### County Hospitalization Projections ####
  
  #Data Prep
  # county.hosp <- reactive({
  #   progress <- Progress$new()
  #   # Make sure it closes when we exit this reactive, even if there's an error
  #   on.exit(progress$close())
  #   progress$set(message = "Gathering Hospitalization Forecasts", value = 0)
  #   
  #   cnty <- input$select.county.hosp
  #   progress$inc(3/4)
  #   # out <- lapply(cnty[1], function(x) get_can_cnty(x))
  #   # cnty.hosp <- do.call("rbind",out)
  #   out <- filter(can.county.observed, fips ==  cnty)
  #   
  #   cnty.hosp <- out %>% select(date,hospitalBedsRequired) %>% as.data.frame()
  #   progress$inc(1/4)
  #   return(cnty.hosp)
  #   
  # })
  # 
  ##################################
  ##### COUNTS OF COUNTY BEDS  #####
  #####     Add to global      #####
  ##################################
  
  # fc.cnty.beds <- reactive({
  #   c <- names(canfipslist[match(input$select.county.hosp,canfipslist)])
  #   
  #   if (c %in% cnty.beds[,1] == TRUE) {
  #     beds <- c(cnty.beds[which(cnty.beds$COUNTY == c),9])
  #   } else {
  #     beds <- c(NA)
  #   }
  # })
  
  
  # hosp.proj.cnty.ts <- reactive({
  #   
  #   c <- names(canfipslist[match(input$select.county.hosp,canfipslist)])
  #   min_hosp <- min(covid$Most.Recent.Date)
  #   hosp <- covid %>% select(Most.Recent.Date,COVID.19.Positive.Patients) %>% filter(covid$County.Name == c) %>% as.data.frame()
  #   can.hosp.proj <- county.hosp() %>% select(date, hospitalBedsRequired) %>% filter(min_hosp <= date & date <= Sys.Date() + 30)
  #   
  #   covid.xts <- xts(hosp[,-1],hosp$Most.Recent.Date)
  #   can.proj.xts <- xts(can.hosp.proj[,-1],can.hosp.proj$date)
  #   
  #   df <- merge(covid.xts,can.proj.xts)
  #   
  #   df <- as.data.table(df) %>% as.data.frame()
  #   
  #   df$period <- ifelse(!is.na(df$covid.xts), "solid", "dot")
  #   return(df)
  #   
  # })
  # 
  # #Value Boxes
  # output$actual.cnty.hosp.box <- renderValueBox({
  #   c <- names(canfipslist[match(input$select.county.hosp,canfipslist)])
  #   cdt <- max(covid$Most.Recent.Date)
  #   current.hosp <- as.character(covid[which(covid$Most.Recent.Date == cdt & covid$County.Name == c),"COVID.19.Positive.Patients"])
  #   valueBox( "Counts/Beds Here",
  #             #paste0(format(as.numeric(current.hosp), big.mark = ","),"/",
  #             #       #format(as.numeric(fc.cnty.beds()), big.mark = ",") 
  #             #       ), 
  #             paste0("Actuals / Total Beds: ",cdt), 
  #             color = "black")
  # })
  # 
  # output$mean.cnty.proj.hosp.box <- renderValueBox({ 
  #   
  #   cdt.ihme <- max( IHME[which(IHME$date <= Sys.Date() + 30),]$date  )
  #   mean.proj <-  hosp.proj.cnty.ts() %>% slice(n()) %>% select(3)
  #   valueBox( format(round(mean.proj, digits = 0), big.mark = ","), 
  #             paste0("30-Day Forecast through ", cdt.ihme), color = "blue")
  #   
  # })
  # 
  # #Graph
  # output$county.hosp.plot <- renderPlotly({
  #   
  #   df <- hosp.proj.cnty.ts()  
  #   
  #   cdt <- max(df[which(!is.na(df$covid.xts)),1])
  #   
  #   today <- list(type = "line", 
  #                 y0 = 0, 
  #                 y1 = 1, 
  #                 yref = "paper",
  #                 x0 = cdt, 
  #                 x1 = cdt, 
  #                 line = list(color = "black", dash = 'dash') )
  #   
  #   p <-  plot_ly(df,
  #                 hoverinfo = 'text') %>%
  #     add_trace(x = df[[1]], 
  #               y = df[[2]], 
  #               name = "Actuals",
  #               type = 'scatter',
  #               mode = "lines+markers", 
  #               hoverinfo = 'text',
  #               text = paste0(df[[1]],
  #                             "<br>",
  #                             "Actual Hospitalization (PLACEHOLDER DATA - PLEASE REPLACE!!): ", format(df[[2]], big.mark = ",") ),
  #               line = list(color = "black"),
  #               marker = list(color = "black",  symbol= "circle")
  #     ) %>%
  #     add_trace(x = df[[1]], 
  #               y = df[[3]], 
  #               name = "COVIDActNow - Proj.",
  #               type = 'scatter',
  #               mode = "lines", 
  #               inherit = TRUE,
  #               line = list(color="orange"),
  #               linetype = ~I(period),
  #               hoverinfo = 'text',
  #               text = paste0(df[[1]],
  #                             "<br>",
  #                             "COVIDActNow Estimate: ", format(df[[3]], big.mark = ",") )
  #               
  #     ) %>%
  #     layout(
  #       title = as.character(counties[match(input$select.county.hosp, counties$fips),"county"]),
  #       xaxis = list(title = NULL, showline = TRUE, showgrid = FALSE, zeroline = FALSE ),
  #       yaxis = list(title = "Hospitalziations", showline = TRUE, showgrid = FALSE, zeroline = FALSE),
  #       margin = list(l = 100),
  #       showlegend = TRUE, 
  #       shapes = list(today)
  #     ) 
  #   return(p)
  #   
  # })
  # 
  # #Download file of COUNTY Hospitalization Forecasts
  # output$dlhosp.cnty <- downloadHandler(
  #   
  #   filename = function() { paste("Hospital_Forecasts_for_",names(canfipslist[match(input$select.county.hosp,canfipslist)]),Sys.Date(),'.csv', sep='') },
  #   
  #   content = function(file) {
  #     
  #     c <- names(canfipslist[match(input$select.county.hosp,canfipslist)])
  #     # Title
  #     t <- c(paste("Hospitalization Forecasts for ",c, sep = ""),"","","","","","")
  #     #Subtitle
  #     tt <-  c(paste("COVID Assessment Tool - Downloaded on",Sys.Date(), sep = " "),"","","","","","")
  #     #Column labels
  #     l <- c("Date","Actuals", "COVIDActNow")
  #     
  #     df <-  hosp.proj.cnty.ts()[,1:3] %>% as.data.frame()
  #     df[,2:3] <- lapply(df[,2:3],function(x) round(x,2))
  #     df[is.na(df)] <- 0
  #     df[] <- lapply(df, as.character)
  #     
  #     #Source
  #     s <- c("Please see the Technical Notes tab of the application for data sources.","","","","","","")
  #     p <- c(paste0("Prepared by: ",state_name," Department of Public Health"),"","","","","","")
  #     
  #     dlm <- rbind(t, tt, l, df, s, p)
  #     write.table(dlm, file, row.names = F, col.names = F, quote = F, na= "NA", sep = ",")
  #   })   
  

} # End Server
