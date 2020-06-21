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
    
    icl_rt_f <- icl %>% select(date, constant_mobility_mean_time_varying_reproduction_number_R.t.) %>% rename(mean_rt = constant_mobility_mean_time_varying_reproduction_number_R.t.)
    icl_rt <- icl_model %>% select(date, mean_time_varying_reproduction_number_R.t.) %>% rename(mean_rt = mean_time_varying_reproduction_number_R.t.) 
    icl_rt <- rbind(icl_rt, icl_rt_f)
    
    fu <- filter(yu, !is.na(r_values_mean))
    
    rt.rt.xts <- xts(rt_live[,4], rt_live$date)
    
    can.rt.xts <- xts(can.state.observed[,8],can.state.observed$date)
    epifc.rt.xts <- xts(epi_forecast[which(epi_forecast$type == "nowcast"),4], 
                        epi_forecast[which(epi_forecast$type == "nowcast"),]$date)
    yu.xts <- xts(fu[,19],fu$date)
    ucla.rt.xts <- xts(ucla_state[,2],ucla_state$date)
    ucla.rt.xts <- ucla.rt.xts[paste0("/",Sys.Date()-1)]
    icl.rt.xts <- xts(icl_rt[,2], icl_rt$date) 
    
    df <- merge(rt.rt.xts, can.rt.xts,epifc.rt.xts, yu.xts, ucla.rt.xts, icl.rt.xts)
    
    df$mean.rt <- rowMeans(df[,c(1:4,6)], na.rm = TRUE)
    df[is.nan(as.numeric(df))] <- NA_character_
    df <- as.data.table(df) %>% as.data.frame()
    df[,2:8] <- sapply(df[,2:8], function(x) as.numeric(as.character(x)) )
    return(df)
    
  })
  
  #Value Boxes
  output$mean.rt.box <- renderValueBox({
    cdt <- Sys.Date()-1 
    current.rt <- round(rt.ts()[which(rt.ts()$index == cdt),8], digits = 2)
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
      title = "What does a Reff of this size mean?",
      text = HTML("<p>If the R effective is greater than 1, COVID-19 will spread <b>exponentially</b>. If R effective is less than 1, COVID-19 
                         will spread more slowly and cases will decline. The higher the value of R effective, the faster an epidemic will progress.
                         The following graph illustrates the change in growth as R effective increases.</p>
                         <img src='reff_cuml_infection.png' alt='Infections increase faster with larger values of R effective' width='400' height='400'/>
                         <p><a href='https://www.cebm.net/covid-19/when-will-it-be-over-an-introduction-to-viral-reproduction-numbers-r0-and-re/' target='_blank'>Source: CEBM</a></p>"
      ),
      html = TRUE,
      type = NULL
    )
  })
  
  output$hilo_rt.box <- renderUI({
    
    df <- rt.ts()
    df <- df %>% filter(index < Sys.Date()) %>% slice(n())
    
    rt.min <- as.numeric( apply(df[,c(2:5,7)], 1, function(i) min(i, na.rm = TRUE)) )
    rt.max <- as.numeric( apply(df[,c(2:5,7)], 1, function(i) max(i, na.rm = TRUE)) )
    
     name.min <- switch(as.character(colnames(df)[match(apply(df[,c(2:5,7)], 1, function(i) min(i, na.rm = TRUE)),df)]),
                         "rt.rt.xts" = "rt.live",
                         "can.rt.xts" = "COVIDActNow",
                         "epifc.rt.xts" = "EpiForecasts",
                         "yu.xts" = "covid19-projections.com",
                         "ucla.rt.xts" = "UCLA",
                         "icl.rt.xts" = "ICL")
        
        name.max<- switch(as.character(colnames(df)[match(apply(df[,c(2:5,7)], 1, function(i) max(i, na.rm = TRUE)),df)]),
                           "rt.rt.xts" = "rt.live",
                           "can.rt.xts" = "COVIDActNow",
                           "epifc.rt.xts" = "EpiForecasts",
                           "yu.xts" = "covid19-projections.com",
                           "ucla.rt.xts" = "UCLA",
                           "icl.rt.xts" = "ICL")
    
    tagList(valueBox( paste0( round(rt.min,digits = 2)," - ", round(rt.max,digits = 2)) , paste0(name.min," - ",name.max), color = "navy", width = 12) )
    
  })
  
  #Graph
  output$rt.plot <- renderPlotly({
    
    df <- rt.ts() %>% filter(index < Sys.Date() & index > Sys.Date() -80)
    
    p <-  plot_ly(df,
                  hoverinfo = 'text') %>%
      add_trace(x = df[[1]], 
                y = df[[2]], 
                name = "rt.live",
                type = 'scatter',
                mode = "lines",
                line = list(color="orange", dash = 'dot', opacity = 0.5),
                text = paste0(df[[1]],
                              "<br>",
                              "rt.live estimated Reff: ", round(df[[2]], digits=2)
                )
      ) %>%
      add_trace(x = df[[1]], 
                y = df[[3]], 
                name = "COVIDActNow",
                type = 'scatter',
                mode = "lines", 
                line = list(color="blue", dash = 'dot', opacity = 0.5),
                hoverinfo = 'text',
                text = paste0(df[[1]],
                              "<br>",
                              "COVIDActNow estimated Reff: ", round(df[[3]], digits=2) )
      ) %>%
      add_trace(x = df[[1]], 
                y = df[[4]], 
                name = "EpiForecasts",
                type = 'scatter',
                mode = "lines", 
                line = list(color="purple", dash = 'dot', opacity = 0.5),
                hoverinfo = 'text',
                text = paste0(df[[1]],
                              "<br>",
                              "EpiForecasts estimated Reff: ", round(df[[4]], digits=2) )
      ) %>%
      add_trace(x = df[[1]], 
                y = df[[5]], 
                name = "covid19-projections.com",
                type = 'scatter',
                mode = "lines", 
                line = list(color="red", dash = 'dot', opacity = 0.5),
                hoverinfo = 'text',
                text = paste0(df[[1]],
                              "<br>",
                              "covid19-projections.com estimated Reff: ", round(df[[5]], digits=2) )
      ) %>%
      # add_trace(x = df[[1]], 
      #           y = df[[6]], 
      #           name = "UCLA",
      #           type = 'scatter',
      #           mode = "lines", 
      #           line = list(color="grey", dash = 'dot', opacity = 0.5),
      #           hoverinfo = 'text',
      #           text = paste0("UCLA estimated Reff: ", round(df[[6]], digits=2) )
      #           #marker = list(color = "blue",  symbol= "circle")
      # ) %>%
      add_trace(x = df[[1]],
                y = df[[7]],
                name = "ICL",
                type = 'scatter',
                mode = "lines",
                line = list(color="grey", dash = 'dot', opacity = 0.5),
                hoverinfo = 'text',
                text = paste0(df[[1]],
                              "<br>",
                              "Imperial College London estimated Reff: ", round(df[[7]], digits=2) )
      ) %>%
      add_trace(x = df[[1]], 
                y = df[[8]], 
                name = "Mean Reff",
                type = 'scatter',
                mode = "lines", 
                hoverinfo = 'text',
                line = list(color = '#2b8cbe', width = 5),
                text = paste0(df[[1]],
                              "<br>",
                              "Mean estimated Reff: ", round(df[[8]], digits=2), 
                              "<br>",
                              ifelse(round(df[[8]], digits=2) >= 1.4,
                                     "Spread of COVID-19 is very likely increasing",
                                     ifelse(round(df[[8]], digits=2) < 1.4 & round(df[[8]], digits=2) >= 1.1,
                                            "Spread of COVID-19 may be increasing",
                                            ifelse(round(df[[8]], digits=2) < 1.1 & round(df[[8]], digits=2) >= 0.9,
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
      tt <-  c(paste("COVID Assessment Tool - Downloaded on",Sys.Date(), sep = " "),"","","","","","")
      #Column labels
      l <- c("Date","rt.live","COVIDActNow","EpiForecasts","covid19-projections.com","ICL","Mean Reff")
      
      df <- rt.ts()[,c(1:5,7,8)] %>% filter(index < Sys.Date() & index > Sys.Date() -80)
      df[,2:7] <- lapply(df[,2:7],function(x) round(x,2))
      # df[is.na(df)] <- 0
      df[] <- lapply(df, as.character)
      
      #Source
      s <- c("Please see the Technical Notes tab of the application for data sources.","","","","","","")
      p <- c(paste0("Prepared by: ",state_name," Department of Public Health"),"","","","","","")
      
      dlm <- rbind(t, tt, l, df, s, p)
      write.table(dlm, file, row.names = F, col.names = F, quote = F, na= "NA", sep = ",")
    })   
  
  
  #### County Rt Nowcasts ####
  
  #Data Prep
  county.rt <- reactive({
    progress <- Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Gathering R Effective Nowcasts", value = 0)
    
    c <- names(canfipslist[match(input$select.county.rt,canfipslist)])
    cnty <- input$select.county.rt
    
    progress$inc(3/4)
    # out <- lapply(cnty[1], function(x) get_can_cnty(x))
    out <- filter(can.county.observed, fips ==  cnty)
    
    # cnty.rt <- do.call("rbind",out)
    cnty.rt <- out %>% select(date,RtIndicator) %>% as.data.frame() #,RtIndicatorCI90
    cnty.rt$date <- as.Date(cnty.rt$date)
    progress$inc(1/4)
    
    df <- xts(cnty.rt[,-1],cnty.rt$date)
    if (c %in% unique(yu.cnty$subregion)   ) { cnty.yu <- yu.cnty %>% filter(subregion == c) %>% select(date, r_values_mean) 
    yu.xts <- xts(cnty.yu[,-1],cnty.yu$date)
    df <- merge(df,yu.xts)
    }
    
    # if (c %in% unique(ucla_cnty_rt$county) ) { cnty.ucla <- ucla_cnty_rt %>% filter(county == c) %>% select(date, Rt)       
    # ucla.xts <- xts(cnty.ucla[,-1],cnty.ucla$date)
    # df <- merge(df,ucla.xts)
    # }
    
    if (ncol(df) > 1) {df$mean.proj <- rowMeans(df[,1:ncol(df)], na.rm = TRUE)} 
    
    df <- as.data.table(df) %>% as.data.frame() %>% filter(index < Sys.Date())
    
    return(df)
    
  })
  
  #Graph
  output$county.rt.plot <- renderPlotly({
    
    df <- county.rt()
    
    c <- names(canfipslist[match(input$select.county.rt,canfipslist)])
    
    #df$ymin <- df$RtIndicator - (df$RtIndicatorCI90)
    #df$ymax <- df$RtIndicator + (df$RtIndicatorCI90)
    
    p <-  plot_ly(df,
                  x = df[[1]], 
                  y = df[[2]], 
                  name = "COVIDActNow",
                  type = 'scatter',
                  mode = "lines", 
                  line = list(color="blue", dash = 'dot', opacity = 0.5),
                  hoverinfo = 'text',
                  text = paste0(df[[1]],
                                "<br>",
                                "COVIDActNow estimated Reff: ", round(df[[2]], digits=2) )
    )
    if (c %in% unique(yu.cnty$subregion)   ) {p <- p %>% add_trace(x = df[[1]], 
                                                                   y = df[["yu.xts"]], 
                                                                   name = "covid19-projections.com",
                                                                   type = 'scatter',
                                                                   mode = "lines", 
                                                                   line = list(color="red", dash = 'dot', opacity = 0.5),
                                                                   hoverinfo = 'text',
                                                                   text = paste0(df[[1]],
                                                                                 "<br>",
                                                                                 "covid19-projections.com estimated Reff: ", round(df[["yu.xts"]], digits=2) )
    ) 
    }
    # if (c %in% unique(ucla_cnty_rt$county) ) {p <- p %>% add_trace(x = df[[1]], 
    #                                                                y = df[["ucla.xts"]], 
    #                                                                name = "UCLA",
    #                                                                type = 'scatter',
    #                                                                mode = "lines", 
    #                                                                line = list(color="grey", dash = 'dot', opacity = 0.5),
    #                                                                hoverinfo = 'text',
    #                                                                text = paste0(df[[1]],
    #                                                                              "<br>",
    #                                                                              "UCLA estimated Reff: ", round(df[["ucla.xts"]], digits=2) )
    # )
    # }
    if (ncol(df) > 2) {p <- p %>% add_trace(x = df[[1]], 
                                            y = df[["mean.proj"]], 
                                            name = "Mean Reff",
                                            type = 'scatter',
                                            mode = "lines", 
                                            hoverinfo = 'text',
                                            text = paste0(df[[1]],
                                                          "<br>",
                                                          "Mean estimated Reff: ", round(df[["mean.proj"]], digits=2),
                                                          "<br>",
                                                          ifelse(round(df[["mean.proj"]], digits=2) >= 1.4,
                                                                 "Spread of COVID-19 is very likely increasing",
                                                                 ifelse(round(df[["mean.proj"]], digits=2) < 1.4 & round(df[["mean.proj"]], digits=2) >= 1.1,
                                                                        "Spread of COVID-19 may be increasing",
                                                                        ifelse(round(df[["mean.proj"]], digits=2) < 1.1 & round(df[["mean.proj"]], digits=2) >= 0.9,
                                                                               "Spread of COVID-19 is likely stable",
                                                                               "Spread of COVID-19 is likely decreasing"
                                                                        )
                                                                 )
                                                          )
                                            ),
                                            inherit = FALSE,
                                            line = list(color = '#2b8cbe', width = 5),
                                            linetype = "solid"
    )
    }
    # add_ribbons(x = df[[1]],
    #             ymax =  df[[5]],
    #             ymin =  df[[4]],
    #             opacity = 0.5,
    #             inherit = TRUE,
    #             line = list(color = '#2b8cbe' ),
    #             fillcolor = '#2b8cbe',
    #             name = '90% CI'
    # ) %>%
    p <- p %>% layout(  legend = list(orientation = 'h'),
                        title = as.character(counties[match(input$select.county.rt, counties$fips),"county"]),
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
                          line = list(color = "gray50", dash= "dash", opacity = 0.3)
                        )
    ) 
    
    return(p)
    
  })
  
  #Download file of individual COUNTY Reff Values
  output$dlRt.indv.cnty <- downloadHandler(
    
    filename = function() { paste("Rt_Nowcasts_",names(canfipslist[match(input$select.county.rt,canfipslist)]),"_",Sys.Date(),'.csv', sep='') },
    
    content = function(file) {
      
      c <- names(canfipslist[match(input$select.county.rt,canfipslist)])
      # Title
      t <- c(paste("R-Effective County Model Time Series for ",c, sep = ""),"","","","")
      #Subtitle
      tt <-  c(paste("COVID Assessment Tool - Downloaded on",Sys.Date(), sep = " "),"","","","")
      
      df <- county.rt() %>% as.data.frame()
      if ( ncol(df) > 2 ) { df[,2:ncol(df)] <- lapply(df[,2:ncol(df)],function(x) round(x,2)) } else { df[,2] <- round(df[,2],2)  } 
      df[is.na(df)] <- 0
      df[] <- lapply(df, as.character)
      
      #Column labels
      
      l <- c("Date","COVIDActNow")
      
      if ( c %in% unique(yu.cnty$subregion) ) {  l <- c(l, c("covid19-projections.com")) }
      if ( c %in% unique(ucla_cnty_rt$county) ) {  l <- c(l, c("UCLA")) }
      if ( length(l) > 2 ) { l <- c(l, c("Mean Reff") ) }
      
      #Source
      s <- c("Please see the Technical Notes tab of the application for data sources.","","","","")
      p <- c(paste0("Prepared by: ",state_name," Department of Public Health"),"","","","")
      
      dlm <- rbind(t, tt, l, df, s, p)
      write.table(dlm, file, row.names = F, col.names = F, quote = F, na= "NA", sep = ",")
    })   
  
  #### Rt Dot Plot ####
  
  #Data Prep
  cnty.7.day.rt <- data.table(can.county.observed) %>%
    .[, max_date := max(date, na.rm = T), by = .(county)] %>%
    .[date > Sys.Date()-7, .(Rt.m = mean(RtIndicator, na.rm = T),
                             ll = mean(RtIndicator - RtIndicatorCI90, na.rm=T),
                             ul = mean(RtIndicator + RtIndicatorCI90, na.rm=T)), by = .(county)] %>% na.omit()
  
  # cnty.7.day.rt <- reactive({
  #     
  #                 cnty.can <- can.county.observed %>% filter(date <= Sys.Date()-1,
  #                                                       date > Sys.Date()-8) %>% 
  #                     select(county, date, RtIndicator) %>% 
  #                     mutate(date = as.Date(date)) %>% 
  #                     rename(Rt = RtIndicator) %>%
  #                     as.data.frame()
  #                 cnty.yu <- yu.cnty %>% filter(date <= Sys.Date()-1,
  #                                               date >  Sys.Date()-8) %>% 
  #                                         select(subregion, date, r_values_mean) %>% 
  #                                         rename(county = subregion,
  #                                                Rt = r_values_mean )
  #                 cnty.ucla <- ucla_cnty_rt %>% filter(date <= Sys.Date()-1,
  #                                                      date >  Sys.Date()-8) %>% 
  #                                               select(county, date, Rt) 
  #                 
  #                 df <- rbind(cnty.can,cnty.yu,cnty.ucla) %>% 
  #                         arrange(county,date) %>%
  #                         group_by(county) %>%
  #                         summarise(Rt.m = mean(Rt, na.rm = T),
  #                                   Rt.sd = sd(Rt, na.rm = T) ) %>% 
  #                         na.omit() %>%
  #                         mutate(ll = Rt.m  - 1.95*Rt.sd,
  #                                ul = Rt.m  + 1.95*Rt.sd)
  #                 return(df)
  # 
  # })
  # 
  #Graph
  output$rt.dot.plot <- renderPlotly({
    
    df <- cnty.7.day.rt
    
    p <-  plot_ly(df,
                  x = ~ reorder(df$county, desc(df$Rt.m)), 
                  y = ~ df$Rt.m, 
                  name = "R effective",
                  type = 'scatter',
                  mode = "markers", 
                  marker = list(color = '#2b8cbe'),
                  hoverinfo = 'text',
                  text = ~paste0(df$county, " County<br>","7-day Average Reff: ", round(df$Rt.m, digits=2), 
                                 "<br>",
                                 ifelse(df$Rt.m >= 1.4,
                                        "Spread of COVID-19 is very likely increasing",
                                        ifelse(df$Rt.m < 1.4 & df$Rt.m >= 1.1,
                                               "Spread of COVID-19 may be increasing",
                                               ifelse(df$Rt.m < 1.1 & df$Rt.m >= 0.9,
                                                      "Spread of COVID-19 is likely stable",
                                                      "Spread of COVID-19 is likely decreasing"
                                               )
                                        )
                                 )
                  )
    ) %>%
      add_segments(x =~ reorder(df$county, df$Rt.m), 
                   xend = ~ reorder(df$county, df$Rt.m), 
                   y = df$ll, 
                   yend = df$ul, 
                   type = "scatter",
                   mode = "lines",
                   opacity = .5,
                   line = list(color='#2b8cbe', width = 6),
                   showlegend = FALSE
      ) %>%
      layout(
        xaxis = list(title = "", tickangle = -30, showgrid = FALSE, zeroline = FALSE ),
        yaxis = list(title = "R-Eff", showline = TRUE, showgrid = FALSE, zeroline = FALSE ),
        margin = list(l = 100),
        showlegend = FALSE, 
        shapes = list(
          type = "line", 
          x0 = 0, 
          x1 = 1, 
          xref = "paper",
          y0 = 1, 
          y1 = 1, 
          yref = "y",
          line = list(color = "gray50", dash= "dash", opacity = 0.3)
        )
      ) 
    return(p)
    
    
  })
  
  #Download file of ALL COUNTY 7-day average Reff Values
  output$dlRt.cnty <- downloadHandler(
    
    filename = function() { paste("Rt_Nowcasts_7DayAvg_Counties",Sys.Date(),'.csv', sep='') },
    
    content = function(file) {
      
      # Title
      t <- c(paste("R-Effective 7 Day Averages for Counties", sep = ""),"","","","")
      #Subtitle
      tt <-  c(paste("COVID Assessment Tool - Downloaded on",Sys.Date(), sep = " "),"","","","")
      
      df <- cnty.7.day.rt %>% as.data.frame()
      if ( ncol(df) > 2 ) { df[,2:ncol(df)] <- lapply(df[,2:ncol(df)],function(x) round(x,2)) } else { df[,2] <- round(df[,2],2)  } 
      df[is.na(df)] <- 0
      df[] <- lapply(df, as.character)
      
      #Column labels
      
      l <- c("County","COVIDActNow - 7 Day Avg", "LL", "UL")
      
      #Source
      s <- c("Please see the Technical Notes tab of the application.","","","","")
      p <- c(paste0("Prepared by: ",state_name," Department of Public Health"),"","","","")
      u <- c("Source: COVIDActNow - https://blog.covidactnow.org/modeling-metrics-critical-to-reopen-safely/","","","","")
      
      dlm <- rbind(t, tt, l, df, s, p, u)
      write.table(dlm, file, row.names = F, col.names = F, quote = F, na= "NA", sep = ",")
    })   
  
  
  
  #### Hospitalization Projections ####
  
  #Data Prep
  hosp.proj.ts <- reactive({
    
    min_hosp <- min(covid$Most.Recent.Date)
    hosp <- covid %>% select(Most.Recent.Date,COVID.19.Positive.Patients) %>% filter(covid$County.Name == state_name) %>% as.data.frame()
    can.hosp.proj <- can.state.observed %>% select(date, hospitalBedsRequired) %>% filter(min_hosp <= date & date <= Sys.Date() + 30)
    IHME.hosp.proj <- IHME %>% select(date, allbed_mean) %>% filter(min_hosp <= date & date <= Sys.Date() + 30)
    mobs.hosp.proj <- mobs %>% select(2,8) %>% filter(min_hosp <= date & date <= Sys.Date() + 30)
    mit.hosp.proj <- mit %>% select(11,7) %>% filter(min_hosp <= date & date <= Sys.Date() + 30)
    
    covid.xts <- xts(hosp[,-1],hosp$Most.Recent.Date)
    can.proj.xts <- xts(can.hosp.proj[,-1],can.hosp.proj$date)
    ihme.proj.xts <- xts(IHME.hosp.proj[,-1],IHME.hosp.proj$date)
    mobs.proj.xts <- xts(mobs.hosp.proj[,-1],mobs.hosp.proj$date)
    mit.proj.xts <- xts(mit.hosp.proj[,-1],mit.hosp.proj$date)
    
    df <- merge(covid.xts,can.proj.xts,ihme.proj.xts,mobs.proj.xts,mit.proj.xts)
    
    df$mean.proj <- rowMeans(df[,2:5], na.rm = TRUE)
    df$mean.proj <- ifelse(!is.na(df$covid.xts), NA, df$mean.proj)
    
    df <- as.data.table(df) %>% as.data.frame()
    
    df$period <- ifelse(!is.na(df$covid.xts), "solid", "dash")
    df$type <- ifelse(!is.na(df$covid.xts), "Est.", "Proj.")
    return(df)
    
  })
  
  #Value Boxes
  output$actual.hosp.box <- renderValueBox({
    
    cdt <- max(covid$Most.Recent.Date)
    current.hosp <- as.character(covid[which(covid$Most.Recent.Date == cdt & covid$County.Name == state_name),"COVID.19.Positive.Patients"])
    valueBox( "Actuals Go Here",
              #format(as.numeric(current.hosp), big.mark = ","), 
              paste0("Actuals: ",cdt), color = "black")
    
  })
  
  output$mean.proj.hosp.box <- renderUI({ 
    
    cdt.ihme <- max( IHME[which(IHME$date <= Sys.Date() + 30),]$date  )
    mean.proj <-  hosp.proj.ts() %>% slice(n()) %>% select(7)
    valueBox( format(round(mean.proj, digits = 0), big.mark = ","), paste0("Mean 30-Day Forecast through ", cdt.ihme), color = "blue", width = 12)
    
  })
  
  #Graphs
  output$hosp.proj.plot <- renderPlotly({
    
    df <- hosp.proj.ts()  
    
    cdt <- max(df[which(!is.na(df$covid.xts)),1])
    
    p <-  plot_ly(df,
                  hoverinfo = 'text') %>%
      add_trace(x = df[[1]], 
                y = df[[2]], 
                name = "Actuals",
                type = 'scatter',
                mode = "lines+markers", 
                hoverinfo = 'text',
                text = paste0(df[[1]],
                              "<br>",
                              "Actual Hospitalization (PLACEHOLDER DATA - PLEASE REPLACE!!): ", format(round(df[[2]],0), big.mark = ",") ),
                line = list(color = "black"),
                marker = list(color = "black",  symbol= "circle")
      ) %>%
      add_trace(x = df[[1]], 
                y = df[[3]], 
                name = ~I(paste0("COVIDActNow - ",df$type)),
                type = 'scatter',
                mode = "lines", 
                inherit = TRUE,
                line = list(color="orange"),
                linetype = ~I(period),
                hoverinfo = 'text',
                text = paste0(df[[1]],
                              "<br>",
                              "COVIDActNow Estimate: ", format(round(df[[3]],0), big.mark = ",") )
                
      ) %>%
      add_trace(x = df[[1]], 
                y = df[[4]], 
                name = ~I(paste0("IHME - ",df$type)),
                type = 'scatter',
                mode = "lines", 
                inherit = TRUE,
                line = list(color="navy"),
                linetype = ~I(period),
                hoverinfo = 'text',
                text = paste0(df[[1]],
                              "<br>",
                              "IHME Estimate: ", format(round(df[[4]],0), big.mark = ",") )
      ) %>%
      add_trace(x = df[[1]], 
                y = df[[5]], 
                name = ~I(paste0("MOBS - ",df$type)),
                type = 'scatter',
                mode = "lines", 
                inherit = TRUE,
                line = list(color="red"),
                linetype = ~I(period),
                hoverinfo = 'text',
                text = paste0(df[[1]],
                              "<br>",
                              "MOBS Estimate: ", format(round(df[[5]],0), big.mark = ",") )
      ) %>%
      add_trace(x = df[[1]], 
                y = df[[6]], 
                name = ~I(paste0("MIT - ",df$type)),
                type = 'scatter',
                mode = "lines", 
                inherit = TRUE,
                line = list(color="green"),
                linetype = ~I(period),
                hoverinfo = 'text',
                text = paste0(df[[1]],
                              "<br>",
                              "MIT Estimate: ", format(round(df[[6]],0), big.mark = ",") )
      ) %>%
      add_trace(x = df[[1]], 
                y = df[[7]], 
                name = "Mean Proj.",
                type = 'scatter',
                mode = "lines", 
                hoverinfo = 'text',
                text = paste0(df[[1]],
                              "<br>",
                              "Mean Projection: ", format(round(df[[7]],0), big.mark = ",") ),
                line = list(color = '#2b8cbe', width = 5)
      ) %>%
      layout(
        title = NULL,
        xaxis = list(title = NULL, showline = TRUE, showgrid = FALSE, zeroline = FALSE ),
        yaxis = list(title = "Hospitalizations", showline = TRUE, showgrid = FALSE, zeroline = FALSE ),
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
      tt <-  c(paste("COVID Assessment Tool - Downloaded on",Sys.Date(), sep = " "),"","","","","","")
      #Column labels
      l <- c("Date","Actuals", "COVIDActNow","IHME","MOBS","MIT","Mean")
      
      df <- hosp.proj.ts()[,1:7] %>% as.data.frame()
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
  county.hosp <- reactive({
    progress <- Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Gathering Hospitalization Forecasts", value = 0)
    
    cnty <- input$select.county.hosp
    progress$inc(3/4)
    # out <- lapply(cnty[1], function(x) get_can_cnty(x))
    # cnty.hosp <- do.call("rbind",out)
    out <- filter(can.county.observed, fips ==  cnty)
    
    cnty.hosp <- out %>% select(date,hospitalBedsRequired) %>% as.data.frame()
    progress$inc(1/4)
    return(cnty.hosp)
    
  })
  
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
  
  
  hosp.proj.cnty.ts <- reactive({
    
    c <- names(canfipslist[match(input$select.county.hosp,canfipslist)])
    min_hosp <- min(covid$Most.Recent.Date)
    hosp <- covid %>% select(Most.Recent.Date,COVID.19.Positive.Patients) %>% filter(covid$County.Name == c) %>% as.data.frame()
    can.hosp.proj <- county.hosp() %>% select(date, hospitalBedsRequired) %>% filter(min_hosp <= date & date <= Sys.Date() + 30)
    
    covid.xts <- xts(hosp[,-1],hosp$Most.Recent.Date)
    can.proj.xts <- xts(can.hosp.proj[,-1],can.hosp.proj$date)
    
    df <- merge(covid.xts,can.proj.xts)
    
    df <- as.data.table(df) %>% as.data.frame()
    
    df$period <- ifelse(!is.na(df$covid.xts), "solid", "dash")
    return(df)
    
  })
  
  #Value Boxes
  output$actual.cnty.hosp.box <- renderValueBox({
    c <- names(canfipslist[match(input$select.county.hosp,canfipslist)])
    cdt <- max(covid$Most.Recent.Date)
    current.hosp <- as.character(covid[which(covid$Most.Recent.Date == cdt & covid$County.Name == c),"COVID.19.Positive.Patients"])
    valueBox( "Counts/Beds Here",
              #paste0(format(as.numeric(current.hosp), big.mark = ","),"/",
              #       #format(as.numeric(fc.cnty.beds()), big.mark = ",") 
              #       ), 
              paste0("Actuals / Total Beds: ",cdt), 
              color = "black")
  })
  
  output$mean.cnty.proj.hosp.box <- renderValueBox({ 
    
    cdt.ihme <- max( IHME[which(IHME$date <= Sys.Date() + 30),]$date  )
    mean.proj <-  hosp.proj.cnty.ts() %>% slice(n()) %>% select(3)
    valueBox( format(round(mean.proj, digits = 0), big.mark = ","), 
              paste0("30-Day Forecast through ", cdt.ihme), color = "blue")
    
  })
  
  #Graph
  output$county.hosp.plot <- renderPlotly({
    
    df <- hosp.proj.cnty.ts()  
    
    cdt <- max(df[which(!is.na(df$covid.xts)),1])
    
    today <- list(type = "line", 
                  y0 = 0, 
                  y1 = 1, 
                  yref = "paper",
                  x0 = cdt, 
                  x1 = cdt, 
                  line = list(color = "black", dash = 'dash') )
    
    p <-  plot_ly(df,
                  hoverinfo = 'text') %>%
      add_trace(x = df[[1]], 
                y = df[[2]], 
                name = "Actuals",
                type = 'scatter',
                mode = "lines+markers", 
                hoverinfo = 'text',
                text = paste0(df[[1]],
                              "<br>",
                              "Actual Hospitalization (PLACEHOLDER DATA - PLEASE REPLACE!!): ", format(df[[2]], big.mark = ",") ),
                line = list(color = "black"),
                marker = list(color = "black",  symbol= "circle")
      ) %>%
      add_trace(x = df[[1]], 
                y = df[[3]], 
                name = "COVIDActNow - Proj.",
                type = 'scatter',
                mode = "lines", 
                inherit = TRUE,
                line = list(color="orange"),
                linetype = ~I(period),
                hoverinfo = 'text',
                text = paste0(df[[1]],
                              "<br>",
                              "COVIDActNow Estimate: ", format(df[[3]], big.mark = ",") )
                
      ) %>%
      layout(
        title = as.character(counties[match(input$select.county.hosp, counties$fips),"county"]),
        xaxis = list(title = NULL, showline = TRUE, showgrid = FALSE, zeroline = FALSE ),
        yaxis = list(title = "Hospitalziations", showline = TRUE, showgrid = FALSE, zeroline = FALSE),
        margin = list(l = 100),
        showlegend = TRUE, 
        shapes = list(today)
      ) 
    return(p)
    
  })
  
  #Download file of COUNTY Hospitalization Forecasts
  output$dlhosp.cnty <- downloadHandler(
    
    filename = function() { paste("Hospital_Forecasts_for_",names(canfipslist[match(input$select.county.hosp,canfipslist)]),Sys.Date(),'.csv', sep='') },
    
    content = function(file) {
      
      c <- names(canfipslist[match(input$select.county.hosp,canfipslist)])
      # Title
      t <- c(paste("Hospitalization Forecasts for ",c, sep = ""),"","","","","","")
      #Subtitle
      tt <-  c(paste("COVID Assessment Tool - Downloaded on",Sys.Date(), sep = " "),"","","","","","")
      #Column labels
      l <- c("Date","Actuals", "COVIDActNow")
      
      df <-  hosp.proj.cnty.ts()[,1:3] %>% as.data.frame()
      df[,2:3] <- lapply(df[,2:3],function(x) round(x,2))
      df[is.na(df)] <- 0
      df[] <- lapply(df, as.character)
      
      #Source
      s <- c("Please see the Technical Notes tab of the application for data sources.","","","","","","")
      p <- c(paste0("Prepared by: ",state_name," Department of Public Health"),"","","","","","")
      
      dlm <- rbind(t, tt, l, df, s, p)
      write.table(dlm, file, row.names = F, col.names = F, quote = F, na= "NA", sep = ",")
    })   
  
  
  #### Statewide Cumulative Deaths Projections ####
  
  #Data Prep
  cdeath.ca <- reactive({
    
    reich_test <- reich_lab %>% unique() %>% as.data.frame()
    
    cdeaths_test <- covid %>% select(Most.Recent.Date,Total.Count.Deaths) %>% 
      filter(covid$County.Name == state_name) %>% 
      mutate(model_team = 'Actuals') %>%
      rename(model_team = model_team,
             target_end_date = Most.Recent.Date,
             pointNA = Total.Count.Deaths
      ) %>%
      select(model_team, pointNA, target_end_date) %>%
      as.data.frame()
    
    reich_test <- rbind(reich_test,cdeaths_test) 
    
    reich_test <-  reich_test %>% distinct(model_team, target_end_date, .keep_all = TRUE)  %>%  spread(model_team, pointNA)
    
  })
  
  #Value Boxes
  output$actual.cdeath.box <- renderValueBox({
    
    cdt <- max(covid$Most.Recent.Date)
    current.deaths <- as.character(covid[which(covid$Most.Recent.Date == cdt & covid$County.Name == state_name),4])
    valueBox( format(as.numeric(current.deaths), big.mark = ","), paste0("Actuals (NYTIMES DATA): ",cdt), color = "black")
    
  })
  
  output$mean.proj.cdeaths.box <- renderUI({ 
    
    ensemble <- cdeath.ca() %>% select(target_end_date,COVIDhub.ensemble) %>% filter(!is.na(COVIDhub.ensemble))
    cdt.ens <- max(ensemble$target_end_date)
    mean.proj <- ensemble %>% slice(n()) %>% select(2)
    valueBox( format(round(mean.proj, digits = 0), big.mark = ","), paste0("COVIDhub Ensemble Forecast through ", cdt.ens), color = "blue", width = 12)
    
  })
  
  #Graphs
  output$cdeath.proj.plot <- renderPlotly({
    
    df <- cdeath.ca()
    
    #Need to filter out Reich Lab models that represent scenarios rather than forecasts of current conditions
    models <- names(df)
    models <- setdiff(models, c("target_end_date", "CU.nointerv", "CU.60contact","CU.70contact",
                                "CU.80contact","CU.80contact1x10p","CU.80contact1x5p","CU.80contactw10p",
                                "CU.80contactw5p","COVIDhub.ensemble", "Actuals" ) )
    models <-  models %>% c("Actuals","COVIDhub.ensemble")
    
    p <- plot_ly(data=df, type = "scatter", mode = "lines")
    
    for(trace in models){
      if (trace == "Actuals") {
        
        p <- p %>% plotly::add_trace(x = ~target_end_date, 
                                     y = as.formula(paste0("~`", trace, "`")), 
                                     name = trace, 
                                     type = 'scatter',
                                     mode = "lines+markers", 
                                     line = list(color ="black"),
                                     marker = list(color = "black",  symbol= "circle"),
                                     hoverinfo = 'text',
                                     text = paste0(df[[1]],
                                                   "<br>",
                                                   "Actual Total Deaths (NYTIMES DATA): ", format(df$Actuals, big.mark = ","))
        )
      } else {
        
        if (trace == "COVIDhub.ensemble") {
          p <- p %>% add_trace(x = ~target_end_date,
                               y = as.formula(paste0("~`", trace, "`")), 
                               inherit = FALSE,
                               name = trace, 
                               line = list(shape = "spline", color = '#2b8cbe'),
                               marker = list(color = '#2b8cbe',  symbol= "circle"),
                               hoverinfo = 'text',
                               text = paste0(df[[1]],
                                             "<br>",
                                             "COVIDhub Ensemble Forecast: ", format(df$COVIDhub.ensemble, big.mark = ","))
          ) 
        } else {
          p <- p %>% plotly::add_trace(x = ~target_end_date, 
                                       y = as.formula(paste0("~`", trace, "`")), 
                                       name = trace, 
                                       type = 'scatter',
                                       mode = "lines", 
                                       line = list(color ="lightgray"),
                                       hoverinfo = 'text+y',
                                       text = paste0(df[[1]],
                                                     "<br>",
                                                     trace," Forecast")
          )
        }
      }
    }
    p %>% 
      layout(title = NULL,
             xaxis = list(title = " ", showline = TRUE, showgrid = FALSE, zeroline = FALSE ),
             yaxis = list(title = "Total Deaths", showline = TRUE, showgrid = FALSE, zeroline = FALSE, hoverformat = ',.2r' ),
             margin = list(l = 100),
             legend = list(traceorder = "reversed"),
             showlegend = TRUE)
    
  })
  
  #Download file of Statewide Cumulative Deaths Forecasts
  output$dlDeath <- downloadHandler(
    
    filename = function() { paste("Cumulative_Deaths_Forecasts_",Sys.Date(),'.csv', sep='') },
    
    content = function(file) {
      
      # Title
      t <- c(paste("Statewide Cumulative Deaths Forecasts", sep = ""),rep("",ncol(cdeath.ca())-1) )
      #Subtitle
      tt <-  c(paste("COVID Assessment Tool - Downloaded on",Sys.Date(), sep = " "),rep("",ncol(cdeath.ca())-1))
      #Column labels
      l <- names(cdeath.ca())
      
      df <- cdeath.ca() %>% as.data.frame()
      #df[,2:ncol(df)] <- lapply(df[,2:ncol(df)],function(x) round(x,2))
      df[is.na(df)] <- 0
      df[] <- lapply(df, as.character)
      
      #Source
      s <- c("Please see the Technical Notes tab of the application for data sources.",rep("",ncol(cdeath.ca())-1))
      p <- c(paste0("Prepared by: ",state_name," Department of Public Health"),rep("",ncol(cdeath.ca())-1))
      
      dlm <- rbind(t, tt, l, df, s, p)
      write.table(dlm, file, row.names = F, col.names = F, quote = F, na= "NA", sep = ",")
    })   
  
  
  #### County Cumulative Death Projections ####
  
  ### You can add additional county death forecasts here ###
  ### 
  
  #Data prep
  county.deaths <- reactive({
    
    progress <- Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Gathering Death Forecast Data", value = 0)
    
    fips <- input$select.county.death
    cnty <- names(canfipslist[match(fips,canfipslist)])
    
    #Used to filter model estimates that occur prior to actuals
    min_death <- min(covid$Most.Recent.Date)
    
    death <- covid %>% select(Most.Recent.Date,Total.Count.Deaths) %>% filter(covid$County.Name == cnty) %>% as.data.frame()
    
    progress$inc(3/4)
    # out <- lapply(fips[1], function(x) get_can_cnty(x))
    # can.death <- do.call("rbind",out)
    out <- filter(can.county.observed, county ==  cnty)
    
    can.death <- out %>% select(date,cumulativeDeaths) %>% 
      filter(min_death <= date & date <= Sys.Date() + 30) %>% 
      rename(CovidActNow = cumulativeDeaths) %>% as.data.frame()
    progress$inc(1/4)
    
    covid.xts <- xts(death[,-1],death$Most.Recent.Date)
    can.proj.xts <- xts(can.death[,-1],can.death$date)
    #Add additional forecasts as xts object
    
    df <- merge(covid.xts,can.proj.xts)
    
    #Estimate a mean forecast here
    df$mean.proj <- rowMeans(df[,2], na.rm = TRUE)
    df$mean.proj <- ifelse(!is.na(df$covid.xts), NA, df$mean.proj)
    
    df <- as.data.table(df) %>% as.data.frame()
    
    df$period <- ifelse(!is.na(df$covid.xts), "solid", "dash")
    df$type <- ifelse(!is.na(df$covid.xts), "Est.", "Proj.")
    
    return(df)
    
  })
  
  #Value Boxes
  output$actual.cnty.death.box <- renderValueBox({
    c <- names(canfipslist[match(input$select.county.death,canfipslist)])
    cdt <- max(covid$Most.Recent.Date)
    current.deaths <- as.character(covid[which(covid$Most.Recent.Date == cdt & covid$County.Name == c),"Total.Count.Deaths"])
    valueBox( paste0(format(as.numeric(current.deaths), big.mark = ",") ), 
              paste0("Actual Deaths (NYTIMES DATA): ",cdt), 
              color = "black")
  })
  
  output$mean.cnty.proj.death.box <- renderValueBox({ 
    df <- county.deaths()
    cdt <- max( df$index )
    mean.proj <-  df %>% slice(n()) %>% select(mean.proj)
    valueBox( format(round(mean.proj, digits = 0), big.mark = ","), 
              paste0("30-Day Forecast through ", cdt), color = "blue")
    
  })
  
  #Graph
  output$county.death.plot <- renderPlotly({
    
    df <- county.deaths()  
    
    cdt <- max(df[which(!is.na(df$covid.xts)),1])
    
    today <- list(type = "line", 
                  y0 = 0, 
                  y1 = 1, 
                  yref = "paper",
                  x0 = cdt, 
                  x1 = cdt, 
                  line = list(color = "black", dash = 'dash') )
    
    p <-  plot_ly(df,
                  hoverinfo = 'text') %>%
      add_trace(x = df[[1]], 
                y = df[[2]], 
                name = "Actuals",
                type = 'scatter',
                mode = "lines+markers", 
                hoverinfo = 'text',
                text = paste0(df[[1]],
                              "<br>",
                              "Actual Deaths (NYTIMES DATA): ", format(df[[2]], big.mark = ",") ),
                line = list(color = "black"),
                marker = list(color = "black",  symbol= "circle")
      ) %>%
      add_trace(x = df[[1]], 
                y = df[[3]], 
                name = ~I(paste0("COVIDActNow - ",df$type)),
                type = 'scatter',
                mode = "lines", 
                inherit = TRUE,
                line = list(color="orange"),
                linetype = ~I(period),
                hoverinfo = 'text',
                text = paste0(df[[1]],
                              "<br>",
                              "COVIDActNow Estimate: ", format(df[[3]], big.mark = ",") )
                
      ) %>%
      #Example trace for additional forecast
      # add_trace(x = df[[1]], 
      #           y = df[[4]], 
      #           name = ~I(paste0("UCLA - ",df$type)),
      #           type = 'scatter',
      #           mode = "lines", 
      #           inherit = TRUE,
      #           line = list(color="blue"),
      #           linetype = ~I(period),
      #           hoverinfo = 'text',
      #           text = paste0(df[[1]],
      #                         "<br>",
      #                         "UCLA Estimate: ", format(df[[4]], big.mark = ",") )
      #           
      # ) %>%
      add_trace(x = df[[1]], 
                y = df[[4]], 
                name = "Mean Proj.",
                type = 'scatter',
                mode = "lines", 
                hoverinfo = 'text',
                text = paste0(df[[1]],
                              "<br>",
                              "Mean Projection: ", format(round(df[[4]],0), big.mark = ",") ),
                line = list(color = '#2b8cbe', width = 5)
      ) %>%
      layout(
        title = as.character(counties[match(input$select.county.death, counties$fips),"county"]),
        xaxis = list(title = NULL, showline = TRUE, showgrid = FALSE, zeroline = FALSE ),
        yaxis = list(title = "Total Deaths", showline = TRUE, showgrid = FALSE, zeroline = FALSE),
        margin = list(l = 100),
        showlegend = TRUE, 
        shapes = list(today)
      ) 
    return(p)
    
  })
  
  #Download file of COUNTY Total Death Forecasts
  output$dlDeath.cnty <- downloadHandler(
    
    filename = function() { paste("Cumulative_Death_Forecasts_for_",names(canfipslist[match(input$select.county.death,canfipslist)]),Sys.Date(),'.csv', sep='') },
    
    content = function(file) {
      
      c <- names(canfipslist[match(input$select.county.death,canfipslist)])
      # Title
      t <- c(paste("Cumulative Death Forecasts for ",c, sep = ""),rep("",ncol(county.deaths())-1))
      #Subtitle
      tt <-  c(paste("COVID Assessment Tool - Downloaded on",Sys.Date(), sep = " "),rep("",ncol(county.deaths())-1))
      
      df <- county.deaths() %>% select(-c(period, type)) %>% rename(date = index) %>% as.data.frame()
      df[is.na(df)] <- 0
      df[] <- lapply(df, as.character)
      
      #Column labels
      l <- c("Date","Total Deaths")
      
      if ( "can.proj.xts" %in% names(county.deaths()) ) {  l <- c(l, c("COVIDActNow")) }
      #Add lines for additional sources of forecasts
      #if ( "ucla.proj.xts" %in% names(county.deaths()) ) {  l <- c(l, c("UCLA")) }
      if ( length(l) > 2 ) { l <- c(l, c("Mean") ) }
      
      #Source
      s <- c("Please see the Technical Notes tab of the application for data sources.",rep("",ncol(county.deaths())-1))
      p <- c(paste0("Prepared by: ",state_name," Department of Public Health"),rep("",ncol(county.deaths())-1))
      
      dlm <- rbind(t, tt, l, df, s, p)
      write.table(dlm, file, row.names = F, col.names = F, quote = F, na= "NA", sep = ",")
    })   
  
  #### Scenario Models ####
  
  output$model.descrip.ts <- renderUI({
    
    UKKC <- as.character(input$include_JHU_UKKC)
    model_descrip_list <- lapply(UKKC, function(i) { HTML(paste("<p>",as.character(scenarios[match(i, scenarios$colvar),2]),": ",
                                                                      as.character(scenarios[match(i, scenarios$colvar),4]),"</p>")) })
    do.call(tagList, model_descrip_list)
    
  })
  
  #### Daily Estimates #####
  
  
  ### For NPI scenario models, this is the drop that lists the NPI drop down. 
  ### These lists are defined in the model_output_lists.R
  ### California had over a dozen models, and therefore needed a flexible way to add/remove them.
  ### The dates are to identify when collections of models were run.
  
  output$physical.select <- renderUI({
    
    s <- as.character(state_name == input$county_ts)
    choice.list <- switch(s,
                          "TRUE" = list ( #"5/22/2020" = modellist[c(8:11)],
                                     #"4/23/2020" = modellist[c(4:7)],
                                     "4/11/2020" = otherlist[1:2],
                                     "4/07/2020" = otherlist[3]  ),
                           list (#"5/22/2020" = modellist[c(8:11)],
                                 #"4/23/2020" = modellist[c(4:7)],
                                 "4/11/2020" = otherlist[1:2]    )
    )
    
    pickerInput(
      inputId = "include_JHU_UKKC", "Select Scenario", 
      choices = choice.list,
      selected = c("strictDistancingNow", 
                   "weakDistancingNow"),
      options = list(`actions-box` = TRUE, noneSelectedText = "Select Scenario"), 
      #inline = TRUE,
      multiple = TRUE,
      choicesOpt = list( style = rep(("color: black; background: white; font-weight: bold;"),13)) 
    )
    
  })
  
  output$epi_covid_select <- renderUI({
    selectInput("select_COVID",
                "Select Actuals (THIS IS PLACEHOLDER DATA):",
                COVIDvar,
                selected = switch(input$selected_crosswalk,
                                  "1" = "COVID.19.Positive.Patients",
                                  "2" = "ICU.COVID.19.Positive.Patients",
                                  "3" = "Total.Count.Deaths")
    )
  })
  
  state.model.xts <- reactive({
    
    c <- input$county_ts
    
    # This is how we organized Johns Hopkins Runs, which consisted of a mean, median and intervals.
    # We were primarily interested in displaying hospitalziations, ICU beds and cumulative deaths despite other 
    # outputs available from some modelers. The goal here is to harmonize and organize outputs from multiple modelers.
    
    # JHU_sts.m <- to_xts_awsJHU(JHU_aws, c,
    #                            switch(input$selected_crosswalk,
    #                                   "1" = "hosp_occup_mean",
    #                                   "2" = "icu_occup_mean",
    #                                   "3" = "cum_deaths_mean"
    #                            ))
    # 
    # JHU_sts.md <- to_xts_awsJHU(JHU_aws, c,
    #                             switch(input$selected_crosswalk,
    #                                    "1" = "hosp_occup_q50",
    #                                    "2" = "icu_occup_q50",
    #                                    "3" = "cum_deaths_q50"
    #                             ))
    # colnames(JHU_sts.md) <- paste(colnames(JHU_sts.md),"M", sep = ".")
    # 
    # JHU_sts.L <- to_xts_awsJHU(JHU_aws, c,
    #                            switch(input$selected_crosswalk,
    #                                   "1" = "hosp_occup_q25",
    #                                   "2" = "icu_occup_q25",
    #                                   "3" = "cum_deaths_q25"
    #                            ))
    # colnames(JHU_sts.L) <- paste(colnames(JHU_sts.L),"L", sep = ".")
    # 
    # JHU_sts.H <- to_xts_awsJHU(JHU_aws, c, 
    #                            switch(input$selected_crosswalk,
    #                                   "1" = "hosp_occup_q75",
    #                                   "2" = "icu_occup_q75",
    #                                   "3" = "cum_deaths_q75"
    #                            ))
    # colnames(JHU_sts.H) <- paste(colnames(JHU_sts.H),"H", sep = ".")
    
    IHME_sts <- to_xts_IHME(IHME,state_name,
                            switch(input$selected_crosswalk,
                                   "1" = "allbed_mean",
                                   "2" = "ICUbed_mean",
                                   "3" = "totdea_mean"
                            ))
    IHME_sts.L <- to_xts_IHME(IHME,state_name,
                              switch(input$selected_crosswalk,
                                     "1" = "allbed_lower",
                                     "2" = "ICUbed_lower",
                                     "3" = "totdea_lower"
                              ))
    IHME_sts.H <- to_xts_IHME(IHME,state_name,
                              switch(input$selected_crosswalk,
                                     "1" = "allbed_upper",
                                     "2" = "ICUbed_upper",
                                     "3" = "totdea_upper"
                              ))
    
    CAN_sts <- to_xts_CAN(CAN_aws, c,
                          switch(input$selected_crosswalk,
                                 "1" = "hospitalizations",
                                 "2" = "beds",
                                 "3" = "deaths"
                          ))
    
    COVID_sts <- to_xts_COVID(covid, c)
    
    #Not all modelers produce coutny level outputs for scenarios. i.e. IHME
    
    if (c != state_name) {
      all_ts <- suppressWarnings( merge.xts(#JHU_sts.m, #New JHU with optional estimates and intervals
                                            #JHU_sts.md,
                                            #JHU_sts.L,
                                            #JHU_sts.H,
                                            CAN_sts,
                                            COVID_sts, fill = NA) )
    } else { 
      all_ts <- suppressWarnings( merge.xts(#JHU_sts.m, #New JHU with optional estimates and intervals
                                            #JHU_sts.md,
                                            #JHU_sts.L,
                                            #JHU_sts.H,
                                            IHME_sts, #IHME with intervals
                                            IHME_sts.L,
                                            IHME_sts.H,
                                            CAN_sts,
                                            COVID_sts, 
                                            fill = NA #Covid outputs
      ) ) 
    }
    
    #all_ts <- all_ts[,c(-1)]
    all_ts <- all_ts["20200301/20201231"] #Some models extend beyond 2020
    return(all_ts)
  })
  
  total.cnty.beds <- reactive({
    
    c <- input$county_ts 
    
    # This coded grabs county bed counts for a reference line in plots.
    # if (c %in% cnty.beds[,1] == TRUE) {
    #   beds <- c(cnty.beds[which(cnty.beds$COUNTY == c),9])
    # } else {
    #   beds <- c(NA)
    # }
    
    beds <- 100 
    
  })
  
  #Regex patterns for JHU scenarios
  
  jhu.no <- "UK.\\w+.\\d+_\\d+|.\\w+_\\w{4,}"
  jhu.M <- "UK.\\w+.\\d+_\\d+.M|.\\w+_\\w{4,}.M"
  jhu.lh <- "UK.\\w+.\\d[w].\\w+.[LH]|.\\w+_\\w{4,}.[LH]"
  jhu.lh.b <- "UK.\\w+.\\d+_\\d+.[LH]|.\\w+_\\w{4,}.[LH]"
  
  output$physical.graph <- renderDygraph({
    
    df <- state.model.xts()
    dtrange <- paste(as.character(input$dateRange_ts), collapse = "/")
    
    chbx <- c()
    
    #### Actuals
    
    if ( input$actuals == TRUE) {chbx <- c(chbx,c(input$select_COVID)) }
    
    UKKC <- as.character(input$include_JHU_UKKC)
    
    if ( TRUE %in% grepl(jhu.no, UKKC) & input$physical.mmd == "M" ) {
      JHU_list <- UKKC[grep(jhu.no,UKKC)]
      chbx <- c(chbx, c(JHU_list) )
    } else {
      JHU_list <- UKKC[grep(jhu.no,UKKC)]
      chbx <- c(chbx, c( as.character(lapply(seq_along(JHU_list), function(i) { paste0(as.character( JHU_list[[i]] ),".M" ) } ) ) )  )
    }
    
    if (TRUE %in% grepl(jhu.no, UKKC) & input$physical.iqr == TRUE) {
      JHU_list <- UKKC[grep(jhu.no,UKKC)]
      chbx <- c(chbx, c( as.character(lapply(seq_along(JHU_list), function(i) {paste0(as.character( JHU_list[[i]] ),".L" ) } )) ),
                c( as.character(lapply(seq_along(JHU_list), function(i) {paste0(as.character( JHU_list[[i]] ),".H" ) } )) ) )
    }
    
    if ( TRUE %in% grepl("IHME_sts", UKKC ) & input$county_ts == state_name ) { 
      chbx <- chbx %>% c("IHME_sts")
    }
    
    if ( TRUE %in% grepl("IHME_sts", UKKC ) & input$IHME.iqr == TRUE & input$county_ts == state_name) { 
      IHME <- "IHME_sts"
      chbx <- c(chbx, c( as.character(lapply(seq_along(IHME), function(i) {paste0(as.character( IHME[[i]] ),".L") } )) ),
                c( as.character(lapply(seq_along(IHME), function(i) {paste0(as.character( IHME[[i]] ),".H") } )) )
      )
    }
    
    if ( TRUE %in% grepl("weakDistancingNow|strictDistancingNow",UKKC) & 
         
         input$county_ts %in% can_counties == TRUE ) {
      can <- UKKC[grep("weakDistancingNow|strictDistancingNow",UKKC)] 
      chbx <- chbx %>% c(can)
    } 
    
    df <- df[,c(chbx)]
    
    FUNC_JSFormatNumber <- "function(x) {return x.toString().replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,')}"
    
    d <- dygraph(df, main = switch(input$selected_crosswalk,
                                   "1" = paste0(input$county_ts," COVID Hospitalizations"),
                                   "2" = paste0(input$county_ts," COVID ICU Patients"),
                                   "3" = paste0(input$county_ts," COVID Cumulative Deaths")
    )) 
    
    if ( TRUE %in% grepl(jhu.lh, chbx) |  TRUE %in% grepl(jhu.lh.b, chbx) )  {
      
      if ( input$physical.mmd == "M") {
        chbx.M <- chbx[grep(jhu.no,chbx)]
        chbx.M <- unique(str_remove(chbx.M, "\\.[LH]"))
        for (scenario in chbx.M) { 
          d <- d %>% dySeries(c( paste0(scenario,".L"),paste0(scenario),paste0(scenario,".H")),  label = names(modellist[match(scenario,modellist)]), fillGraph = FALSE)
        }
      } else  {
        chbx.M <- chbx[grep(jhu.M,chbx)]
        chbx.M <- str_remove(chbx.M, ".M")
        for (scenario in chbx.M) { 
          d <- d %>% dySeries(c( paste0(scenario,".L"),paste0(scenario,".M"),paste0(scenario,".H")),  label = names(modellist[match(scenario,modellist)]), fillGraph = FALSE)
        }
      }
      # No intervals 
    } else {
      if ( input$physical.mmd == "M") {
        chbx.M <- chbx[grep(jhu.no,chbx)]
        for (scenario in chbx.M) { 
          d <- d %>% dySeries(paste0(scenario),  label = names(modellist[match(scenario,modellist)]), fillGraph = FALSE)
        }
      } else {
        chbx.M <- chbx[grep(jhu.M,chbx)]
        chbx.M <- str_remove(chbx.M, ".M")
        for (scenario in chbx.M) { 
          d <- d %>% dySeries(paste0(scenario,".M"),  label = names(modellist[match(scenario,modellist)]), fillGraph = FALSE)
        }
      }
      
    }
    
    if ( TRUE %in% grepl("IHME_sts.[LH]", chbx) ){
      if ( "IHME_sts.L" %in% c(chbx) )  {d <- d %>% dySeries(c("IHME_sts.L","IHME_sts","IHME_sts.H"),  label = 'IHME Model', fillGraph = FALSE) }
    } else {
      if ( "IHME_sts" %in% c(chbx) )  {d <- d %>% dySeries("IHME_sts",  label = 'IHME Model', fillGraph = FALSE) }
    }
    
    if ( "weakDistancingNow" %in% c(chbx)   )  {d <- d %>% dySeries("weakDistancingNow",  label = 'CAN: Delay/Distancing', fillGraph = FALSE) }
    if ( "strictDistancingNow" %in% c(chbx) )  {d <- d %>% dySeries("strictDistancingNow",  label = 'CAN: Shelter in Place', fillGraph = FALSE) }
    
    if ( "Total.Count.Deaths" %in% c(chbx) ) {d <- d %>% dySeries("Total.Count.Deaths",  label = "Total Deaths", fillGraph= FALSE, drawPoints = TRUE, pointSize = 5, pointShape = "square", color = "black") }
    if ( "COVID.19.Positive.Patients" %in% c(chbx) ) {d <- d %>% dySeries("COVID.19.Positive.Patients",  label = "Patients Positive for COVID-19", fillGraph= FALSE, drawPoints = TRUE, pointSize = 5, pointShape = "diamond", color = "black") }
    if ( "ICU.COVID.19.Positive.Patients" %in% c(chbx) ) {d <- d %>% dySeries("ICU.COVID.19.Positive.Patients",  label = "ICU Patients Positive for COVID-19", fillGraph= FALSE, drawPoints = TRUE, pointSize = 5, pointShape = "hexagon", color = "black") }
    
    #### Add county beds
    if ( input$selected_crosswalk == "1" & input$county_ts == state_name) { 
      d <- d %>% dyLimit(50000, label = "Phase 1 Surge Capacity", labelLoc = c("left"), color = "black", strokePattern = "dashed") 
    } else {
      if ( input$selected_crosswalk == "1" & !is.na(total.cnty.beds()) == TRUE ) { d <- d %>% dyLimit(total.cnty.beds(), label = "Total Licensed Beds", labelLoc = c("left"), color = "black", strokePattern = "dashed") }
    }
    
    d <- d %>% dyOptions(digitsAfterDecimal=0, strokeWidth = 3, connectSeparatedPoints = TRUE, drawGrid = FALSE) %>%
      dyAxis("y", axisLabelFormatter=htmlwidgets::JS(FUNC_JSFormatNumber), valueFormatter=htmlwidgets::JS(FUNC_JSFormatNumber)) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 4)) %>%
      dyEvent(Sys.Date(), "Today", labelLoc = "top") %>%
      dyLegend(show = "always",
               labelsDiv = "legendDivID2",
               hideOnMouseOut = TRUE) %>%
      dyRangeSelector(height = 30, dateWindow = c((Sys.Date() - 30), as.Date("2020-12-31")) )
    
  })
  
  #### Static Daily Estimates ####
  
  output$physical.graph.static <- renderPlot({
    
    df <- state.model.xts()[ paste0( as.Date(input$physical.graph_date_window[[1]]),"/",as.Date(input$physical.graph_date_window[[2]]) ) ]
    #dtrange <- paste(as.character(input$dateRange_ts), collapse = "/")
    
    chbx <- c()
    #### Uncontrolled + Actuals
    #if ( input$overlay_uncontrolled == TRUE ) { chbx <- chbx %>% c("No_Intervention") }
    if ( input$actuals == TRUE) {chbx <- c(chbx,c(input$select_COVID)) }
    
    UKKC <- as.character(input$include_JHU_UKKC)
    
    if ( TRUE %in% grepl(jhu.no, UKKC) & input$physical.mmd == "M" ) {
      JHU_list <- UKKC[grep(jhu.no,UKKC)]
      chbx <- c(chbx, c(JHU_list) )
    } else {
      JHU_list <- UKKC[grep(jhu.no,UKKC)]
      chbx <- c(chbx, c( as.character(lapply(seq_along(JHU_list), function(i) { paste0(as.character( JHU_list[[i]] ),".M" ) } ) ) )  )
    }
    
    if (TRUE %in% grepl(jhu.no, UKKC) & input$physical.iqr == TRUE) {
      JHU_list <- UKKC[grep(jhu.no,UKKC)]
      chbx <- c(chbx, c( as.character(lapply(seq_along(JHU_list), function(i) {paste0(as.character( JHU_list[[i]] ),".L" ) } )) ),
                c( as.character(lapply(seq_along(JHU_list), function(i) {paste0(as.character( JHU_list[[i]] ),".H" ) } )) ) )
    }
    
    if ( TRUE %in% grepl("IHME_sts", UKKC ) & input$county_ts == state_name ) { 
      chbx <- chbx %>% c("IHME_sts")
    }
    
    if ( TRUE %in% grepl("IHME_sts", UKKC ) & input$IHME.iqr == TRUE & input$county_ts == state_name) { 
      IHME <- "IHME_sts"
      chbx <- c(chbx, c( as.character(lapply(seq_along(IHME), function(i) {paste0(as.character( IHME[[i]] ),".L") } )) ),
                c( as.character(lapply(seq_along(IHME), function(i) {paste0(as.character( IHME[[i]] ),".H") } )) )
      )
    }
    
    if ( TRUE %in% grepl("weakDistancingNow|strictDistancingNow",UKKC) & 
         input$county_ts %in% can_counties == TRUE ) {
      can <- UKKC[grep("weakDistancingNow|strictDistancingNow",UKKC)] 
      chbx <- chbx %>% c(can)
    } 
    
    df <- df[,c(chbx)]
    
    # nl <- as.numeric(match("No_Intervention",names(df)))
    # maxy <- suppressWarnings( max(df[,-as.numeric(nl)], na.rm=TRUE) + 
    #                               ( max(df[,-as.numeric(nl)], na.rm=TRUE) * 0.05)
    # )
    
    colors <- c("No Intervention"= "black",
                "IHME Model" = "#023858", 
                "CAN: Shelter in Place" = "#02818a", 
                "CAN: Delay/Distancing" = "#238443",
                
                'JHU: NPIs 30-40% Effective' = "#d7301f",
                'JHU: NPIs 40-50% Effective' = "#238b45",
                'JHU: NPIs 50-60% Effective' = "#4d004b",
                'JHU: NPIs 60-70% Effective' = "#67001f",
                
                "JHU: Continuing Lockdown" = "#d7301f",
                'JHU: Slow-paced Reopening' = "#238b45",
                'JHU: Moderate-paced Reopening' = "#4d004b",
                'JHU: Fast-paced Reopening' = "#67001f",
                
                #"Total Confirmed Cases" = "red",
                "Total Deaths" = "black",
                "Patients Positive for COVID-19" = "black",
                "ICU Patients Positive for COVID-19" = "black"
                #"Positive + Suspected Patients" = "green",
                #"Positive + Suspected ICU" = "blue"
    )
    
    #test_colors <- c("Continued_Lockdown" = "#d7301f")
    
    p <- ggplot() 
    
    if (input$selected_crosswalk == "1" & input$drop_hline == TRUE & input$county_ts == state_name) { 
      
      p <- p + geom_line(df, mapping = aes(x= Index, y = 50000), color = "black", linetype = "dashed") + 
        geom_text(aes(x = as.Date(input$physical.graph_date_window[[1]]), y= 50000, 
                      label = "Phase 1 Surge Capacity"), 
                  hjust = -0.1,
                  vjust = -0.3) 
    } else {
      
      if ( input$selected_crosswalk == "1" & !is.na(total.cnty.beds()) == TRUE ) { 
        p <- p + geom_line(df, mapping = aes(x= Index, y = total.cnty.beds()), color = "black", linetype = "dashed") + 
          geom_text(aes(x = as.Date(input$physical.graph_date_window[[1]]), y= total.cnty.beds(), 
                        label = "Total Licensed Beds"), 
                    hjust = -0.1,
                    vjust = -0.3) 
      }
    }
    
    #if ( "No_Intervention" %in% c(chbx) ) { p <- p + geom_line(df, mapping = aes(x = Index, y = No_Intervention), color = "black", size = 1.5, linetype = "dashed") }
    
    ### JHU Scenarios
    
    if ( TRUE %in% grepl(jhu.no, chbx))  {
      chbx.M <- chbx[grep(jhu.no,chbx)]
      chbx.M <- unique(str_remove(chbx.M, "\\.[MLH]"))
      for (scenario in chbx.M) { 
        c <- as.character(colors[match(names(modellist[match(scenario,modellist)]),names(colors))])
        if ( scenario %in% c(chbx)   ) { p <- p + geom_line(df, mapping = aes_string(x="Index", y=scenario, color = shQuote(names(modellist[match(scenario,modellist)])) ), size = 1.5, linetype = "solid") }
        if ( paste0(scenario,".M") %in% c(chbx)   ) { p <- p + geom_line(df, mapping = aes_string(x="Index", y=paste0(scenario,".M"), color = shQuote(names(modellist[match(scenario,modellist)])) ), size = 1.5, linetype = "solid") }
        if ( paste0(scenario,".L") %in% c(chbx) ) { p <- p + geom_ribbon(df, mapping = aes_string(x ="Index", ymin = paste0(scenario,".L"), ymax = paste0(scenario,".H") ), fill=c, color = c, alpha = 0.2) }
      }
    }
    
    ### Other Models/Scenarios
    
    if ( "IHME_sts" %in% c(chbx) ) { p <- p + geom_line(df, mapping = aes(x=Index, y=IHME_sts, color = "IHME Model"),  size = 1.5, linetype = "solid") }
    if ( "IHME_sts.L" %in% c(chbx) ) { p <- p + geom_ribbon(df, mapping = aes(x = Index, ymin = IHME_sts.L, ymax = IHME_sts.H), fill="#a6bddb", color = "#a6bddb", alpha = 0.2) }
    
    if ( "strictDistancingNow" %in% c(chbx)   ) { p <- p + geom_point(df, mapping = aes(x=Index, y=strictDistancingNow, color = "CAN: Shelter in Place") ) }
    if ( "weakDistancingNow" %in% c(chbx)     ) { p <- p + geom_point(df, mapping = aes(x=Index, y=weakDistancingNow, color = "CAN: Delay/Distancing") )   }
    
    ### Actuals
    
    if ( "Total.Count.Deaths" %in% c(chbx) ) {p <- p + geom_point(df, mapping = aes(x = Index, y = Total.Count.Deaths,  color = "Total Deaths"), shape = 15, fill = "black", size = 3 ) }
    if ( "COVID.19.Positive.Patients" %in% c(chbx) ) {p <- p + geom_point(df, mapping = aes(x = Index, y = COVID.19.Positive.Patients, color = "Patients Positive for COVID-19"), shape = 23, fill = "black", size = 3 ) }
    if ( "ICU.COVID.19.Positive.Patients" %in% c(chbx) ) {p <- p + geom_point(df, mapping = aes(x = Index, y = ICU.COVID.19.Positive.Patients,  color = "ICU Patients Positive for COVID-19"), shape = 19, fill = "black", size = 3 ) }
    
    # if ( input$overlay_uncontrolled == TRUE ) {
    #     p <- p + scale_y_continuous(labels = scales::comma, limits = c(0, as.numeric(maxy)) ) 
    # }  else {
    p <- p + scale_y_continuous(labels = scales::comma) 
    #}
    
    p <- p + labs(x = "Date", 
                  y = switch(input$selected_crosswalk,
                             "1" = "Hospital Bed Occupancy",
                             "2" = "ICU Bed Occupancy",
                             "3" = "Cumulative Deaths"),
                  color = "Legend") + scale_color_manual(values = colors) +
      
      ggtitle(switch(input$selected_crosswalk,
                     "1" = paste0(input$county_ts," COVID Hospitalizations"),
                     "2" = paste0(input$county_ts," COVID ICU Patients"),
                     "3" = paste0(input$county_ts," COVID Cumulative Deaths")
      )) +
      
      theme(plot.title = element_text(size = 18, face = "bold"),
            axis.title = element_text(face = "bold", size = 18, colour = "black"),
            axis.text.x = element_text(face = "bold", color = "black", size = 18),
            axis.text.y = element_text(face = "bold", color = "black", size = 18),
            axis.line = element_line(color = "black", size = 1, linetype = "solid"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            legend.text=element_text(size=14),
            legend.position = "bottom"
            
      ) 
    
    return(p)
    
  })
  
  
  ## download Static figure data 
  
  static.plot.data <- reactive({
    df <- state.model.xts()[ paste0( as.Date(input$physical.graph_date_window[[1]]),"/",as.Date(input$physical.graph_date_window[[2]]) ) ]
    #dtrange <- paste(as.character(input$dateRange_ts), collapse = "/")
    
    chbx <- c()
    #### Uncontrolled + Actuals
    if ( input$actuals == TRUE) {chbx <- c(chbx,c(input$select_COVID)) }
    
    UKKC <- as.character(input$include_JHU_UKKC)
    
    if ( TRUE %in% grepl("UK.\\w+.\\d+_\\d+|.\\w+_\\w{4,}", UKKC) & input$physical.mmd == "M" ) {
      JHU_list <- UKKC[grep("UK.\\w+.\\d+_\\d+|.\\w+_\\w{4,}",UKKC)]
      chbx <- c(chbx, c(JHU_list) )
    } else {
      JHU_list <- UKKC[grep("UK.\\w+.\\d+_\\d+|.\\w+_\\w{4,}",UKKC)]
      chbx <- c(chbx, c( as.character(lapply(seq_along(JHU_list), function(i) { paste0(as.character( JHU_list[[i]] ),".M" ) } ) ) )  )
    }
    
    if (TRUE %in% grepl("UK.\\w+.\\d+_\\d+|.\\w+_\\w{4,}", UKKC) & input$physical.iqr == TRUE) {
      JHU_list <- UKKC[grep("UK.\\w+.\\d+_\\d+|.\\w+_\\w{4,}",UKKC)]
      chbx <- c(chbx, c( as.character(lapply(seq_along(JHU_list), function(i) {paste0(as.character( JHU_list[[i]] ),".L" ) } )) ),
                c( as.character(lapply(seq_along(JHU_list), function(i) {paste0(as.character( JHU_list[[i]] ),".H" ) } )) ) )
    }
    
    if ( TRUE %in% grepl("IHME_sts", UKKC ) & input$county_ts == state_name ) { 
      chbx <- chbx %>% c("IHME_sts")
    }
    
    if ( TRUE %in% grepl("IHME_sts", UKKC ) & input$IHME.iqr == TRUE & input$county_ts == state_name) { 
      IHME <- "IHME_sts"
      chbx <- c(chbx, c( as.character(lapply(seq_along(IHME), function(i) {paste0(as.character( IHME[[i]] ),".L") } )) ),
                c( as.character(lapply(seq_along(IHME), function(i) {paste0(as.character( IHME[[i]] ),".H") } )) )
      )
    }
    
    if ( TRUE %in% grepl("weakDistancingNow|strictDistancingNow",UKKC) & input$selected_crosswalk != "2") {
      can <- UKKC[grep("weakDistancingNow|strictDistancingNow",UKKC)] 
      chbx <- chbx %>% c(can)
    } 
    
    df <- df[,c(chbx)] %>% data.frame() %>% mutate(Date = seq(as.Date(input$physical.graph_date_window[[1]]),as.Date(input$physical.graph_date_window[[2]]), by = "day"))
    
    df
    
  })
  
  
  output$dlScenario <- downloadHandler(
    filename = function () {
      paste0("COVID_Scenarios_",input$county_ts,".csv")
    },
    
    content = function(file) {
      
      # Title
      t <- c(paste("Long-term COVID Scenarios for ",input$county_ts, sep = ""),rep("",ncol(static.plot.data())-1))
      #Subtitle
      tt <-  c(paste("COVID Assessment Tool - Downloaded on",Sys.Date(), sep = " "),rep("",ncol(static.plot.data())-1))
      #Column labels
      l <- names(static.plot.data())
      
      df <- static.plot.data()
      df[is.na(df)] <- 0
      df[] <- lapply(df, as.character)
      
      #Source
      s <- c("Please see the Technical Notes tab of the application for data sources.",rep("",ncol(static.plot.data())-1))
      p <- c(paste0("Prepared by: ",state_name," Department of Public Health"), rep("",ncol(static.plot.data())-1))
      
      dlm <- rbind(t, tt, l, df, s, p)
      write.table(dlm, file, row.names = F, col.names = F, quote = F, na= "NA", sep = ",")
      
      #write.csv(df, file, row.names = F)
    }
    
  )
  
  
  
} # End Server
