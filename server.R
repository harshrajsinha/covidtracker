library(dplyr)
library(highcharter)
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  ## render map
    # mapJson <- fromJSON(file = "data/world_map/country_map/India/in-all.geo.json")
    # 
    # mapdata <- get_data_from_map(map)
    # data_map <- mapdata %>%
    #   select(code = "hc-key") %>% 
    #   mutate(value = 1e5  * abs(rt(nrow(.), df = 10)))
    # mapColor <- "orange"
    # 
    # hc = highchart() %>%
    # hc_add_series_map(mapJson, data_map, value = "value", joinBy = c("hc-key", "code"),
    #                   dataLabels = list(enabled = F, format = '{point.name}')) %>%
    # hc_legend(enabled = F) %>%
    # hc_mapNavigation(enabled = TRUE) %>%
    # hc_exporting(enabled = F) #%>%
    # # hc_tooltip(useHTML = TRUE, 
    # #            headerFormat = "<table>",
    # #            pointFormat = paste("<tr><th colspan = \"1\"><b>",
    # #                                if(regionLevel == 0){ "Country:" } else if(regionLevel == 1)
    # #                                {"State:"} else {"City:"
    # #                                }, "</b><b style=\"color:#1874CD\">{point.name}</b> </th></tr>"
    # #            ),
    # #            footerFormat = "</table>")  
    # output$hcCountryMap <- renderHighchart({hc})
  
    ##Daily Trend India
    hcDailyTrend <- highchart() %>% 
      hc_title(text = "Daily Cumulative Data") %>% 
      hc_chart(type = "column") %>% 
      hc_xAxis(categories = df$reportdate) %>%
      hc_plotOptions(column = list( stacking = "normal" )) %>%
      hc_add_series(
        df$totaldeceased,
        name = "Deaths",
        color = "#db2828",
        dataLabels = list(enabled = TRUE)
      ) %>% 
      hc_add_series(
        df$totalactive,
        name = "Active",
        color = "#fbbd08",
        dataLabels = list(enabled = TRUE)
      ) %>% 
      hc_add_series(
        df$totalrecovered,
        name = "Recovered",
        color = "#21ba45",
        dataLabels = list(enabled = TRUE)
      )
  
    output$hcCountryTrend <- renderHighchart({hcDailyTrend})
    
    ##State wise trend
    confirmed_state_data  = state_daily %>% filter(Date == maxStateDate, Status == "Confirmed")
    confirmed_state_data = confirmed_state_data[, -(1:3)]
    recovered_state_data  = state_daily %>% filter(Date == maxStateDate, Status == "Recovered")
    recovered_state_data = recovered_state_data[, -(1:3)]
    deceased_state_data  = state_daily %>% filter(Date == maxStateDate, Status == "Deceased")
    deceased_state_data = deceased_state_data[, -(1:3)]
    state_list <- colnames(state_daily)
    state_list <- state_list[ -(1:3)] ## remove  Date, Status, TT column
    
    
    hcStateDailyTrend <- highchart() %>% 
      hc_title(text = "Statewise Daily Data") %>% 
      hc_chart(type = "column") %>% 
      hc_xAxis(categories = state_list) %>%
      hc_add_series(
        unlist(unname(deceased_state_data)),
        name = "Deaths",
        color = "#db2828",
        dataLabels = list(enabled = TRUE)
      ) %>% 
      hc_add_series(
        unlist(unname(recovered_state_data)),
        name = "Recovered",
        color = "#21ba45",
        dataLabels = list(enabled = TRUE)
      ) %>% 
      hc_add_series(
        unlist(unname(confirmed_state_data)),
        name = "Confirmed",
        color = "#fbbd08",
        dataLabels = list(enabled = TRUE)
      )
    
    output$hcStateTrend <- renderHighchart({hcStateDailyTrend})
    
    maxdf = df %>% filter(reportdate == max(df$reportdate))
    act = maxdf$totalconfirmed - (maxdf$totaldeceased + maxdf$totalrecovered)
    acd = maxdf$dailyconfirmed - (maxdf$dailydeceased + maxdf$dailyrecovered)
    
    ##Value Box
    output$vbTC <- renderValueBox({
        valueBox(width = 4, subtitle = "Total Cases", value = format(round(as.numeric(maxdf$totalconfirmed), 0), nsmall=0, big.mark=",") , color = "yellow", icon = icon("group icon-yellow"))
    })
    output$vbAC <- renderValueBox({
        valueBox(width = 4, subtitle = "Active Cases", value = format(round(as.numeric(act), 0), nsmall=0, big.mark=","), color = "blue", icon = icon("user outline icon-blue"))
    })
    output$vbRC <- renderValueBox({
        valueBox(width = 4, subtitle = "Recovered Cases", value = format(round(as.numeric(maxdf$totalrecovered), 0), nsmall=0, big.mark=","), color = "green", icon = icon("user circle icon-green"))
    })
    output$vbD <- renderValueBox({
        valueBox(width = 4, subtitle = "Deaths", value = format(round(as.numeric(maxdf$totaldeceased), 0), nsmall=0, big.mark=","), color = "red", icon = icon("user icon-red"))    
    })    
    
    ##Info Box
    percTGR = round((maxdf$dailyconfirmed/ maxdf$totalconfirmed)*100,2)    
    percAGR = round((act/maxdf$totalconfirmed)*100,2)
    percRR = round((maxdf$totalrecovered/maxdf$totalconfirmed)*100, 2)
    percDR = round((maxdf$dailydeceased/maxdf$totaldeceased)*100, 2)
    output$ibTGR <- renderValueBox({
      infoBox(size= "small", width = 4, subtitle = "Total Growth Rate", value = paste0(as.character(percTGR),"%"), color = "yellow")
    })
    output$ibAGR <- renderValueBox({
      infoBox(size= "small", width = 4, subtitle = "Active %", value = paste0(as.character(percAGR),"%"), color = "orange")
    })
    output$ibRR <- renderValueBox({
      infoBox(size= "small", width = 4, subtitle = "Recovery %", value = paste0(as.character(percRR),"%"), color = "green")
    })
    output$ibDR <- renderValueBox({
      infoBox(size= "small", width = 4, subtitle = "Death %", value = paste0(as.character(percDR),"%"), color = "red")
    })   
    
    
    df$dailyconfirmedprec = round((df$dailyconfirmed/df$totalconfirmed)*100, 1)
    df$dailyactiveprec = round((df$dailyactive/df$totalactive)*100, 1)
    df$dailyrecoveredprec = round((df$dailyrecovered/df$totalrecovered)*100, 1)
    df$dailydeathprec = round((df$dailydeceased/df$totaldeceased)*100, 1)
    
    df <- within(df, dailyconfirmedprec[dailyconfirmedprec < 0] <- 0)
    df <- within(df, dailyactiveprec[dailyactiveprec < 0] <- 0)
    df <- within(df, dailyrecoveredprec[dailyrecoveredprec < 0] <- 0)
    df <- within(df, dailydeathprec[dailydeathprec < 0] <- 0)
    
    ## National trend analysis
    hcCountryPrec <- highchart() %>% 
      hc_title(text = "Daily Trend Analysis %") %>% 
      hc_chart(type = "line") %>% 
      hc_xAxis(categories = df$reportdate) %>%
      hc_plotOptions(column = list( stacking = "normal" )) %>%
      hc_add_series(
        df$dailydeathprec,
        name = "Deaths",
        color = "#db2828",
        dataLabels = list(enabled = TRUE)
      ) %>% 
      hc_add_series(
        df$dailyrecoveredprec,
        name = "Recovered",
        color = "#21ba45",
        dataLabels = list(enabled = TRUE)
      ) %>% 
      hc_add_series(
        df$dailyactiveprec,
        name = "Active",
        color = "#fbbd08",
        dataLabels = list(enabled = TRUE)
      )  %>% 
      hc_add_series(
        df$dailyconfirmedprec,
        name = "Confirmed",
        color = "#2185d0",
        dataLabels = list(enabled = TRUE)
      )    
    
      output$hcCountryPrec <- renderHighchart({hcCountryPrec})    
      
      ## Top 5 state Active/Confirmed/Recovered
      active_state <- head(state_max[order(-state_max$Confirmed),], 5)
      
      hcStateTop5 <- highchart() %>% 
        hc_title(text = "Daily Confirmed Cases (Top 5 States)") %>% 
        hc_chart(type = "bar") %>% 
        hc_xAxis(categories = active_state$State) %>%
        hc_plotOptions(column = list( stacking = "normal" )) %>%
        hc_add_series(
          active_state$Confirmed,
          name = "Confirmed",
          color = "#db2828",
          dataLabels = list(enabled = TRUE)
        )      
      
      output$hcStateTopLoc <- renderHighchart({hcStateTop5})  
      
      ## Testing per million / total testing
      
      hcTesting <- highchart() %>% 
        hc_title(text = "Testing Per Million") %>% 
        hc_chart(type = "column") %>% 
        hc_xAxis(categories = df_testing$testedasof) %>%
        hc_plotOptions(column = list( stacking = "normal" )) %>%
        hc_add_series(
          df_testing$testspermillion,
          name = "Testing Per Million",
          color = "#21ba45",
          dataLabels = list(enabled = TRUE)
        ) 
      
      output$hcTestingPerM <- renderHighchart({hcTesting})    
      
      
      hcTestingTotal <- highchart() %>% 
        hc_title(text = "Total Testing") %>% 
        hc_chart(type = "column") %>% 
        hc_xAxis(categories = df_testing$testedasof) %>%
        hc_plotOptions(column = list( stacking = "normal" )) %>%
        hc_add_series(
          df_testing$totalsamplestested,
          name = "Total Sample Tested",
          color = "#2185d0",
          dataLabels = list(enabled = TRUE)
        )      
      
      output$hcTestingTotal <- renderHighchart({hcTestingTotal})    
      
      
}