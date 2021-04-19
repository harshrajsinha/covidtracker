library(shiny)
#library(shinydashboard)
library(highcharter)
library(semantic.dashboard)
library(DT)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  title = "COVID19 INDIA",
  dashboardHeader(title = "COVID19", color="blue", inverted = FALSE, h2("COVID19 INDIA INFOGRAPHICS")),
  dashboardSidebar(
    sidebarMenu(
      id = "mnu",
      menuItem(text = "Dashboard", tabName = "tabDashboard", icon = icon("laptop")),
      menuItem(text = "Canned Report", tabName = "tabCannedAnalysis", icon = icon("chart line")),
      menuItem(text = "Adhoc Analysis", tabName = "tabAdhocAnalysis", icon = icon("snowflake"))
    )
  ),
  dashboardBody(
    includeCSS("www/covid19.css"),    
    tabItems(
      tabItem(
        tabName = "tabDashboard",
        fluidRow(
          width = NULL,
          valueBoxOutput("vbTC", width = 4),
          valueBoxOutput("vbAC", width = 4),
          valueBoxOutput("vbRC", width = 4),
          valueBoxOutput("vbD", width = 4)
        ),
        fluidRow(
          infoBoxOutput("ibTGR", width = 4),
          infoBoxOutput("ibAGR", width = 4),
          infoBoxOutput("ibRR", width = 4),
          infoBoxOutput("ibDR", width = 4)
        ),
        fluidRow(
          box(
            title = "India",  
            color = "green",
            ribbon = TRUE,
            title_side = "top right",
            highchartOutput("hcCountryTrend")
          ),
          box(
            color = "red",
            ribbon = TRUE,
            title = "State",
            title_side = "top right",
            highchartOutput("hcStateTopLoc")
            
          )
        ),
        fluidRow(
          box(
            width = 16,
            title = "India",  
            color = "orange",
            ribbon = TRUE,
            title_side = "top right",
            highchartOutput("hcCountryPrec")
          )
        ),
        fluidRow(
          box(
            width = 16,
            color = "olive",
            ribbon = TRUE,
            title = "State",
            title_side = "top right",
            highchartOutput("hcStateTrend")
            
          )
        )
      ),
      tabItem(
        tabName = "tabCannedAnalysis",
        fluidRow(
          box(
            title = "Testing Per Million",  
            color = "green",
            ribbon = TRUE,
            title_side = "top right",
            highchartOutput("hcTestingPerM")
          ),
          box(
            color = "blue",
            ribbon = TRUE,
            title = "Total Testing",
            title_side = "top right",
            highchartOutput("hcTestingTotal")
          )          
        )
      ),
      tabItem(
        tabName = "tabAdhocAnalysis"
      )
    )
  )
)
