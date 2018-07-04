####################################################
# Shiny demo - ui
# 2018-06-06
# Author: Dr. Albert Blarer
# Demo HSLU
####################################################

library(shinydashboard)
library(DT)
library(plotly)
library(networkD3)

sidebar <- dashboardSidebar(
  hr(),
  sidebarMenu(id="tabs",
              menuItem("Data Access", tabName="dataaccess", icon=icon("file-o"), selected=TRUE),
              menuItem("Data View", tabName="dataview", icon=icon("table"),
                menuSubItem("Tabular View", tabName="tableview")),
              menuItem("Time Domain", tabName="timedomain", icon=icon("line-chart"),
                menuSubItem("Time Series", tabName="timeseries")),
              menuItem("Frequency Domain", tabName="frequencydomain", icon=icon("bar-chart"),
                menuSubItem("Periodogram", tabName="periodogram")),
              menuItem("Network Analysis", tabName="networkanalysis", icon=icon("connectdevelop"),
                menuSubItem("Centralities", tabName="centralities"),
                menuSubItem("Community Detection", tabName="communitydetection"))
  )
)

body <-  dashboardBody(
  # include the custom.css
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tabItems(
    
    tabItem(tabName = "dataaccess",
            tags$style(".nav-tabs {
                          background-color: #fff;
                       }
                       .nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
                       background-color: transparent;
                       }
                       .nav-tabs-custom .nav-tabs li.active {
                       border-top-color: #D3342C;
                       }"),
            fluidRow(
              column(12,
              tabBox(
                title = "Data Source",
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset1", height = "250px", width = "100%",
                tabPanel("CSV",
                         #Selector for file upload
                         fileInput("csvfile", "Choose CSV File",
                                   accept = c(
                                     "text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv")
                         ),
                         tags$hr(),
                         checkboxInput("header", "Header", TRUE)
                ),
                tabPanel("SOLR", "to be implemented ..."),
                tabPanel("IMPALA", "to be implemented ..."),
                tabPanel("HADOOP", "to be implemented ...")
              )
            )
            )
    ),
    
    tabItem(tabName = "tableview",
            h4("Table"),
            br(),
            fluidRow(
              column(12,
                     div(
                       style = 'overflow-x: scroll',
                       DT::dataTableOutput("csvData")
                     ))
            )
    ),
    
    tabItem(tabName = "timeseries",
            h4("Temporal Visualization"),
            br(),
            fluidPage(
              fluidRow(
                column(2,
                       radioButtons("aggLevel", label = h4("Aggregation level:"),
                                    choices = list("hours" = 3600, "days" = 86400), 
                                    selected = 86400)
                       ),
                column(10, plotlyOutput(outputId = "timeSeries", height = "600px"))
              )
            )
    ),
    
    tabItem(tabName = "periodogram",
            h4("Frequency Visualization"),
            br(),
            fluidPage(
              fluidRow(
                column(12, plotlyOutput(outputId = "plotLSSA", height = "600px"))
              )
            )
    ),
    
    tabItem(tabName = "centralities",
            tags$style(".nav-tabs {
                       background-color: #fff;
                       }
                       .nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
                       background-color: transparent;
                       }
                       .nav-tabs-custom .nav-tabs li.active {
                       border-top-color: #D3342C;
                       }"),
            fluidPage(
              h4("Network Centralities"),
              fluidRow(
              column(12,
                     dateRangeInput('dateRangeOfCentrality',
                                    label = paste('Select a time (day) period:'),
                                    start = as.Date('2016-10-16'), end = as.Date('2016-10-16'),
                                    min = as.Date('2016-10-11'), max = as.Date('2016-11-08'),
                                    separator = " to ", format = "yyyy-mm-dd",
                                    startview = 'month', language = 'en', weekstart = 1
                     ),
                     br(),
                     actionButton("go1Button", "Calculate Centralities"),
                     hr(),
                     tabBox(
                       # The id lets us use input$tabset1 on the server to find the current tab
                       id = "tabset1", height = "250px", width = "100%",
                       tabPanel("Degree",
                                div(
                                  style = 'overflow-x: scroll',
                                  DT::dataTableOutput("allDegreeData")
                                )
                       ),
                       tabPanel("In-Degree", 
                                div(
                                  style = 'overflow-x: scroll',
                                  DT::dataTableOutput("inDegreeData")
                                )
                       ),
                       tabPanel("Out-Degree", 
                                div(
                                  style = 'overflow-x: scroll',
                                  DT::dataTableOutput("outDegreeData")
                                )
                       ),
                       tabPanel("Beetweenness", 
                                div(
                                  style = 'overflow-x: scroll',
                                  DT::dataTableOutput("btwCentralityData")
                                )
                       ),
                       tabPanel("Closeness", 
                                div(
                                  style = 'overflow-x: scroll',
                                  DT::dataTableOutput("cloCentralityData")
                                )
                       ),
                       tabPanel("Eigenvector", 
                                div(
                                  style = 'overflow-x: scroll',
                                  DT::dataTableOutput("evcCentralityData")
                                )
                       )
                     )
              )
            ))
    ),
    
    tabItem(tabName = "networkgraph",
            h4("Graph"),
            fluidPage(
              fluidRow(
                
              )
            )
    ),
    
    tabItem(tabName = "communitydetection",
            fluidPage(
              h4("Mention Graph using the Louvain Algorithm"),
              fluidRow(
                column(12,
                  dateRangeInput('dateRangeOfCommunity',
                               label = paste('Select a time (day) period:'),
                               start = as.Date('2016-10-11'), end = as.Date('2016-10-13'),
                               min = as.Date('2016-10-11'), max = as.Date('2016-11-08'),
                               separator = " to ", format = "yyyy-mm-dd",
                               startview = 'month', language = 'en', weekstart = 1
                  ),
                  br(),
                  actionButton("go2Button", "Calculate Community"),
                  hr(),
                        forceNetworkOutput("communityGraph", width = "100%", height = "700px")
                  )
              )
            )
    ),
    tabItem(tabName = "Geomap",
            h4("geomapTitle"),
            br(),
            fluidPage(
              fluidRow(
                
              )
            )
    )
  )
)

dashboardPage(
  skin = "red",
  dashboardHeader(title = "Demo HSLU"),
  sidebar,
  body
)