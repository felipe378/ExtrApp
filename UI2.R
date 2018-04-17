require(shiny)
require(bootstrap)
require(jpeg)
require(ggplot2)
require(ggExtra)
require(ggdendro)
require(DT)
require(stringr)
require(gsubfn)
require(proto)
require(sqldf)
require(shinyjs)
require(shinyBS)
require(shinydashboard)
require(plotly)
require(googleVis)
require(lubridate)

ui<-dashboardPage(
  dashboardHeader(title = "SIBYL - Extrusion Application",
                  titleWidth = 350
  ),
  
  dashboardSidebar(
    width = 350,
    sidebarMenu(id = "tabs",
                menuItem("Welcome", tabName = "welcome", icon = icon("home")),
                menuItem("Part Catalog - PPS Data", tabName = "partcatalog", icon = icon("list")),
                menuItem("Sampling and Test Method Information", tabName = "samplinginformation", icon = icon("wrench")),
                menuItem("MES & SAP Batch Data", tabName = "mesdata", icon = icon("cogs")),
                menuItem("Shopping Cart PPS Data", tabName = "shoppingcartdata"),
                menuItem("Analysis Tool", tabName = "analysistool", icon=icon("bar-chart")),
                menuItem("Help", tabName = "help", icon = icon("question-circle-o"))
    ) #end sidebarMenu
    
  ), #end dashboardSidebar
  
  
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "welcome",
              fluidRow(
                box(title = "Introduction", solidHeader = TRUE, status = "info", width = 12,
                    tags$p(HTML(paste(readLines("IntroductionText.txt"), collapse = "<br>")))
                )#end box
              ),#end fluidRow
              fluidRow(
                box(title = "About the Part Catalog", solidHeader = TRUE, status = "primary", width = 12,
                    tags$p(HTML(paste(readLines("PartCatalogText.txt"), collapse = "<br>")))
                )#end box
              ),#end fluidRow
              fluidRow(
                box(title = "About the Shopping Cart", solidHeader = TRUE, status = "info", width = 12,
                    tags$p(HTML(paste(readLines("ShoppingCartText.txt"), collapse = "<br>")))
                )#end box
              ),#end fluidRow
              fluidRow(
                box(title = "About the MES Batch Information", solidHeader = TRUE, status = "primary", width = 12,
                    tags$p(HTML(paste(readLines("MESBatchInformationText.txt"), collapse = "<br>")))
                )#end box
              ),#end fluidRow
              fluidRow(
                box(title = "About the Scrap Codes", solidHeader = TRUE, status = "info", width = 12,
                    tags$p(HTML(paste(readLines("ScrapRatesAndCodesText.txt"), collapse = "<br>")))
                )#end box
              )#end fluidRow
      ), #end tabItem
      tabItem(tabName = "partcatalog",
              fluidRow(
                column(12,offset = 0, style='padding:0px;',
                       box(title = "Windchill Information", 
                           solidHeader = TRUE, status = "primary", collapsible = TRUE, 
                           width = 2,
                           selectInput("windchill_select", 
                                       label = NULL,
                                       choices = filterkey[Filter.Type == "Windchill", Filter],
                                       multiple = TRUE,
                                       selectize = TRUE,
                                       selected = c("Part Number", "Part Description")
                           )
                       ),
                       box(title = "Resin Information", 
                           solidHeader = TRUE, status = "primary", collapsible = TRUE, 
                           width = 2,
                           selectInput("resin_select", 
                                       label = NULL,
                                       choices = filterkey[Filter.Type == "Resin", Filter],
                                       multiple = TRUE,
                                       selectize = TRUE,
                                       selected = NULL
                           )
                       ),
                       box(title = "Tooling Information", 
                           solidHeader = TRUE, status = "primary", collapsible = TRUE, 
                           width = 2,
                           selectInput("tooling_select", 
                                       label = NULL,
                                       choices = filterkey[Filter.Type == "Tooling", Filter],
                                       multiple = TRUE,
                                       selectize = TRUE,
                                       selected = NULL
                           )
                       ),
                       box(title = "Parameter Information", 
                           solidHeader = TRUE, status = "primary", collapsible = TRUE, 
                           width = 2,
                           selectInput("parameter_select", 
                                       label = NULL,
                                       choices = filterkey[Filter.Type == "Parameter", Filter],
                                       multiple = TRUE,
                                       selectize = TRUE,
                                       selected = NULL
                           )
                       ),
                       box(title = "Attribute Information", 
                           solidHeader = TRUE, status = "primary", collapsible = TRUE, 
                           width = 2,
                           selectInput("attribute_select", 
                                       label = NULL,
                                       choices = filterkey[Filter.Type == "Attribute", Filter],
                                       multiple = TRUE,
                                       selectize = TRUE,
                                       selected = c("Inner Diameter (in)", "Outer Diameter (in)",
                                                    "Length (in)")
                           )
                       ),
                       box(title = "Special Information", 
                           solidHeader = TRUE, status = "primary", collapsible = TRUE, 
                           width = 2,
                           selectInput("special_select", 
                                       label = NULL,
                                       choices = filterkey[Filter.Type == "Special", Filter],
                                       multiple = TRUE,
                                       selectize = TRUE,
                                       selected = NULL
                           )
                       )
                )
                
              ), #end fluidRow
              
              fluidRow(
                #this will contain the values of the filters
                uiOutput("windchill_filters")
              ), #end fluidRow
              
              fluidRow(
                DT::dataTableOutput("catalog_table")
              ) #end fluidRow
              
              
              
      ) #end tabItem
    )#end tabItems
  ) #end DashBoard Body
)#end UI
