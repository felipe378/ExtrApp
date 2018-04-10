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

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "SIBYL - Extrusion Application",
                 titleWidth = 350
  ),
  
  dashboardSidebar(
    width = 350,
    sidebarMenu(id = "tabs",
                menuItem("Test Analysis", tabName = "testanalysis", icon = icon("home"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "testanalysis", 
              ###this is the test analysis tab for analyzing the MES Data
              fluidRow(
                valueBoxOutput("sapbatchboxoutput", width = 3),
                valueBoxOutput("materialboxoutput", width = 3),
                valueBoxOutput("lineboxoutput", width = 3),
                valueBoxOutput("operatorboxoutput", width = 3)
              ), #end fluidRow for value boxes
              fluidRow(
                valueBoxOutput("startdateboxoutput", width = 3),
                valueBoxOutput("enddateboxoutput", width = 3),
                valueBoxOutput("monthlyrunsboxoutput", width = 3),
                valueBoxOutput("averageyieldboxoutput", width = 3)
              ), #end fluidRow for value boxes
              fluidRow(
                column(
                  width = 4,
                  wellPanel(#a well panel to store the box for choosing the data
                    id = "datawellpanel", style = "overflow-y:scroll; height: 600px",
                    box(title = "Choose Data and Graph", solidHeader = TRUE, 
                        status = "primary", collapsible = FALSE, width = 12,
                        #radio button to choose the data set
                        radioButtons(inputId = "dataset",
                                     #'this controls the data set a user chooses
                                     label = "Choose a Data Set to Analyze",
                                     choiceNames = c("Single", "Multi","Tapered"),
                                     choiceValues = c(1,2,3)
                        ),
                        conditionalPanel(
                          #only appears once a data set has been chosen
                          condition = "input.dataset",
                          radioButtons(inputId = "graphpackage",
                                       #'this controls what type of graphs are available
                                       label = "Select a Chart/Graph Type",
                                       choiceNames = c("GGplot2"),
                                       choiceValues = c(2)
                          )
                        ),#end conditionPanel of PD Custom
                        #'the selectinputs for the graph type are all the same, which package
                        #'is used to render the graph is determined by the choice value for the graph
                        #'it follows this pattern:
                        #'first integer -> graph group (1 = google, 2 = ggplot2, 3 = plotly)
                        #'second integeger -> special or non special graph (0 = not-special,
                        #'2 = dendrogram, 3 = parallel coordinates, 4 = google motion chart)
                        #'third integer -> degrees of freedom
                        #'fourth integer -> is color a degree of freedom (0 = no, 1 = yes)
                        #'fifth integer -> is size a degree of freedom (0 = no, 1 = yes)
                        #'sixth integer -> does the graph allow multiple traces (0 = no, 1 = yes)
                        #'7 and 8 integer -> graph ID (currently starts at 06).
                        
                        uiOutput("graphchoiceoutput") #the choice for the user of which graph
                        
                    )#end box for choosing data set and graph
                  )#end wellPanel
                  
                ),#end column
                column(
                  #'this column will contain the parameters needed to plot the graph such as
                  #'defining the axes, defining the id, etc.
                  width = 4,
                  wellPanel(#a well panel to store the box for choosing the data
                    id = "datawellpanel", style = "overflow-y:scroll; height: 600px",
                    box(title = "Select the Parameters for the Graph", solidHeader = TRUE, 
                        status = "primary", collapsible = FALSE, width = 12,
                        uiOutput("graphaxeshtmloutput")
                    )#end box
                  )#end wellPanel
                ),#end column
                column(
                  #'this column will contain the filters for the data
                  width = 4,
                  wellPanel(#a well panel to store the box for filtering the data
                    id = "filterwellpanel", style = "overflow-y:scroll; max-height: 600px",
                    box(title = "Material Filter", solidHeader = TRUE, 
                        status = "info", collapsible = TRUE, width = 12, collapsed = TRUE,
                        #radio button choose whether the filter is allowed to be used
                        #by default the filter will not eb used
                        radioButtons(inputId = "usematerialfilter",
                                     label = "Do you want to use this filter?",
                                     choices = list("Yes" = 1, "No" = 0),
                                     selected = "0"),
                        conditionalPanel(#appears if the filter will be used
                          condition = "input.usematerialfilter == '1'",
                          #if the user has selected to use the material filter
                          radioButtons(inputId = "includeexcludematerial",
                                       label = "Include or Exclude the Choices?",
                                       choices = list("Include" = 1, "Exclude" = 0),
                                       selected = "1"),
                          uiOutput("materialfilterchoicesoutput")
                          )#end conditionPanel
                        ), #end box for Material Filter
                    box(title = "SAP Batch Filter", solidHeader = TRUE, 
                        status = "info", collapsible = TRUE, width = 12, collapsed = TRUE,
                        #radio button choose whether the filter is allowed to be used
                        #by default the filter will not eb used
                        radioButtons(inputId = "usebatchfilter",
                                     label = "Do you want to use this filter?",
                                     choices = list("Yes" = 1, "No" = 0),
                                     selected = "0"),
                        conditionalPanel(#appears if the filter will be used
                          condition = "input.usebatchfilter == '1'",
                          #if the user has selected to use the material filter
                          radioButtons(inputId = "includeexcludebatch",
                                       label = "Include or Exclude the Choices?",
                                       choices = list("Include" = 1, "Exclude" = 0),
                                       selected = "1"),
                          uiOutput("batchfilterchoicesoutput")#render the choices for the batch numbers
                        )#end conditionPanel
                    ),#end Box for SAP Batch filter
                    box(title = "Line Filter", solidHeader = TRUE, 
                        status = "info", collapsible = TRUE, width = 12, collapsed = TRUE,
                        #radio button choose whether the filter is allowed to be used
                        #by default the filter will not eb used
                        radioButtons(inputId = "uselinefilter",
                                     label = "Do you want to use this filter?",
                                     choices = list("Yes" = 1, "No" = 0),
                                     selected = "0"),
                        conditionalPanel(#appears if the filter will be used
                          condition = "input.uselinefilter == '1'",
                          #if the user has selected to use the material filter
                          radioButtons(inputId = "includeexcludeline",
                                       label = "Include or Exclude the Choices?",
                                       choices = list("Include" = 1, "Exclude" = 0),
                                       selected = "1"),
                          uiOutput("linefilterchoicesoutput")
                        )#end conditionPanel
                    ),
                    box(title = "Date Range Filter", solidHeader = TRUE, 
                        status = "info", collapsible = TRUE, width = 12, collapsed = TRUE,
                        #radio button choose whether the filter is allowed to be used
                        #by default the filter will not eb used
                        radioButtons(inputId = "usedatefilter",
                                     label = "Do you want to use this filter?",
                                     choices = list("Yes" = 1, "No" = 0),
                                     selected = "0"),
                        conditionalPanel(#appears if the filter will be used
                          condition = "input.usedatefilter == '1'",
                          #if the user has selected to use the material filter
                          radioButtons(inputId = "includeexcludedate",
                                       label = "Include or Exclude the Choices?",
                                       choices = list("Include" = 1, "Exclude" = 0),
                                       selected = "1"),
                          uiOutput("daterangefilteroutput")
                        )#end conditionPanel
                    ),
                    box(title = "Choose a Column to Filter", solidHeader = TRUE, 
                        status = "info", collapsible = TRUE, width = 12, collapsed = TRUE,
                        radioButtons(inputId = "usefirstcolumnfilter",
                                     label = "Do you want to use this filter?",
                                     choices = list("Yes" = 1, "No" = 0),
                                     selected = "0"),
                        conditionalPanel(#appears if the filter will be used
                          condition = "input.usefirstcolumnfilter == '1'",
                          uiOutput("firstcolumnchoiceoutput"),
                          selectInput(inputId = "firstcolumnoperator",
                                      #user selects operator
                                      label = "Select an Operator",
                                      choices = list("None" = 0,
                                                     "Inequality" = 1, 
                                                     "Range" = 2, 
                                                     "Matching Values" = 3
                                      ),
                                      selected = "1"
                          ),
                          conditionalPanel(#user has selected Inequality
                            condition = "input.firstcolumnoperator == '1'",
                            fluidRow(
                              column(width = 6,
                                     selectInput(inputId = "firstcolumninequality",
                                                 #user selects an equality function
                                                 label = "",
                                                 choices = list("X <" = 1,
                                                                "X <=" = 2, 
                                                                "X >=" = 3, 
                                                                "X >" = 4
                                                 )
                                     )
                              ),#end column
                              column(width = 6,
                                     textInput(inputId = "firstcolumninequalityinput" ,
                                               label = "")  
                              )#end column
                            )#end fluidRow
                            
                          ), #end conditionalPanel
                          conditionalPanel(#user has selected range
                            condition = "input.firstcolumnoperator == '2'",
                            fluidRow(
                              column(width = 6,
                                     selectInput(inputId = "firstcolumnrangemininequality",
                                                 #user selects an equality function
                                                 label = "",
                                                 choices = list("X <" = 1,
                                                                "X <=" = 2, 
                                                                "X >=" = 3, 
                                                                "X >" = 4
                                                 )
                                     )
                              ),#end column
                              column(width = 6,
                                     textInput(inputId = "firstcolumnrangemininput" ,
                                               label = "")  
                              )#end column
                            ),#end fluidRow
                            fluidRow(
                              column(width = 6,
                                     textInput(inputId = "firstcolumnrangemaxinput" ,
                                               label = "")  
                              ), #end column
                              column(width = 6,
                                     selectInput(inputId = "firstcolumnrangemaxinequality",
                                                 #user selects an equality function
                                                 label = "",
                                                 choices = list("X <" = 1,
                                                                "X <=" = 2, 
                                                                "X >=" = 3, 
                                                                "X >" = 4
                                                 )
                                     )
                              ) #end column
                            ) #end fluidRow
                          ), #end conditionalPanel
                          conditionalPanel(#user has selected matching values
                            condition = "input.firstcolumnoperator == '3'",
                            radioButtons(inputId = "firstcolumnvaluesie",
                                         label = "Include or Exclude the Values?",
                                         choices = list("Yes" = 1, "No" = 0),
                                         selected = "1"),
                            uiOutput("firstcolumnfilter")
                            ) #end conditionalPanel 3
                        )#end outer conditionalPanel
                    ),
                    box(title = "Choose a Column to Filter", solidHeader = TRUE, 
                        status = "info", collapsible = TRUE, width = 12, collapsed = TRUE,
                        radioButtons(inputId = "usesecondcolumnfilter",
                                     label = "Do you want to use this filter?",
                                     choices = list("Yes" = 1, "No" = 0),
                                     selected = "0"),
                        conditionalPanel(#appears if the filter will be used
                          condition = "input.usesecondcolumnfilter == '1'",
                          uiOutput("secondcolumnchoiceoutput"),
                          selectInput(inputId = "secondcolumnoperator",
                                      #user selects operator
                                      label = "Select an Operator",
                                      choices = list("None" = 0,
                                                     "Inequality" = 1, 
                                                     "Range" = 2, 
                                                     "Matching Values" = 3
                                      ),
                                      selected = "1"
                          ),
                          conditionalPanel(#user has selected Inequality
                            condition = "input.secondcolumnoperator == '1'",
                            fluidRow(
                              column(width = 6,
                                     selectInput(inputId = "secondcolumninequality",
                                                 #user selects an equality function
                                                 label = "",
                                                 choices = list("X <" = 1,
                                                                "X <=" = 2, 
                                                                "X >=" = 3, 
                                                                "X >" = 4
                                                 )
                                     )
                              ),#end column
                              column(width = 6,
                                     textInput(inputId = "secondcolumninequalityinput" ,
                                               label = "")  
                              )#end column
                            )#end fluidRow
                            
                          ), #end conditionalPanel
                          conditionalPanel(#user has selected range
                            condition = "input.secondcolumnoperator == '2'",
                            fluidRow(
                              column(width = 12,
                                     radioButtons(inputId = "secondcolumnrangeie",
                                                  label = "Include or Exclude the Range?",
                                                  choices = list("Include" = 1, "Exclude" = 0),
                                                  selected = "1")
                              )
                            ),
                            fluidRow(
                              column(width = 6,
                                     selectInput(inputId = "secondcolumnrangemininequality",
                                                 #user selects an equality function
                                                 label = "",
                                                 choices = list("X <" = 1,
                                                                "X <=" = 2, 
                                                                "X >=" = 3, 
                                                                "X >" = 4
                                                 )
                                     )
                              ),#end column
                              column(width = 6,
                                     textInput(inputId = "secondcolumnrangemininput" ,
                                               label = "")  
                              )#end column
                            ),#end fluidRow
                            fluidRow(
                              column(width = 6,
                                     textInput(inputId = "secondcolumnrangemaxinput" ,
                                               label = "")  
                              ), #end column
                              column(width = 6,
                                     selectInput(inputId = "secondcolumnrangemaxinequality",
                                                 #user selects an equality function
                                                 label = "",
                                                 choices = list("X <" = 1,
                                                                "X <=" = 2, 
                                                                "X >=" = 3, 
                                                                "X >" = 4
                                                 )
                                     )
                              ) #end column
                            ) #end fluidRow
                          ), #end conditionalPanel
                          conditionalPanel(#user has selected matching values
                            condition = "input.secondcolumnoperator == '3'",
                            radioButtons(inputId = "secondcolumnvaluesie",
                                         label = "Include or Exclude the Values?",
                                         choices = list("Yes" = 1, "No" = 0),
                                         selected = "1"),
                            uiOutput("secondcolumnfilter")
                          ) #end conditionalPanel 3
                        )#end outer conditionalPanel
                    )
                    
                  )#end wellPanel for filters
                )#end column for filters
              ),#end fluidRow
              fluidRow(
                column(
                  width = 4,
                  wellPanel(#a well panel to store the box for choosing the grouping
                    id = "groupingwellpanel", style = "overflow-y:scroll; height: 600px",
                    box(title = "Choose Data and Graph", solidHeader = TRUE, 
                        status = "primary", collapsible = FALSE, width = 12,
                        radioButtons(inputId = "usemaingroup",
                                     label = "Do you want to group the data?",
                                     choices = list("Yes" = 1, "No" = 0),
                                     selected = "0"),
                        conditionalPanel(#appears if the filter will be used
                          condition = "input.usemaingroup == '1'",
                          #if the user has selected to use the material filter
                          uiOutput("maingroupchoiceoutput"),
                          radioButtons(inputId = "usesubgroup",
                                       label = "Do you want use a secondary grouping for the data?",
                                       choices = list("Yes" = 1, "No" = 0),
                                       selected = "0"),
                          conditionalPanel(#appears if the filter will be used
                            condition = "input.usesubgroup == '1'",
                            uiOutput("subgroupchoiceoutput")
                          )#end conditionPanel
                          
                        )#end conditionPanel
                    )#end Box
                  )#end wellPanel
                ),#end column
                column(
                  width = 8,
                  wellPanel(#a well panel to store the box for choosing the grouping
                    id = "ploteditwellpanel", style = "overflow-y:scroll; height: 600px",
                    tabBox(
                      title = "Edit Plot Formatting",
                      id = "ploteditbox",
                      tabPanel("Edit Axes",
                               radioButtons("changexticks", "Change the Spacing for the X Axis Ticks?",
                                            choices = list("No" = 0, "Yes" = 1),
                                            selected = "0"),
                               conditionalPanel(#appears if the ticks will be changed
                                 condition = "input.changexticks == '1'",
                                 textInput(inputId = "xtickspacing", 
                                           label = "Input the Spacing for the Tick Marks", 
                                           value = "10"
                                           )
                               ),#end conditionPanel
                               radioButtons("xaxis_scale", "Choose a Scale for the X-Axis",
                                            choices = list("Linear" = 1, "Log" = 2),
                                            selected = "1"),
                               
                               radioButtons("changeyticks", "Change the Spacing for the Y Axis Ticks?",
                                            choices = list("No" = 0, "Yes" = 1),
                                            selected = "0"),
                               conditionalPanel(#appears if the ticks will be changed
                                 condition = "input.changeyticks == '1'",
                                 textInput(inputId = "ytickspacing", 
                                           label = "Input the Spacing for the Tick Marks", 
                                           value = "10"
                                 )
                               ),#end conditionPanel
                               radioButtons("yaxis_scale", "Choose a Scale for the Y-Axis",
                                            choices = list("Linear" = 1, "Log" = 2),
                                            selected = "1")
                               ),
                      tabPanel("Edit Legend")
                      ) #end tabbox
                    )#end wellPanel
                )#end column
              ),#end fluidRow for the second row
              fluidRow(
                #fluid row for the analysis
                column(
                  width = 4,
                  wellPanel(#a well panel to store the box for choosing the grouping
                    id = "overlaywellpanel", style = "overflow-y:scroll; height: 600px",
                    box(title = "Choose Data and Graph", solidHeader = TRUE, 
                        status = "primary", collapsible = FALSE, width = 12,
                        radioButtons(inputId = "boxplotinput",
                                     "Would you like to overlay a boxplot?",
                                     choices = list("No" = 0, "Yes" = 1),
                                     selected = "0"
                                     ),
                        radioButtons(inputId = "usejitter",
                                     "Would you like to jitter the points to increase their seperation? (Hover and Color Grouping will not Work)",
                                     choices = list("No" = 0, "Yes" = 1),
                                     selected = "0"
                        )
                    ) #end box
                  )#end wellPanel
                )#end column
              ), #end fluidRow
              fluidRow(
                column(
                  width = 12,
                  dataTableOutput("testdatatable") 
                )
              ),
              fluidRow(
                #fluid row to hold the plots
                column(
                  width = 12,
                  plotOutput("mainplotoutput",
                             hover = hoverOpts(id = "mainplot_hover", delay = 300),
                             brush = brushOpts(
                               id = "mainplot_brush",
                               # delay = 0,
                               # delayType = input$brush_policy,
                               # direction = input$brush_dir,
                               resetOnNew = TRUE)), #end plotui
                  uiOutput("mainplot_hover_info")
                  
                )
              ), #end fluid Row for main plot
              fluidRow(#zoom plot plot
                column(
                  width = 12,
                  plotOutput("zoomplot",
                             hover = hoverOpts(id = "zoomplot_hover", delay = 300)
                             ),
                  uiOutput("zoomplot_hover_info")
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  dataTableOutput("zoomdatatable") 
                )
              ),
              hr(),
              fluidRow(
                column(
                  width = 12,
                  tags$h2("Omitted Data Beacause it was NA or Empty"),
                  dataTableOutput("omitteddatatable") 
                )
              ),
              fluidRow(
                column(
                  width = 3,
                  downloadButton('downloadfiltereddata','Download Filtered Data')
                ),
                column(
                  width = 3,
                  downloadButton('downloadzoomdata','Download Zoom Data')
                ),
                column(
                  width = 3,
                  downloadButton('downloadomitteddata','Download Omitted Data')
                )
              )
              
              
      ) #end tabItem for testanalysis
      
    )#end tabItems
  )#end dasboardBody
   
  
) #end UI

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #### Must be Loaded #### 
  
  listofgraphs <- list(list("Scatter Chart" = 6,
                            "Line Chart" = 7,
                            "Line Chart with 2 Y-Axes" = 8,
                            "Bar Chart (Horzintal Bars)" = 9,
                            "Column Chart (Vertical Bars)" = 10,
                            "Area Chart" = 11,
                            "Stepped Area Chart" = 12,
                            "Bubble Chart" = 14,
                            "Pie Chart" = 15,
                            "Histogram" = 16,
                            "Motion Chart" = 17,
                            "Annotation Time Line Chart" = 18),
                       list("Scatter Plot" = 19,
                            "Counts Plot" = 20,
                            "Area Chart" = 21,
                            "Ordered Bar Chart" = 22,
                            "Histogram" = 23,
                            "Density Plot" = 24,
                            "Box Plot" = 25,
                            "Pie Chart" = 26,
                            "Bubble Plot" = 27,
                            "Tree Map" = 28,
                            "Marginal Histogram" = 29,
                            "Marginal Boxplot" = 30,
                            "Dendrogram" = 31,
                            "Cluster for PCA" = 32),
                       list("Scatter Plot" = 33,
                            "Line Plot" = 34,
                            "Filled Area Plot" = 35,
                            "Box Plot" = 36,
                            "Histogram" = 37,
                            "2D Histogram" = 38,
                            "Bubble Chart" = 39,
                            "Heat Map" = 40,
                            "Stacked Area Plot" = 41,
                            "3D Scatter Plot" = 42,
                            "3D Line Plot" = 43,
                            "3D Mesh Plot" = 44,
                            "3D Mesh Plot" = 45,
                            "Parallel Coordinates Plot" = 46)
                       )
  
  
  
  #### The Actual Code ####
  
  
  plottingdata <- reactiveValues(
    #current data for plotting
    data = tapered_tari_parameter_and_yield_data,
    filtered_data = tapered_tari_parameter_and_yield_data,
    omitted_data = NULL #stores the data remove because the values are NA
  )
  
  output$graphchoiceoutput <- renderUI(
    selectInput(inputId = "graphtype",
                #the graphs available are dependent on the graph package
                label = "Select a Chart/Graph Type",
                choices = listofgraphs[[as.numeric(input$graphpackage)]]
    )
  )
  
  
  graphinformation <- reactiveValues(
    graphaxeshtml = NULL
  )
  
  
  observeEvent(input$graphtype,{
    #for googleplots
    
    graphtypeid <- input$graphtype #gets the graph id that lets the program know what type of graph
    
    #'this will store the html to render the next questions that the user must answer to plot the
    #'data.
    
    
    axeshtml <- switch(graphtypeid,
                       "1" = #not curerntly available
                         Null,
                       "2" = #not curerntly available
                         Null,
                       "3" = #not curerntly available
                         Null,
                       "4" = #not curerntly available
                         Null,
                       "5" = #not curerntly available
                         Null,
                       # "6" = #googleVis Scatter Plot
                       #   tagList(radioButtons("isxcategorical", "Would You Like to have the X-Axis be Categorical (Such as Having the X-Axis be Material Number, Line, Batch, Or Even Columns)",
                       #                        choices = list("No" = 0, "Yes" = 1),
                       #                        selected = "0"),
                       #           conditionalPanel(
                       #             condition = "input.isxcategorical == '0'",
                       #             #if the user does NOT want the x-axis to be categorical
                       #             selectInput("xaxis_data", "Choose Data for the X-Axis",
                       #                         choices = colnames(plottingdata$data),
                       #                         selected = NULL)
                       #           ),
                       #             #if the user does want the x-axis to be categorical
                       #           conditionalPanel(
                       #             condition = "input.isxcategorical == '1'",
                       #             #if the user does want the x-axis to be categorical
                       #             radioButtons("xcategoricalselection", "Select What Grouping You want for the X Axis",
                       #                          choices = list("Material Number" = 1, "Line" = 2, 
                       #                                         "SAP Batch Number" = 3, "The Columns" = 4),
                       #                          selected = "1"),
                       #             uiOutput("xaxis_data_render")
                       #             #the xaxis data will be inputted here by inserUI in the observe
                       #             #event of xcategoricalselection
                       #           ), #end conditionPanel
                       #           selectInput("yaxis_data", "Choose Data for the Y-Axis",
                       #                    choices = colnames(plottingdata$data),
                       #                    selected = NULL),
                       #        radioButtons("xaxis_scale", "Choose a Scale for the X-Axis",
                       #                    choices = list("Linear" = 1, "Log" = 2),
                       #                    selected = NULL),
                       #        radioButtons("yaxis_scale", "Choose a Scale for the Y-Axis",
                       #                     choices = list("Linear" = 1, "Log" = 2),
                       #                     selected = NULL)
                       #        ),
                       # "7" = #googleVis Line Chart
                       #   list(radioButtons("isxcategorical", "Would You Like to have the X-Axis be Categorical (Such as Having the X-Axis be Material Number, Line, Batch, Or Even Columns)",
                       #                    choices = list("No" = 0, "Yes" = 1),
                       #                    selected = "0"),
                       #        conditionalPanel(
                       #          condition = "input.isxcategorical == '0'",
                       #          #if the user does NOT want the x-axis to be categorical
                       #          selectInput("xaxis_data", "Choose Data for the X-Axis",
                       #                      choices = colnames(plottingdata$data),
                       #                      selected = NULL)
                       #        ),
                       #        #if the user does want the x-axis to be categorical
                       #        conditionalPanel(
                       #          condition = "input.isxcategorical == '1'",
                       #          #if the user does want the x-axis to be categorical
                       #          radioButtons("xcategoricalselection", "Select What Grouping You want for the X Axis",
                       #                       choices = list("Material Number" = 1, "Line" = 2, 
                       #                                      "SAP Batch Number" = 3, "The Columns" = 4),
                       #                       selected = "1"),
                       #          uiOutput("xaxis_data_render")
                       #          #the xaxis data will be inputted here by inserUI in the observe
                       #          #event of xcategoricalselection
                       #        ), #end conditionPanel
                       #        selectInput("yaxis_data", "Choose Data for the Y-Axis",
                       #                    choices = colnames(plottingdata$data),
                       #                    selected = NULL),
                       #        radioButtons("xaxis_scale", "Choose a Scale for the X-Axis",
                       #                     choices = list("Linear" = 1, "Log" = 2),
                       #                     selected = NULL),
                       #        radioButtons("yaxis_scale", "Choose a Scale for the Y-Axis",
                       #                     choices = list("Linear" = 1, "Log" = 2),
                       #                     selected = NULL)
                       #        ),
                       # "8" = #googleVis Line Chart with 2 Y-Axes
                       #   tagList(radioButtons("isxcategorical", "Would You Like to have the X-Axis be Categorical (Such as Having the X-Axis be Material Number, Line, Batch, Or Even Columns)",
                       #                       choices = list("No" = 0, "Yes" = 1),
                       #                       selected = "0"),
                       #           conditionalPanel(
                       #             condition = "input.isxcategorical == '0'",
                       #             #if the user does NOT want the x-axis to be categorical
                       #             selectInput("xaxis_data", "Choose Data for the X-Axis",
                       #                         choices = colnames(plottingdata$data),
                       #                         selected = NULL)
                       #           ),
                       #           #if the user does want the x-axis to be categorical
                       #           conditionalPanel(
                       #             condition = "input.isxcategorical == '1'",
                       #             #if the user does want the x-axis to be categorical
                       #             radioButtons("xcategoricalselection", "Select What Grouping You want for the X Axis",
                       #                          choices = list("Material Number" = 1, "Line" = 2, 
                       #                                         "SAP Batch Number" = 3, "The Columns" = 4),
                       #                          selected = "1"),
                       #             uiOutput("xaxis_data_render")
                       #             #the xaxis data will be inputted here by inserUI in the observe
                       #             #event of xcategoricalselection
                       #           ), #end conditionPanel
                       #        selectInput("yaxis_data1", "Choose Data for the First Y-Axis",
                       #                    choices = colnames(plottingdata$data),
                       #                    selected = NULL),
                       #        selectInput("yaxis_data2", "Choose Data for the Second Y-Axis",
                       #                    choices = colnames(plottingdata$data),
                       #                    selected = NULL),
                       #        radioButtons("xaxis_scale", "Choose a Scale for the X-Axis",
                       #                     choices = list("Linear" = 1, "Log" = 2),
                       #                     selected = NULL),
                       #        radioButtons("yaxis_scale1", "Choose a Scale for the First Y-Axis",
                       #                     choices = list("Linear" = 1, "Log" = 2),
                       #                     selected = NULL),
                       #        radioButtons("yaxis_scale2", "Choose a Scale for the Second Y-Axis",
                       #                     choices = list("Linear" = 1, "Log" = 2),
                       #                     selected = NULL)
                       #        ),
                       # "9" = #googleVis Bar Chart
                       #   tagList(selectInput("xaxis_data", "Choose Data for the X-Axis",
                       #                    choices = colnames(plottingdata$data),
                       #                    selected = NULL),
                       #        selectInput("yaxis_data", "Choose Data for the Y-Axis",
                       #                    choices = colnames(plottingdata$data),
                       #                    selected = NULL),
                       #        radioButtons("xaxis_scale", "Choose a Scale for the X-Axis",
                       #                     choices = list("Linear" = 1, "Log" = 2),
                       #                     selected = NULL)
                       #   ),
                       # "10" = #googleVis Column Chart
                       #   list(selectInput("xaxis_data", "Choose Data for the X-Axis",
                       #                    choices = colnames(plottingdata$data),
                       #                    selected = NULL),
                       #        selectInput("yaxis_data", "Choose Data for the Y-Axis",
                       #                    choices = colnames(plottingdata$data),
                       #                    selected = NULL),
                       #        radioButtons("yaxis_scale", "Choose a Scale for the Y-Axis",
                       #                     choices = list("Linear" = 1, "Log" = 2),
                       #                     selected = NULL)
                       #   ),
                       # "11" = #googleVis Area Chart
                       #   tagList(selectInput("xaxis_data", "Choose Data for the X-Axis",
                       #                    choices = colnames(plottingdata$data),
                       #                    selected = NULL),
                       #        selectInput("yaxis_data", "Choose Data for the Y-Axis",
                       #                    choices = colnames(plottingdata$data),
                       #                    selected = NULL),
                       #        radioButtons("xaxis_scale", "Choose a Scale for the X-Axis",
                       #                     choices = list("Linear" = 1, "Log" = 2),
                       #                     selected = NULL),
                       #        radioButtons("yaxis_scale", "Choose a Scale for the Y-Axis",
                       #                     choices = list("Linear" = 1, "Log" = 2),
                       #                     selected = NULL)
                       #   ),
                       # "12" = #googleVis Stepped Area Chart
                       #   list(selectInput("xaxis_data", "Choose Data for the X-Axis",
                       #                    choices = colnames(plottingdata$data),
                       #                    selected = NULL),
                       #        selectInput("yaxis_data", "Choose Data for the Y-Axis",
                       #                    choices = colnames(plottingdata$data),
                       #                    selected = NULL),
                       #        radioButtons("xaxis_scale", "Choose a Scale for the X-Axis",
                       #                     choices = list("Linear" = 1, "Log" = 2),
                       #                     selected = NULL),
                       #        radioButtons("yaxis_scale", "Choose a Scale for the Y-Axis",
                       #                     choices = list("Linear" = 1, "Log" = 2),
                       #                     selected = NULL)
                       #   ),
                       # "13" = #not curerntly available
                       #   Null,
                       # "14" = #googleVis Bubble Chart
                       #   tagList(selectInput("idaxis_data", "Choose Grouping for the ID of the Bubble",
                       #                    choices = colnames(plottingdata$data),
                       #                    selected = NULL),
                       #        selectInput("xaxis_data", "Choose Data for the X-Axis",
                       #                    choices = colnames(plottingdata$data),
                       #                    selected = NULL),
                       #        selectInput("yaxis_data", "Choose Data for the Y-Axis",
                       #                    choices = colnames(plottingdata$data),
                       #                    selected = NULL),
                       #        radioButtons("xaxis_scale", "Choose a Scale for the X-Axis",
                       #                     choices = list("Linear" = 1, "Log" = 2),
                       #                     selected = NULL),
                       #        radioButtons("yaxis_scale", "Choose a Scale for the Y-Axis",
                       #                     choices = list("Linear" = 1, "Log" = 2),
                       #                     selected = NULL),
                       #        selectInput("coloraxis_data", "Choose Grouping for the Color",
                       #                    choices = colnames(plottingdata$data),
                       #                    selected = NULL),
                       #        selectInput("sizeaxis_data", "Choose Data for the Size of the Bubble",
                       #                    choices = colnames(plottingdata$data),
                       #                    selected = NULL)
                       #   ),
                       # "15" = #googleVis Pie Chart
                       #   tagList(selectInput("idaxis_data", "Choose Data for the ID of Each Slice",
                       #                    choices = colnames(plottingdata$data),
                       #                    selected = NULL),
                       #        selectInput("comparisonaxis_data", "Choose Data for Comparison",
                       #                    choices = colnames(plottingdata$data),
                       #                    selected = NULL)
                       #   ),
                       # "16" = #googleVis Histogram
                       #   tagList(selectInput("xaxis_data", "Choose Data for the X-Axis",
                       #                    choices = colnames(plottingdata$data),
                       #                    selected = NULL),
                       #        selectInput("yaxis_data", "Choose Data for the Y-Axis",
                       #                    choices = colnames(plottingdata$data),
                       #                    selected = NULL),
                       #        radioButtons("yaxis_scale", "Choose a Scale for the Y-Axis",
                       #                     choices = list("Linear" = 1, "Log" = 2),
                       #                     selected = NULL)
                       #   ),
                       # "17" = #googleVis Motion Chart
                       #   tagList(selectInput("idaxis_data", "Choose Data for ID of Each Bubble",
                       #                    choices = colnames(plottingdata$data),
                       #                    selected = NULL),
                       #        selectInput("timeaxis_data", "Choose Data for the Time Axis",
                       #                    choices = colnames(plottingdata$data),
                       #                    selected = NULL)
                       #   )
                       #   ,
                       # "18" = #googleVis Annotation Chart
                       #   tagList(#the time-axis will be chosen automatically as the start date,
                       #        selectInput("timeaxis_data", "Choose Data for the Time Axis",
                       #                    choices = colnames(plottingdata$data),
                       #                    selected = NULL),
                       #        selectInput("yaxis_data", "Choose Data for the Y-Axis",
                       #                    choices = colnames(plottingdata$data),
                       #                    selected = NULL),
                       #        radioButtons("yaxis_scale", "Choose a Scale for the Y-Axis",
                       #                     choices = list("Linear" = 1, "Log" = 2),
                       #                     selected = NULL)
                       #   ), #perhaps these will be used later
                       "19" = #GGplot2 Scatter Plot
                         tagList(radioButtons("isxcategorical", "Would You Like to have the X-Axis be Categorical (Such as Having the X-Axis be Material Number, Line, Batch, Or Even Columns)",
                                              choices = list("No" = 0, "Yes" = 1),
                                              selected = "0"),
                                 selectInput("xaxis_data", "Choose Data for the X-Axis",
                                             choices = colnames(plottingdata$data),
                                             selected = NULL),
                                 selectInput("yaxis_data", "Choose Data for the Y-Axis",
                                             choices = colnames(plottingdata$data),
                                             selected = NULL)
                         ),
                       "20" = tagList(),
                       "21" = tagList(),
                       "22" = tagList(),
                       "23" = tagList(),
                       "24" = tagList(),
                       "25" = tagList(),
                       "26" = tagList(),
                       "27" = tagList(),
                       "28" = tagList(),
                       "29" = tagList(),
                       "30" = tagList(),
                       "31" = tagList(),
                       "32" = tagList(),
                       "33" = tagList(),
                       "34" = tagList(),
                       "35" = tagList(),
                       "36" = tagList(),
                       "37" = tagList(),
                       "38" = tagList(),
                       "39" = tagList(),
                       "40" = tagList(),
                       "41" = tagList(),
                       "42" = tagList(),
                       "43" = tagList(),
                       "44" = tagList(),
                       "45" = tagList(),
                       "46" = tagList(),
                       "47" = tagList(),
                       "48" = tagList(),
                       "49" = tagList()
                       )
    
    graphinformation$graphaxeshtml <- axeshtml
  
  })#end observeEvent(input$graphtype)
  

  
  output$graphaxeshtmloutput <- renderUI({
    #will render the information the user has to input for the axes of the specif graph
    return(graphinformation$graphaxeshtml)
  })
  
  
  observeEvent(input$xcategoricalselection,{
    #this observes what the user seleced for the categorical variable
    
    choice_options <- switch(input$xcategoricalselection,
                             #based on waht the user selected for the categorical variable, this
                             #will change
                      "1" = unique(tapered_tari_parameter_and_yield_data[,"Material Number"]),
                      "2" = unique(tapered_tari_parameter_and_yield_data[,"Line"]),
                      "3" = unique(tapered_tari_parameter_and_yield_data[,"SAP Batch Number"]),
                      "4" = colnames(tapered_tari_parameter_and_yield_data)
    )
    
    whatisselected <- switch(input$xcategoricalselection,
                             #depend on the user's selection the initial choices selected changes
                             #all have everything selected except for the "Column" selection
                             "1" = choice_options,
                             "2" = choice_options,
                             "3" = choice_options,
                             "4" = NULL
      
    )
    
      output$xaxis_data_render <- renderUI(
        selectizeInput(inputId = "xaxis_data",
                       label = "Select the Inputs for the Category (multiple are allowed)", 
                       choices = choice_options,
                       selected = whatisselected,
                       multiple = TRUE) 
        
      )#end output$xaxis_data_render
  })
  
  output$batchfilterchoicesoutput <- renderUI(
    #the UI for choosing the batches for the batch filter
    selectizeInput(inputId = "batchfilterchoices",
                   label = "Select the Batch Numbers",
                   choices = unique(tapered_tari_parameter_and_yield_data[,"SAP Batch Number"]),
                   selected = unique(tapered_tari_parameter_and_yield_data[,"SAP Batch Number"]),
                   multiple = TRUE)
  )
  
  output$materialfilterchoicesoutput <- renderUI({
    #the UI for choosing the batches for the material filter
    ui <- selectizeInput(inputId = "materialfilterchoices",
                   label = "Select the Material Numbers",
                   choices = unique(tapered_tari_parameter_and_yield_data[,"Material Number"]),
                   selected = unique(tapered_tari_parameter_and_yield_data[,"Material Number"]),
                   multiple = TRUE)
    return(ui)
  })
  
  output$linefilterchoicesoutput <- renderUI(
    #the UI for choosing the batches for the line filter
    selectizeInput(inputId = "linefilterchoices",
                   label = "Select the Lines",
                   choices = unique(tapered_tari_parameter_and_yield_data[,"Line"]),
                   selected = unique(tapered_tari_parameter_and_yield_data[,"Line"]),
                   multiple = TRUE)
  )
  
  output$daterangefilteroutput <- renderUI(
    #Ui for the daterange filter
    dateRangeInput(inputId = "daterangefilter",
                   label = 'Start Date range input: yyyy-mm-dd',
                   start = min(plottingdata$filtered_data[,"Start Date"]), 
                   end = max(plottingdata$filtered_data[,"Start Date"])
    )
  )
  
  output$firstcolumnchoiceoutput <- renderUI(
    #UI for columnn to choose the first column filter
    selectInput(inputId = "firstcolumnchoice",
                #'user selects the column to filter
                label = "Select a Column to Filter",
                choices = (colnames(tapered_tari_parameter_and_yield_data)[which(colnames(tapered_tari_parameter_and_yield_data)
                                                             %out%
                                                               c("SAP Batch Number",
                                                                 "Material Number",
                                                                 "Line",
                                                                 "Start Date")
                                                             )
                                                       ]
                           ),
                #removes columns that already have filters in place
                selected = NULL
    )
  )
  
  output$secondcolumnchoiceoutput <- renderUI(
    #UI for columnn to choose the first column filter
    selectInput(inputId = "secondcolumnchoice",
                #'user selects the column to filter
                label = "Select a Column to Filter",
                choices = (colnames(tapered_tari_parameter_and_yield_data)[which(colnames(tapered_tari_parameter_and_yield_data)
                                                                      %out%
                                                                        c("SAP Batch Number",
                                                                          "Material Number",
                                                                          "Line",
                                                                          "Start Date")
                                                                      )
                                                       ]
                           ),
                #removes columns that already have filters in place
                selected = NULL
    )
  )
  
  output$firstcolumnfilter <- renderUI(
    #output for the user to select the values for the first column filter
    selectizeInput(inputId = "firstcolumnvalues",
                   label = "Choose the Values",
                   choices = unique(tapered_tari_parameter_and_yield_data),
                   selected = unique(tapered_tari_parameter_and_yield_data),
                   multiple = TRUE
                   )
  )
  
  output$secondcolumnfilter <- renderUI(
    #output for the user to select the values for the second column filter
    selectizeInput(inputId = "secondcolumnvalues",
                   label = "Choose the Values",
                   choices = unique(tapered_tari_parameter_and_yield_data),
                   selected = unique(tapered_tari_parameter_and_yield_data),
                   multiple = TRUE
    )
  )
  
  output$maingroupchoiceoutput <- renderUI(
    #choosing column for the main grouping
    selectInput(inputId = "maingroupchoice",
                label = "Please Select a Column for the Main Grouping (Color)",
                choices = colnames(tapered_tari_parameter_and_yield_data),
                selected = "Material Number",
                multiple = FALSE)
  )
  
  output$subgroupchoiceoutput <- renderUI(
    #choosing column for the main grouping
    selectInput(inputId = "subgroupchoice",
                label = "Please Select a Column for the Sub Grouping (Shape)",
                choices = colnames(tapered_tari_parameter_and_yield_data),
                selected = "Material Number",
                multiple = FALSE)
  )
  
  
  # output$googleplot <- renderGvis(
  #   gvisLineChart(data = test_data,
  #                 xvar = "Start Date",
  #                 yvar = "Yield Percentage",
  #                 options=list(gvis.editor="Edit me!",
  #                              explorer="{actions: ['dragToZoom','rightClickToReset'],maxZoomIn:0.05}",
  #                              crosshair="{trigger:'both'}",
  #                              chartArea="{width:'85%',height:'80%'}",
  #                              height= 1000,
  #                              width = 1000),
  #                 chartid = "googleplotid"
  #                 )
  # ) #perhaps this will be used later
  
  
  
  #### filtering functions ####
  
  observe({
    #observe for filtering the data
    
    placeholder_data <- plottingdata$data
    
    usematerialfilter <- input$usematerialfilter == "1"
    usebatchfilter <- input$usebatchfilter == "1"
    uselinefilter <- input$uselinefilter == "1"
    usedatefilter <- input$usedatefilter == "1"
    usefirstcolumnfilter <- input$usefirstcolumnfilter == "1"
    usesecondcolumnfilter <- input$usesecondcolumnfilter == "1"
    
    need_material <- need(input$materialfilterchoices, message = FALSE)
    isolate({
    if (usematerialfilter){
      #filters the data based on the user's material selections
      placeholder_data <- specificfilter(placeholder_data,
                                         "Material Number", 
                                         input$includeexcludematerial,
                                         input$materialfilterchoices
                                         )
    }
    })
    if (usebatchfilter){
      #filters the data based on the user's batch selections
      placeholder_data <- specificfilter(placeholder_data,
                                         "SAP Batch Number", 
                                         input$includeexcludebatch,
                                         input$batchfilterchoices
      )
    }
    if (uselinefilter){
      #filters the data based on the user's line selections
      placeholder_data <- specificfilter(placeholder_data,
                                         "Line", 
                                         input$includeexcludeline,
                                         input$linefilterchoices
      )
    }
    
    if (usedatefilter){
      #filters the date data
      start <- input$daterangefilter[1]
      end <- input$daterangefilter[2]
      
      placeholder_data <- placeholder_data[placeholder_data[,"Start Date"] >= start,]
      placeholder_data <- placeholder_data[placeholder_data[,"Start Date"] <= end,]
      
    }
    
    
    first_need_condition <- need(input$firstcolumnchoice, message = FALSE)
    
    if (usefirstcolumnfilter && is.null(first_need_condition)){
      if (input$firstcolumnoperator == "1"){
        inequality <- input$firstcolumninequality #inequality symbol
        
        placeholder_data <- switch(inequality,
                                   "1" = placeholder_data[placeholder_data[,input$firstcolumnchoice]
                                                          < input$firstcolumninequalityinput,],
                                   "2" = placeholder_data[placeholder_data[,input$firstcolumnchoice]
                                                          <= input$firstcolumninequalityinput,],
                                   "3" = placeholder_data[placeholder_data[,input$firstcolumnchoice]
                                                          >= input$firstcolumninequalityinput,],
                                   "4" = placeholder_data[placeholder_data[,input$firstcolumnchoice]
                                                          > input$firstcolumninequalityinput,]
                                   )
        
      }
      else if (input$firstcolumnoperator == "2"){
        #range filter
        
        firstinequality <- input$firstcolumnrangemininequality #inequality symbol
        
        placeholder_data <- switch(firstinequality,
                                   "1" = placeholder_data[placeholder_data[,input$firstcolumnchoice]
                                                          < input$firstcolumnrangemininput,],
                                   "2" = placeholder_data[placeholder_data[,input$firstcolumnchoice]
                                                          <= input$firstcolumnrangemininput,],
                                   "3" = placeholder_data[placeholder_data[,input$firstcolumnchoice]
                                                          >= input$firstcolumnrangemininput,],
                                   "4" = placeholder_data[placeholder_data[,input$firstcolumnchoice]
                                                          > input$firstcolumnrangemininput,]
        )
        secondinequality <- input$firstcolumnrangemaxinequality #inequality symbol
        
        placeholder_data <- switch(secondinequality,
                                   "1" = placeholder_data[placeholder_data[,input$firstcolumnchoice]
                                                          < input$firstcolumnrangemaxinput,],
                                   "2" = placeholder_data[placeholder_data[,input$firstcolumnchoice]
                                                          <= input$firstcolumnrangemaxinput,],
                                   "3" = placeholder_data[placeholder_data[,input$firstcolumnchoice]
                                                          >= input$firstcolumnrangemaxinput,],
                                   "4" = placeholder_data[placeholder_data[,input$firstcolumnchoice]
                                                          > input$firstcolumnrangemaxinput,]
        )
      }
      else if (input$firstcolumnoperator == "3"){
        #matching values filter
        
        includeorexclude <- input$firstcolumnvaluesie #(1 for include, 0 for exclude)
        
        if (includeorexclude == "1"){
          #include
          placeholder_data <- placeholder_data[which(placeholder_data[,input$firstcolumnchoice]
                                                     %in% input$firstcolumnvalues),]
        }
        else if (includeorexclude == "0"){
          #exclude
          placeholder_data <- placeholder_data[which(placeholder_data[,input$firstcolumnchoice]
                                                     %out% input$firstcolumnvalues),]
        }
        
      }
    }#end use first column filter
    
    second_need_condition <- need(input$secondcolumnchoice, message = FALSE)
    
    if (usesecondcolumnfilter && is.null(second_need_condition)){
      if (input$secondcolumnoperator == "1"){
        inequality <- input$secondcolumninequality #inequality symbol
        
        placeholder_data <- switch(inequality,
                                   "1" = placeholder_data[placeholder_data[,input$secondcolumnchoice]
                                                          < input$secondcolumninequalityinput,],
                                   "2" = placeholder_data[placeholder_data[,input$secondcolumnchoice]
                                                          <= input$secondcolumninequalityinput,],
                                   "3" = placeholder_data[placeholder_data[,input$secondcolumnchoice]
                                                          >= input$secondcolumninequalityinput,],
                                   "4" = placeholder_data[placeholder_data[,input$secondcolumnchoice]
                                                          > input$secondcolumninequalityinput,]
        )
        
      }
      else if (input$secondcolumnoperator == "2"){
        #range filter
        
        secondinequality <- input$secondcolumnrangemininequality #inequality symbol
        
        placeholder_data <- switch(secondinequality,
                                   "1" = placeholder_data[placeholder_data[,input$secondcolumnchoice]
                                                          < input$secondcolumnrangemininput,],
                                   "2" = placeholder_data[placeholder_data[,input$secondcolumnchoice]
                                                          <= input$secondcolumnrangemininput,],
                                   "3" = placeholder_data[placeholder_data[,input$secondcolumnchoice]
                                                          >= input$secondcolumnrangemininput,],
                                   "4" = placeholder_data[placeholder_data[,input$secondcolumnchoice]
                                                          > input$secondcolumnrangemininput,]
        )
        secondinequality <- input$secondcolumnrangemaxinequality #inequality symbol
        
        placeholder_data <- switch(secondinequality,
                                   "1" = placeholder_data[placeholder_data[,input$secondcolumnchoice]
                                                          < input$secondcolumnrangemaxinput,],
                                   "2" = placeholder_data[placeholder_data[,input$secondcolumnchoice]
                                                          <= input$secondcolumnrangemaxinput,],
                                   "3" = placeholder_data[placeholder_data[,input$secondcolumnchoice]
                                                          >= input$secondcolumnrangemaxinput,],
                                   "4" = placeholder_data[placeholder_data[,input$secondcolumnchoice]
                                                          > input$secondcolumnrangemaxinput,]
        )
      }
      else if (input$secondcolumnoperator == "3"){
        #matching values filter
        
        includeorexclude <- input$secondcolumnvaluesie #(1 for include, 0 for exclude)
        
        if (includeorexclude == "1"){
          #include
          placeholder_data <- placeholder_data[which(placeholder_data[,input$secondcolumnchoice]
                                                     %in% input$secondcolumnvalues),]
        }
        else if (includeorexclude == "0"){
          #exclude
          placeholder_data <- placeholder_data[which(placeholder_data[,input$secondcolumnchoice]
                                                     %out% input$secondcolumnvalues),]
        }
        
      }
    }#end use second column filter
    
    
    
    #cleans the data to plot by removing NA and blank values
    
    if (is.null(need(input$xaxis_data, message = FALSE)) && 
        is.null(need(input$yaxis_data, message = FALSE))){
      #if both are present, then clean the data to remove NAs and blanks ("")
      
      
      clean_data <- placeholder_data[which(!is.na(placeholder_data[, input$xaxis_data])),]
      clean_data <- clean_data[which(!is.na(clean_data[, input$yaxis_data])),]
      
      omitted_datax1 <- placeholder_data[which(is.na(placeholder_data[, input$xaxis_data])),]
      omitted_datay1 <- placeholder_data[which(is.na(placeholder_data[, input$yaxis_data])),]
      
      clean_data <- clean_data[which(clean_data[, input$xaxis_data] != ""),]
      clean_data <- clean_data[which(clean_data[, input$yaxis_data] != ""),]
      
      omitted_datax2 <- placeholder_data[which(placeholder_data[, input$xaxis_data] == ""),]
      omitted_datay2 <- placeholder_data[which(placeholder_data[, input$yaxis_data] == ""),]
      
      omitted_data <- rbind(omitted_datax1, omitted_datay1, omitted_datax2, omitted_datay2)
      #removes duplicate batches
      omitted_data <- omitted_data[which(!duplicated(omitted_data[,"SAP Batch Number"])),]
      
      plottingdata$omitted_data <- omitted_data
      
      if (nrow(clean_data) != 0){
        clean_data[,input$xaxis_data] <- numericIfPossible(clean_data[,input$xaxis_data])
        clean_data[,input$yaxis_data] <- numericIfPossible(clean_data[,input$yaxis_data])
      }
      
      plottingdata$filtered_data <- clean_data
    }
    else{
      plottingdata$filtered_data <- placeholder_data
    }
    
  })
  
  specificfilter <- function(dataframe, column, includeoption, inputs){
    #takes the material, batch, and line filter to filter the data based on include or exclude
    #and the inputs the user has selected
    
    data <- dataframe
    
    include <- includeoption == "1" #did the user choose to include the data selected
    
    if (include){
      #the user chose to include
      data <- data[which(data[,column] %in% inputs),] #the rows of the data that match
    }
    else{
      #the user chose to exclude
      data <- data[which(data[,column] %out% inputs),] #the rows of the data that match
    }
    
    
    return(data)
    
  }#end specific filter
  
  
  
  #### Edit Plots ####
  


  #### Ggplot rendering ####
  
  output$testdatatable <- renderDataTable({
    return(plottingdata$filtered_data)
  },
  filter = "none",
  extensions = 'ColReorder',
  rownames= FALSE,
  options = list(orderClasses = TRUE,
                 columnDefs = list(list(className = 'dt-center',targets = "_all")),
                 scrollX=TRUE,
                 scrollY=500,
                 autoWidth=TRUE,
                 colReorder = TRUE))
  
  plots <- reactiveValues(
    #storing the plots, things when then be added to these base plots as the user edits the plots
    mainplot = NULL,
    zoomplot = NULL
  )
  
  
  
  output$mainplotoutput <- renderPlot({
    data <- plottingdata$filtered_data
    xdata <- data[,input$xaxis_data]
    ydata <- data[,input$yaxis_data]
    
    if (input$isxcategorical == "1"){
      #user has selected for the variable to be categorical
      #this will really only apply to numerical data, but I will convert it to character
      xdata <- as.character(xdata)
      plottingdata$filtered_data[,input$xaxis_data] <- xdata
    }
    
    if (input$xaxis_scale == "2"){
      #user has selected a log scale for the data
      if (is.numeric(xdata)){
        #if it is numeric, change to a log scale
        xdata <-  log10(xdata)
      }
      else{
        #if it is not numeric, output a popup box that says a log scale cannot be used
        #for a categorical variable
        #also updates the radio button to be linear
        showModal(modalDialog(
          title = "Add Part Number",
          "A log scale cannot be applied to a categorical varibale.",
          easyClose = T
        ))
        updateRadioButtons(session, 
                           inputId = "xaxis_scale", 
                           label = "Choose a Scale for the X-Axis",
                           choices = list("Linear" = 1, "Log" = 2),
                           selected = "1")
      }
    }
    
    if (input$yaxis_scale == "2"){
      #user has selected a log scale for the data
      if (is.numeric(ydata)){
        #if it is numeric, change to a log scale
        ydata <-  log10(ydata)
      }
      else{
        #if it is not numeric, output a popup box that says a log scale cannot be used
        #for a categorical variable
        #also updates the radio button to be linear
        showModal(modalDialog(
          title = "Add Part Number",
          "A log scale cannot be applied to a categorical varibale.",
          easyClose = T
        ))
        updateRadioButtons(session, 
                           inputId = "xaxis_scale", 
                           label = "Choose a Scale for the X-Axis",
                           choices = list("Linear" = 1, "Log" = 2),
                           selected = "1")
      }
    }
    
    plot <- ggplot(data, aes(xdata, ydata))
    
    if (input$usemaingroup == "1" && input$usesubgroup == "0"){
      #main grouping but not subgroupind
      maingroup <- factor(data[,input$maingroupchoice])
      plot <- plot + geom_point(aes(colour=maingroup))
    }
    else if (input$usemaingroup == "1" && input$usesubgroup == "1"){
      maingroup <- factor(data[,input$maingroupchoice])
      subgroup <- factor(data[,input$subgroupchoice])
      plot <- plot + geom_point(aes(colour=maingroup,shape=subgroup)) + scale_shape_manual(values=1:nlevels(subgroup))
      #use main grouping and subgrouping
    }
    else{
      plot <- plot + geom_point()
    }
    
    
    changexticks <- input$changexticks == "1"
    if(changexticks){
      xspacing <- input$xtickspacing
      
      if (input$changexticks == "1"){
        #selected to change
        xpresent <- need(xspacing, message = FALSE)
        x_is_integer <- as.integer(xspacing)
        if (is.null(xpresent) && !is.na(x_is_integer)){
          #if it is present and is an integer, it will update the tick spacing
          values <- plottingdata$filtered_data[,isolate(input$xaxis_data)]
          
          
          if (is.numeric(xdata)){
            #continuous if numeric
            xtick_spacing <- seq(min(values), max(values), length.out = as.numeric(xspacing))
            plot <- plot + scale_x_continuous(breaks=c(xtick_spacing))
          }
          else{
            #discrete if not
            xtick_spacing<- values[seq(1, length(values), length.out = as.numeric(xspacing))]
            plot <- plot + scale_x_discrete(breaks=c(xtick_spacing))
          }
          
        }
      }
      
    }
    
    changeyticks <- input$changeyticks == "1"
    if(changeyticks){
      yspacing <- input$ytickspacing
      
      if (input$changeyticks == "1"){
        #selected to change
        ypresent <- need(yspacing, message = FALSE)
        y_is_integer <- as.integer(yspacing)
        if (is.null(ypresent) && !is.na(y_is_integer)){
          #if it is present and is an integer, it will update the tick spacing
          values <- plottingdata$filtered_data[,isolate(input$yaxis_data)]
          
          if (is.numeric(ydata)){
            #continuous if numeric
            ytick_spacing <- seq(min(values), max(values), length.out = as.numeric(yspacing))
            plot <- plot + scale_y_continuous(breaks=c(ytick_spacing))
          }
          else{
            #discrete if not
            ytick_spacing<- values[seq(1, length(values), length.out = as.numeric(yspacing))]
            plot <- plot + scale_ydiscrete(breaks=c(ytick_spacing))
          }
          
        }
      }
      
    }
    
    if (input$boxplotinput == "1"){
      if (is.numeric(xdata)){
        showModal(modalDialog(
          title = "Add Part Number",
          "A boxplot cannot be applied to a continuous X-Axis.",
          easyClose = T
        ))
        updateRadioButtons(session, 
                           inputId = "boxplotinput", 
                           label = "Would you like to overlay a boxplot?",
                           choices = list("No" = 0, "Yes" = 1),
                           selected = "0")
      }
      else{
        plot <- plot + geom_boxplot()
      }
    }
    if (input$usejitter == "1"){
      plot <- plot + geom_jitter()
      if (is.numeric(xdata)){
        showModal(modalDialog(
          title = "Add Part Number",
          "It is not recommended to jitter on a continuous X-Axis.",
          easyClose = T
        ))
      }
    }
    

    return(plot)
    
    
  })
  
  brushselection <- reactiveValues(x = NULL, y = NULL) #the values for the brush area
  
  observe({
    #editing the brush if it is selected
    brush <- input$mainplot_brush
    if (!is.null(brush)) {
      brushselection$x <- c(brush$xmin, brush$xmax)
      brushselection$y <- c(brush$ymin, brush$ymax)
      
    } else {
      brushselection$x <- NULL
      brushselection$y <- NULL
    }
  })
  
  output$zoomplot <- renderPlot({
    #the plot that is zoomed in
    data <- plottingdata$filtered_data
    xdata <- data[,input$xaxis_data]
    ydata <- data[,input$yaxis_data]
    
    if (input$isxcategorical == "1"){
      #user has selected for the variable to be categorical
      #this will really only apply to numerical data, but I will convert it to character
      xdata <- as.character(xdata)
    }
    
    if (input$xaxis_scale == "2"){
      #user has selected a log scale for the data
      if (is.numeric(xdata)){
        #if it is numeric, change to a log scale
        xdata <-  log10(xdata)
      }
      else{
        #do nothing
      }
    }
    
    if (input$yaxis_scale == "2"){
      #user has selected a log scale for the data
      if (is.numeric(ydata)){
        #if it is numeric, change to a log scale
        ydata <-  log10(ydata)
      }
      else{
        #do nothing
      }
    }
    
    plot <- ggplot(data, aes(xdata, ydata))
    
    if (input$usemaingroup == "1" && input$usesubgroup == "0"){
      #main grouping but not subgroupind
      maingroup <- factor(data[,input$maingroupchoice])
      plot <- plot + geom_point(aes(colour=maingroup)) + coord_cartesian(xlim = brushselection$x, ylim = brushselection$y, expand = FALSE)
    }
    else if (input$usemaingroup == "1" && input$usesubgroup == "1"){
      maingroup <- factor(data[,input$maingroupchoice])
      subgroup <- factor(data[,input$subgroupchoice])
      plot <- plot + geom_point(aes(colour=maingroup,shape=subgroup)) + scale_shape_manual(values=1:nlevels(subgroup)) + coord_cartesian(xlim = brushselection$x, ylim = brushselection$y, expand = FALSE)
      #use main grouping and subgrouping
    }
    else{
      plot <- plot + geom_point() + coord_cartesian(xlim = brushselection$x, ylim = brushselection$y, expand = FALSE)
    }
    
    changexticks <- input$changexticks == "1"
    if(changexticks){
      xspacing <- input$xtickspacing
      
      if (input$changexticks == "1"){
        #selected to change
        xpresent <- need(xspacing, message = FALSE)
        x_is_integer <- as.integer(xspacing)
        if (is.null(xpresent) && !is.na(x_is_integer)){
          #if it is present and is an integer, it will update the tick spacing
          values <- plottingdata$filtered_data[,isolate(input$xaxis_data)]
          
          
          if (is.numeric(xdata)){
            #continuous if numeric
            xtick_spacing <- seq(min(values), max(values), length.out = as.numeric(xspacing))
            plot <- plot + scale_x_continuous(breaks=c(xtick_spacing))
          }
          else{
            #discrete if not
            xtick_spacing<- values[seq(1, length(values), length.out = as.numeric(xspacing))]
            plot <- plot + scale_x_discrete(breaks=c(xtick_spacing))
          }
          
        }
      }
      
    }
    
    changeyticks <- input$changeyticks == "1"
    if(changeyticks){
      yspacing <- input$ytickspacing
      
      if (input$changeyticks == "1"){
        #selected to change
        ypresent <- need(yspacing, message = FALSE)
        y_is_integer <- as.integer(yspacing)
        if (is.null(ypresent) && !is.na(y_is_integer)){
          #if it is present and is an integer, it will update the tick spacing
          values <- plottingdata$filtered_data[,isolate(input$yaxis_data)]
          
          if (is.numeric(ydata)){
            #continuous if numeric
            ytick_spacing <- seq(min(values), max(values), length.out = as.numeric(yspacing))
            plot <- plot + scale_y_continuous(breaks=c(ytick_spacing))
          }
          else{
            #discrete if not
            ytick_spacing<- values[seq(1, length(values), length.out = as.numeric(yspacing))]
            plot <- plot + scale_ydiscrete(breaks=c(ytick_spacing))
          }
          
        }
      }
      
    }
    
    
    if (input$boxplotinput == "1"){
      if (is.numeric(xdata)){
        #do nothing if it is numeric
      }
      else{
        plot <- plot + geom_boxplot()
      }
    }
    
    if (input$usejitter == "1"){
      plot <- plot + geom_jitter()
    }
    
    return(plot)
  })
  
  output$omitteddatatable <-DT::renderDataTable({
    return(plottingdata$omitted_data)
  },
  filter = "none",
  extensions = 'ColReorder',
  rownames= FALSE,
  options = list(orderClasses = TRUE,
                 columnDefs = list(list(className = 'dt-center',targets = "_all")),
                 scrollX=TRUE,
                 scrollY=500,
                 autoWidth=TRUE,
                 colReorder = TRUE))
  
  brushed_data<-reactive({
    brushed_data <- brushedPoints(plottingdata$filtered_data, input$mainplot_brush,
                                  xvar=input$xaxis_data,yvar=input$yaxis_data)
    data<-data.frame(brushed_data, stringsAsFactors = FALSE, check.names = FALSE)
    return(data)
  })

  output$zoomdatatable <-DT::renderDataTable({
    return(brushed_data())
  },
  filter = "none",
  extensions = 'ColReorder',
  rownames= FALSE,
  options = list(orderClasses = TRUE,
                 columnDefs = list(list(className = 'dt-center',targets = "_all")),
                 scrollX=TRUE,
                 scrollY=500,
                 autoWidth=TRUE,
                 colReorder = TRUE))
  
  
  ## Plot Hovers
  
  output$mainplot_hover_info <- renderUI({
    hover <- input$mainplot_hover
    
    point <- nearPoints(plottingdata$filtered_data, hover, 
                        xvar=input$xaxis_data,yvar=input$yaxis_data,
                        threshold = 5, maxpoints = 1, addDist = TRUE)
    
    if (nrow(point) == 0) return(NULL)
    
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    wellPanel(
      style = style,
      p(HTML(paste0("<b> SAP Batch Number: ", point$"SAP Batch Number", "</b>", 
                    "<br> Material Number: ", point$"Material Number",
                    "<br> Line: ", point$"Line",
                    "<br>", input$xaxis_data, ": ", point[,input$xaxis_data],
                    "<br>", input$yaxis_data, ": ", point[,input$yaxis_data]
                    )
             )
        )
    )
  })
  
  output$zoomplot_hover_info <- renderUI({
    hover <- input$zoomplot_hover
    
    point <- nearPoints(plottingdata$filtered_data, hover, 
                        xvar=input$xaxis_data,yvar=input$yaxis_data,
                        threshold = 5, maxpoints = 1, addDist = TRUE)
    
    if (nrow(point) == 0) return(NULL)
    
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    wellPanel(
      style = style,
      p(HTML(paste0("<b> SAP Batch Number: ", point$"SAP Batch Number", "</b>", 
                    "<br> Material Number: ", point$"Material Number",
                    "<br> Line: ", point$"Line",
                    "<br>", input$xaxis_data, ": ", point[,input$xaxis_data],
                    "<br>", input$yaxis_data, ": ", point[,input$yaxis_data]
      )
      )
      )
    )
  })
  
  
  #### Summary Boxes ####
  
  output$sapbatchboxoutput <- renderValueBox({
    valueBox(
      value = length(unique(tapered_tari_parameter_and_yield_data[,"SAP Batch Number"])),
      subtitle = "SAP Batches",
      icon = icon("info-circle")
      )
  })
  
  output$materialboxoutput <- renderValueBox({
    valueBox(
      value = length(unique(tapered_tari_parameter_and_yield_data[,"Material Number"])),
      subtitle = "Materials",
      icon = icon("info-circle")
    )
  })
  
  output$lineboxoutput <- renderValueBox({
    valueBox(
      value = length(unique(tapered_tari_parameter_and_yield_data[,"Line"])),
      subtitle = "Lines",
      icon = icon("info-circle")
    )
  })
  
  output$operatorboxoutput <- renderValueBox({
    valueBox(
      value = length(unique(tapered_tari_parameter_and_yield_data[,"Start Operator ID"])),
      subtitle = "Start Operators",
      icon = icon("info-circle")
    )
  })
  
  output$monthlyrunsboxoutput <- renderValueBox({
    numberofmonths <- as.numeric((as.POSIXlt(max(tapered_tari_parameter_and_yield_data$`Start Date`), format="%Y-%m-%d") 
                                  - 
                                    as.POSIXlt(min(tapered_tari_parameter_and_yield_data$`Start Date`), format="%Y-%m-%d"))/30.44)
    
    ui <- valueBox(
      value = signif(length(tapered_tari_parameter_and_yield_data[,"SAP Batch Number"])/numberofmonths, 
                     digits = 4),
      subtitle = "Runs Per Month",
      icon = icon("info-circle")
      )
    return(ui)
  })
  
  output$averageyieldboxoutput <- renderValueBox({
    yieldqty <- sum(as.integer(tapered_tari_parameter_and_yield_data$`Yield Qty`[tapered_tari_parameter_and_yield_data$`Yield Qty` != ""]))
    scrapqty <- sum(as.integer(tapered_tari_parameter_and_yield_data$`Scrap Qty`[tapered_tari_parameter_and_yield_data$`Scrap Qty` != ""]))
    
    ui <- valueBox(
      value = signif(((yieldqty/(yieldqty + scrapqty))*100), 
                     digits = 4),
      subtitle = "Average Yield",
      icon = icon("info-circle")
    )
    
    return(ui)
  })
  
  output$startdateboxoutput <- renderValueBox({
    valueBox(
      value = min(tapered_tari_parameter_and_yield_data[,"Start Date"]),
      subtitle = "Earliest Run (Year-Month-Day)",
      icon = icon("info-circle")
    )
  })
  
  output$enddateboxoutput <- renderValueBox({
    valueBox(
      value = max(tapered_tari_parameter_and_yield_data[,"Start Date"]),
      subtitle = "Latest Run (Year-Month-Day)",
      icon = icon("info-circle")
    )
  })
  
  
  #### Download buttons ####
  
  output$downloadfiltereddata <- downloadHandler(
    #downlaod the data
    filename = function() { paste("Filtered Data", '.csv', sep='') },
    content = function(file) {
      output <- plottingdata$filtered_data
      write.csv(output, file, row.names = FALSE)
    }
  )
  
  output$downloadzoomdata <- downloadHandler(
    #downlaod the data
    filename = function() { paste("Zoom Data", '.csv', sep='') },
    content = function(file) {
      output <- brushed_data()
      write.csv(output, file, row.names = FALSE)
    }
  )
  
  output$downloadomitteddata <- downloadHandler(
    #downlaod the data
    filename = function() { paste("Omitted Data", '.csv', sep='') },
    content = function(file) {
      output <- plottingdata$omitted_data
      write.csv(output, file, row.names = FALSE)
    }
  )
  
  
  
  #### Special Functions ####
  
  `%out%` <- function(a,b){
    ! a %in% b
  } 
  
  
  numericIfPossible <- function(vector){
    #converts the vector to numeric if possible
    if (suppressWarnings(all(!is.na(as.numeric(as.character(vector)))))) {
      return(as.numeric(as.character(vector)))
    } else {
      return(vector)
    }
  }
  
  modeofvector <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
                                           
                                           
  
   
}#end server

# Run the application 
shinyApp(ui = ui, server = server)

