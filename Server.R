#### Function ####

generateFilterList <- function(filters_selected){
  #this function takes the UI input of which filters the user has selected and then will output
  #a tag list of the necessary filters to display.
  
  #subsets the list to the selected filters and then lappls the generateFilterUI function
  filter_ui <- lapply(filters_selected,
                      generateFilterUI)
  
  
  return(filter_ui)
  
}#end generateFilterList


generateFilterUI <- function(filter){
  #this generates the individual UI for each filter
  
  #gets the type of filter
  type <- filterkey[Filter == filter, Data.Type]
  
  
  ui_element <- switch(type,
                       #if it is a query, there will be a text input for the user
                       Query = textInput(inputId = paste0(filterkey[Filter == filter, Data.Table], "_filter_query"),
                                         label = filter,
                                         value = "",
                                         placeholder = "Please Enter a Search"
                       ),
                       #if it is a string, there will be a select input where the user can select multiple options
                       String = selectInput(inputId = paste0(filterkey[Filter == filter, Data.Table], "_filter_string"),
                                            label = filter,
                                            choices = filter_options[[filter]]$Values,
                                            selected = NULL,
                                            multiple = T,
                                            selectize = T
                                            
                       ),
                       #creates a list of a min and max input if the user selects multiple
                       Numeric = list(
                         numericInput(
                           inputId = paste0(filterkey[Filter == filter, Data.Table], "_filter_min"),
                           label = filter,
                           value = filter_options[[filter]]$Min
                         ),
                         numericInput(
                           inputId = paste0(filterkey[Filter == filter, Data.Table], "_filter_max"),
                           label = filter,
                           value = filter_options[[filter]]$Max
                         )
                       ) #end the list for Numeric
                       
  ) #end switch
  
  return(ui_element)
  
}#end generateFilterUI



#### Server ####

server<-function(input,output,session){
  
  output$windchill_filters <- renderUI({
    return(test_ui_list)
  })
  
  filters_selected_reactive <- reactive({
    return(c(input$windchill_select, input$resin_select, input$tooling_select,
             input$parameter_select, input$attribute_select, input$special_select))
  })
  
  output$catalog_table <- DT::renderDataTable({
    #renders the PPS Catalog
    return(total_pps_data[, eval(parse(text = filterkey[Filter == c("Shopping Cart", filters_selected_reactive()) &
                                                          Show.Type == "Show", Data.Table]))
                          ])
  },
  options = list(orderClasses = TRUE,
                 columnDefs = list(list(className = 'dt-center',
                                        targets = "_all"
                 )
                 ),
                 scrollX=TRUE,
                 scrollY=600,
                 autoWidth=TRUE),
  rownames = FALSE,
  escape = FALSE, #escape allows for html elements to be rendered in the table
  server = FALSE) #end Extrusion Catalog
  
  
} #end server

# Run the application 
shinyApp(ui = ui, server = server)
