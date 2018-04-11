#_d:id of the output of checkbox
#_input: the name of the searchbox
#PCS:Part Catalog--Single Extrusion PPS
#PCM: Part Catalog--Multi Extrusion PPS
#PCT: Part Catalog--Tapered Extrusion PPS

server<-function(input,output,session){
  
  output$catalog_table <- DT::renderDataTable({
    #renders the PPS Catalog
    return(total_pps_data)
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
