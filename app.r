library(DT)
library(shiny)
library(scotgov)
library(dplyr)

datasets <- scotgov::list_sg_datasets()

names_only <- datasets%>%
  select(dataset_name)

structure_reactive <- reactive(get_structure(names_only[[1,1]]))

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      # selectInput("amountTable", "Amount Tables", 1:10, 1),
      selectInput("main_1",
                  label="Select dataset",
                  choices = names_only
                  ),
      textOutput("query")
    ),
    mainPanel(
      # UI output
      uiOutput("dt")
    )
  )
)

server <-  function(input, output, session) {

  #attemp get_structure eveb
  observe({
    
    new_struct<-structure_reactive()

    dimension_list <- new_struct$dimensions[[1]]

    #painfully modify the empty tables
    lapply(1:length(dimension_list), function(sparql_table) {
      output[[paste0('T', sparql_table)]] <- DT::renderDataTable(
        data.frame(new_struct[[2]][sparql_table]),
                   rownames= FALSE
        )
    })
  })
  
  
  #build query
  observe({

  new_struct<-structure_reactive()
  dimension_list <- new_struct$dimensions[[1]]

   #start query
   build_query <- paste0("scotgov_get('",
          input$main_1, "'")
   
   #create additions
   for(i in 1:length(dimension_list)){
     if(!is.null(input[[paste0("T",i,"_rows_selected")]])) {
       print(input[[paste0("T",i,"_rows_selected")]])
       build_query <- paste0(build_query,
                             ", ",
                             names(new_struct[[2]][i]),
                             " = c('",
                             paste(new_struct[[2]][[i]][input[[paste0("T",i,"_rows_selected")]]], collapse = "', '"),
                             "')")
     }
   }
     

   build_query <- paste0(build_query, " )")
   
   output$query <- renderText(build_query)

  })
  
  #create dummy tables
  output$dt <- renderUI({
    
    structure <- get_structure(input$main_1)
    structure_reactive <- reactive(structure)
    
    dimension_list <- structure$dimensions[[1]]
    
    tagList(lapply(1:length(dimension_list),
                   function(i) {
      dataTableOutput(paste0('T', i))
    }))
    
  })
  
}

shinyApp(ui, server)
