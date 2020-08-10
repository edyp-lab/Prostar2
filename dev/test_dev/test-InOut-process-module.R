library(shiny)
library(DT)

source(file.path('../../R', 'mod_format_DT.R'), local=TRUE)$value


mod_test_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns('perform'), 'Divide by two !'),
    actionButton(ns('save'), 'Save'),
    h3("table_obj"),
    dataTableOutput(ns('table_obj')),
    h3("table_dataIn"),
    dataTableOutput(ns('table_dataIn')),
    h3("table_dataOut"),
    dataTableOutput(ns('table_dataOut'))
    )
}

mod_test_server <- function(input, output, session, obj, ind){
  ns <- session$ns
  
  rv.test <- reactiveValues(
    dataIn = NULL,
    dataOut = NULL
  )


observe({
  rv.test$dataIn <- obj()
})

  observeEvent(input$perform, {
    assay(rv.test$dataIn[[ind()]]) <- assay(rv.test$dataIn[[ind()]])/2
  })

  observeEvent(input$save, {
    rv.test$dataOut <- rv.test$dataIn
  })

  output$table_obj <- renderDataTable({
    req(obj())
    req(ind())
    DT::datatable(assay(obj()[[ind()]]))
  })

  output$table_dataIn <- renderDataTable({
    req(rv.test$dataIn)
    req(ind())
    DT::datatable(assay(rv.test$dataIn[[ind()]]))
  })

  output$table_dataOut <- renderDataTable({
    req(rv.test$dataOut)
    req(ind())
    DT::datatable(assay(rv.test$dataOut[[ind()]]))
  })

  return(reactive({rv.test$dataOut}))
}






###----------------------------------------------------
ui <- fluidPage(
  tagList(
    uiOutput('chooseAssay'),
    mod_test_ui('test'),
    h3("temoin"),
    mod_format_DT_ui('temoin'),
    h3("out"),
    mod_format_DT_ui('out')
  )
)

server <- function(input, output, session) {
  
  utils::data(Exp1_R25_pept, package='DAPARdata2')
  obj <- Exp1_R25_pept[1:2,]
  
  r <- reactiveValues(
    ret = NULL
  )
  
  
  output$chooseAssay <- renderUI({
    selectInput('id', 'Choose assay', choices=1:length(obj))
  })
  
  
  r$ret <- callModule(mod_test_server, "test",
                      obj = reactive({obj}),
                      ind = reactive({as.numeric(input$id)})
                      )

  callModule(mod_format_DT_server,'out', 
             table2show = reactive({
               req(r$ret)
               req(input$id)
               assay(r$ret()[[as.numeric(input$id)]])
               }),
             style = reactive({ NULL})) 
  
  callModule(mod_format_DT_server,'temoin', 
             table2show = reactive({
               req(input$id)
               assay(obj[[as.numeric(input$id)]])
               }),
             style = reactive({ NULL})) 
  
 
  
}

## run app 
shinyApp(ui=ui, server=server)

