library(QFeatures)
library(shiny)
library(Prostar2)
source(file.path('../R', 'global.R'), local=TRUE)$value
source(file.path('../R', 'mod_navigation.R'), local=TRUE)$value

#' @importFrom shiny NS tagList 
mod_pipe_process_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_navigation_ui(ns('nav_pipe_process'))
  )
}

#' pipe_process Server Function

mod_pipe_process_server <- function(input, output, session, obj){
  ns <- session$ns
  
  
  ## Section navigation module
  # Variable to manage the different screens of the module
  r.nav <- reactiveValues(
    name = "Foo",
    stepsNames = c("Choose assay", "Change", "Save"),
    ll.UI = list( screenStep1 = uiOutput(ns("Screen_Process_1")),
                  screenStep2 = uiOutput(ns("Screen_Process_2")),
                  screenStep3 = uiOutput(ns("Screen_Process_3"))
    ),
    isDone =  rep(FALSE,3),
    mandatory =  rep(TRUE,3),
    reset = FALSE
  )
  
  ## reactive values for variables in the module
  rv.process <- reactiveValues(
    name = "processProtNorm",
    dataIn = NULL,
    dataOut = NULL,
    i = NULL,
    settings = NULL,
    widgets = list(assay = 0,
                   operator = NULL,
                   operand = NULL)
  )
  
  
  observeEvent(req(r.nav$reset),{
    
    rv.process$widgets <- list(assay = 0,
                               operator = NULL,
                               operand = NULL)
    
    ## do not modify this part
    rv.process$dataIn <- obj()
    rv.process$data <- data.frame()
    r.nav$isDone <- rep(FALSE, 3)
    r.nav$reset <- FALSE
    ## end of no modifiable part
  })
  
  
  mod_navigation_server('nav_pipe_process', style=2, pages=r.nav)
  
  #### END of template part of the module
  
  
  
  ##
  ##  
  ## Calls to other modules
  ##
  ##
  
  # Add new modules via 'callModule'
  # for example, the module for settings: mod_settings
  
  rv.process$settings <- mod_settings_server("settings", 
                                             obj = reactive({obj()}))
  
  
  
  
  observe({
    req(obj())
    rv.process$dataIn <- obj()
  })
  
  
  
  
  ##
  ## Definitions of the screens
  ##
  
  ###---------------------------------------------------------------------------------###
  ###                                 Screen 1                                        ###
  ###---------------------------------------------------------------------------------###
  output$Screen_Process_1 <- renderUI({
    selectInput(ns('selectAssay'), 
                'Select assay', 
                choices=0:length(rv.process$dataIn), 
                selected=rv.process$widgets$assay)
  })
  
  
  observeEvent(input$selectAssay, {
    rv.process$widgets$assay <- as.numeric(input$selectAssay)
  })
  
  
  observe({
    if (rv.process$widgets$assay > 0)
      r.nav$isDone[1] <- TRUE
  })
  
  
  ###---------------------------------------------------------------------------------###
  ###                                 Screen 2                                        ###
  ###---------------------------------------------------------------------------------###
  
  output$Screen_Process_2 <- renderUI({
    tagList(
      radioButtons(ns('operator'), 'Choose operator',
                   choices = c('addition' = 'addition',
                               'soustraction' = 'soustraction',
                               'product' = 'product'),
                   selected = rv.process$widgets$operator
      ),
      numericInput(ns('operand'), 
                   'Choose operand', 
                   value = rv.process$widgets$operand, 
                   min=0, 
                   max=10),
      actionButton(ns('change'), 'Apply operator')
    )
  })
  
  
  
  
  observeEvent(input$operator, ignoreInit=TRUE,{
    rv.process$widgets$operator <- input$operator
  })
  
  observeEvent(input$operand, ignoreInit=TRUE,{
    rv.process$widgets$operand <- input$operand
  })
  
  
  
  observeEvent(input$change,{
    
    tmp <- rv.process$dataIn[[rv.process$widgets$assay]]
    
    switch (rv.process$widgets$operator,
            addition = assay(tmp) <- assay(tmp) + rv.process$widgets$operand,
            soustraction = assay(tmp) <- assay(tmp) - rv.process$widgets$operand,
            product = assay(tmp) <- assay(tmp) * rv.process$widgets$operand
    )
    
    rv.process$dataIn <- QFeatures::addAssay(rv.process$dataIn,
                                             tmp,
                                             'tutorial')
    r.nav$isDone[2] <- TRUE
  })
  
  
  ###---------------------------------------------------------------------------------###
  ###                                 Screen 3                                        ###
  ###---------------------------------------------------------------------------------###
  
  output$Screen_Process_3 <- renderUI({
    actionButton(ns("save"), "Save")
  })
  
  
  
  observeEvent(input$save,{ 
    metadata(rv.process$dataIn[[rv.process$widgets$assay+1]])$Params <- list(
      operand = rv.process$widgets$operand,
      operator = rv.process$widgets$operator
    )
    
    rv.process$dataOut <- rv.process$dataIn
    r.nav$isDone[3] <- TRUE
  })
  
  return({reactive(rv.process$dataOut)})
  
}








ui <- fluidPage(
  dataTableOutput('dataset'),
  mod_pipe_process_ui('foo')
)


server <- function(input, output, session) {
  
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  obj<-Exp1_R25_prot
  
  
  rv <- reactiveValues(
    test = obj,
    data = data.frame()
  )
  
  
  rv$test <- mod_pipe_process_server('foo', obj=reactive({obj}) )
  
  
  observeEvent(rv$test(),{
    
    if (is.null(rv$test()))
      rv$data <- data.frame()
    else
    {
      for (i in 1:length(rv$test()))
        rv$data <- rbind(rv$data, assay(rv$test()[[i]])[1,])
      colnames(rv$data) <- colnames(assay(rv$test()))
    }
  })
  
  
  output$dataset <- renderDataTable({
    rv$data
  })
  
}

shinyApp(ui, server)                    
