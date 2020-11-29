library(shiny)
library(R6)
library(tibble)

options(shiny.fullstacktrace = T)

#------------------------ Class TimelineDraw --------------------------------------
source(file.path('.', 'class_TimelineDraw.R'), local=TRUE)$value

source(file.path('../../../../R', 'mod_insert_md.R'), local=TRUE)$value
source(file.path('../../../../R', 'global.R'), local=TRUE)$value

# ------------- Class TimelineDataManager  --------------------------------------
source(file.path('.', 'class_abstract_TimelineManager.R'), local=TRUE)$value
source(file.path('.', 'class_TimelineForProcess.R'), local=TRUE)$value
source(file.path('.', 'class_TimelineForPipeline.R'), local=TRUE)$value



#----------------------- Class ProcessManager ----------------------------------
source(file.path('.', 'class_abstract_ProcessManager.R'), local=TRUE)$value
source(file.path('.', 'class_Process.R'), local=TRUE)$value
source(file.path('.', 'class_Filtering.R'), local=TRUE)$value
source(file.path('.', 'class_Description.R'), local=TRUE)$value


#----------------------------------------------------------------------------


ui = function() {
  fluidPage(
    wellPanel(style="background-color: green;",
              h3('Prostar'),
              actionButton('remoteReset', 'Simulate remote reset'),
              actionButton('skip', 'Simulate skip entire process'),
              actionButton('changeDataset', 'Simulate change of dataset'),
              uiOutput('show_ui')
    )
  )
}
server = function(input, output, session) {
  ns <- NS('App')
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  rv = reactiveValues(
    dataIn = NULL,
    remoteReset = NULL
  )
  
  dataOut <- reactiveValues()
  
  ll.process = list()
  ll.process[['processA']] <- ProcessFiltering$new(ns("ProcessA"))
  ll.process[['Description']] <- ProcessDescription$new(ns("process_Description"))

  
  rv[['description']] <- ll.process[['Description']]$server(
    dataIn = reactive({rv$dataIn}),
    remoteReset = reactive({input$remoteReset}),
    isSkipped = reactive({input$skip %%2 == 0})
    )

  #rv[['processA']] <- ll.process[['processA']]$server(
  #  dataIn = reactive({rv$dataIn}),
  #  remoteReset = reactive({input$remoteReset}),
  #  isSkipped = reactive({input$skip %%2 == 0}))
  
  
  output$show_ui <- renderUI({
    tagList(
      #ll.process[['processA']]$ui(),
      hr(),
      ll.process[['Description']]$ui()
    )
  })
  
  
  observeEvent(input$changeDataset,{
    if (input$changeDataset%%2 ==0)
      rv$dataIn <- Exp1_R25_prot[1:10, , -1]
    else
      rv$dataIn <- NA
  })
  

}

shinyApp(ui, server)