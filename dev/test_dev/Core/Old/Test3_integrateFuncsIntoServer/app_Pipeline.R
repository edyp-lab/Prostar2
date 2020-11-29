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
source(file.path('.', 'class_Pipeline.R'), local=TRUE)$value
source(file.path('.', 'class_PipelineProtein.R'), local=TRUE)$value
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
  
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  rv = reactiveValues(
    dataIn = NULL,
    remoteReset = NULL
  )
  
  dataOut <- reactiveValues()
  
  
  observeEvent(input$changeDataset,{
    if (input$changeDataset%%2 ==0)
      rv$dataIn <- Exp1_R25_prot[1:10, , -1]
    else
      rv$dataIn <- NA
  })
  
  
  pipelineManager <- PipelineProtein$new("PipelineProtein")
  
  
  pipelineManager$server(
    dataIn = reactive({rv$dataIn}),
    dataOut = dataOut,
    remoteReset = reactive({NULL}),
    isSkipped = reactive({NULL})
  )
  
  output$show_ui <- renderUI({
    req(pipelineManager)
    pipelineManager$ui()
  })
  
  observeEvent(req(dataOut$trigger), {
    print('reveceived response from a process')
  })
}

shinyApp(ui, server)