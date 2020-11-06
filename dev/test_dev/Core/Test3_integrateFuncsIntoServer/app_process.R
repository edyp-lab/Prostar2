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
  
 
  processA <- ProcessFiltering$new("ProcessA")
 # processDescription <- ProcessDescription$new("process_Description")
  
  
  # processDescription$server(
  #   dataIn = reactive({rv$dataIn}),
  #   remoteReset = reactive({input$remoteReset}),
  #   isSkipped = reactive({input$skip %%2 == 0})
  # )
  
  processA$server(
    config = list(steps = c('Description', 'Step1', 'Step2', 'Step3'),
                  mandatory = setNames(c(F,F,F,F), c('Description', 'Step1', 'Step2', 'Step3')),
                  status = setNames(c(0, 0, 0, 0), c('Description', 'Step1', 'Step2', 'Step3')),
                  name = 'Filtering'),
    dataIn = reactive({rv$dataIn}),
    remoteReset = reactive({input$remoteReset}),
    isSkipped = reactive({input$skip %%2 == 0}))
  
  
  output$show_ui <- renderUI({
    req(processA)
    tagList(
      processA$ui(),
      hr()
      #processDescription$ui()
    )
  })
  

}

shinyApp(ui, server)