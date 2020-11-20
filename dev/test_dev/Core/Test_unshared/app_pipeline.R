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
source(file.path('.', 'class_Generic_Process.R'), local=TRUE)$value
source(file.path('.', 'class_ProcessA.R'), local=TRUE)$value
source(file.path('.', 'class_ProcessB.R'), local=TRUE)$value
source(file.path('.', 'class_ProcessDescription.R'), local=TRUE)$value

source(file.path('.', 'class_Generic_Pipeline.R'), local=TRUE)$value
source(file.path('.', 'class_Pipeline_1.R'), local=TRUE)$value

#----------------------------------------------------------------------------

## Main app
rv <- reactiveValues()
pipeline <- Pipeline1$new('App')

ui = fluidPage(
  tagList(
    actionButton('changeDataset','Simulate new dataset'),
    pipeline$ui()
  )
)

server = function(input, output){
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  pipeline$server(dataIn = reactive({rv$dataIn}))
  
  
  observeEvent(input$changeDataset,{
    
    if (input$changeDataset%%2 ==0)
      rv$dataIn <- Exp1_R25_prot[1:10, , -1]
    else
      rv$dataIn <- NULL
  })
}

shiny::shinyApp(ui, server)