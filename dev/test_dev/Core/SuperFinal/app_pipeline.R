library(shiny)
library(R6)
library(tibble)

options(shiny.fullstacktrace = T)
options(shiny.reactlog=TRUE) 

#verbose <-'skip'
verbose = F

#------------------------ Class TimelineDraw --------------------------------------
source(file.path('.', 'class_TimelineDraw.R'), local=TRUE)$value

source(file.path('../../../../R', 'mod_insert_md.R'), local=TRUE)$value
source(file.path('../../../../R', 'global.R'), local=TRUE)$value
source(file.path('.', 'class_global.R'), local=TRUE)$value

#----------------------- Class ProcessManager ----------------------------------
source(file.path('.', 'class_ScreenManager.R'), local=TRUE)$value
source(file.path('.', 'class_Process.R'), local=TRUE)$value
source(file.path('.', 'class_ProcessA.R'), local=TRUE)$value
source(file.path('.', 'class_ProcessB.R'), local=TRUE)$value
source(file.path('.', 'class_ProcessDescription.R'), local=TRUE)$value

source(file.path('.', 'class_Pipeline.R'), local=TRUE)$value
source(file.path('.', 'class_PipelineSimple.R'), local=TRUE)$value

# source(file.path('.', 'class_WF1_Original.R'), local=TRUE)$value
# source(file.path('.', 'class_WF1_Filtering.R'), local=TRUE)$value
# source(file.path('.', 'class_WF1_Normalization.R'), local=TRUE)$value
# source(file.path('.', 'class_WF1_Imputation.R'), local=TRUE)$value
# source(file.path('.', 'class_WF1_HypothesisTest.R'), local=TRUE)$value



#----------------------------------------------------------------------------


AddItemToDataset <- function(dataset, name){
  addAssay(dataset, 
           dataset[[length(dataset)]], 
           name=name)
}

## Main app

ui = fluidPage(
  tagList(
    actionButton('send', 'Send dataset'),
    uiOutput('show_ui')
  )
)

server = function(input, output, session){
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  rv <- reactiveValues()
  pipeline <- NULL
  #pipeline <- Pipeline_WF1$new('App')
  
    pipeline <- PipelineSimple$new('App')
    observe({
  pipeline$server(dataIn = reactive({rv$dataIn}))
  })
  output$show_ui <- renderUI({
    cat('::output$show \n')
    req(pipeline)
    pipeline$ui()
  })
  
  observeEvent(input$send,{
    if (input$send%%2 != 0)
      rv$dataIn <- Exp1_R25_prot[,,2]
    else
      rv$dataIn <- NULL
  })
  
}

shiny::shinyApp(ui, server)