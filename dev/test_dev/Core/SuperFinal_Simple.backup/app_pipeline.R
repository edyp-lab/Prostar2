library(shiny)
library(R6)
library(tibble)

options(shiny.fullstacktrace = T)
options(shiny.reactlog=TRUE) 

#verbose <-'skip'
verbose = F

#------------------------ Class TimelineDraw -----------------------------------
source(file.path('.', 'class_TimelineDraw.R'), local=TRUE)$value

source(file.path('../../../../R', 'global.R'), local=TRUE)$value
source(file.path('.', 'class_global.R'), local=TRUE)$value

#----------------------- Class ProcessManager ----------------------------------
source(file.path('.', 'class_ProcessManager.R'), local=TRUE)$value
source(file.path('.', 'class_Process.R'), local=TRUE)$value
source(file.path('.', 'class_ProcessA.R'), local=TRUE)$value
source(file.path('.', 'class_ProcessB.R'), local=TRUE)$value
source(file.path('.', 'class_ProcessDescription.R'), local=TRUE)$value

source(file.path('.', 'class_Pipeline.R'), local=TRUE)$value
source(file.path('.', 'class_PipelineSimple.R'), local=TRUE)$value


## Main app
pipeline <- PipelineSimple$new('App')

ui = fluidPage(pipeline$ui())

server = function(input, output, session){
  rv <- reactiveValues()
  pipeline$server(dataIn = reactive({rv$dataIn}))
}

shiny::shinyApp(ui, server)