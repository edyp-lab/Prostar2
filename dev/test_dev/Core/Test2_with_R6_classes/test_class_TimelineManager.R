library(R6)

source(file.path('.', 'class_ProcessManager.R'), local=TRUE)$value


btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"

#timelineManager <- TimelineForProcess$new(NS('App')('timeline'), style=2)
processManager <- ProcessManager$new(NS('App')('Filtering'))

options(shiny.fullstacktrace = T)

ui = function(){ processManager$ui()}


# Define server logic to summarize and view selected dataset ----
server = function(input, output, session) {
  rv = reactiveValues(data = 3)
  dataOut <- reactiveValues()
  
  processManager$server(dataIn = reactive({rv$data}),
                        dataOut = dataOut)
  
}


shinyApp(ui, server)
