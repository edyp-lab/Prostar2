library(shiny)
library(SummarizedExperiment)


source(file.path('../../R', 'mod_plots_tracking.R'), local=TRUE)$value


ui <- fluidPage(
   fluidRow(
     column(6,tagList(h3('Master'),
                      mod_plots_tracking_ui('master_tracking')
                      )
            ),
            column(6,tagList(h3('Slave'),
                             mod_plots_tracking_ui('slave_tracking')
            )
            )
  )
)


server <- function(input, output, session) {
  
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  keyId <- metadata(Exp1_R25_prot)[['keyId']]
  obj<-Exp1_R25_prot[[2]]
  
  r <- reactiveValues(
    master = NULL,
    slave = NULL
  )
  
  rowData(obj) <- cbind(rowData(obj), ProtOfInterest=sample(c(0,1), nrow(obj), TRUE))
  
  r$master <- callModule(mod_plots_tracking_server,'master_tracking', 
                      obj = reactive({obj}), 
                      params=reactive({NULL}),
                      keyId=reactive({keyId}),
                      reset=reactive({FALSE}) )
  
  r$slave <- callModule(mod_plots_tracking_server,'slave_tracking', 
                      obj = reactive({obj}), 
                      params=reactive({r$master()}),
                      keyId=reactive({keyId}),
                      reset=reactive({FALSE}) )
  
  
  # observe({
  #   r$slave()
  #   print(r$slave())
  # })
  

  
  # observeEvent(r$master(),{
  #   print(r$master())
  # })
}


shinyApp(ui, server)
