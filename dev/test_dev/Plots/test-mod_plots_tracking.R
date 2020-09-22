library(shiny)
library(SummarizedExperiment)


source(file.path('../../../R/Plots', 'mod_plots_tracking.R'), local=TRUE)$value


ui <- fluidPage(
  checkboxInput('sync', 'sync Slave with Master', value=FALSE),
   fluidRow(
     column(6,tagList(h3('Master'),
                      mod_plots_tracking_ui('master_tracking')
                      )
            ),
            column(6,tagList(h3('Slave'),
                             mod_plots_tracking_ui('slave_tracking')
            )
            )
  ),
  
  fluidRow(
    column(6,tagList(h3('Output of master'),
                     uiOutput('master_out')
    )
    ),
    column(6,tagList(h3('Output of slave'),
                     uiOutput('slave_out')
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
                      reset=reactive({FALSE}),
                      slave = reactive({FALSE}))
  
  r$slave <- callModule(mod_plots_tracking_server,'slave_tracking', 
                      obj = reactive({obj}), 
                      params=reactive({if (input$sync) r$master() 
                        else NULL}),
                      keyId=reactive({keyId}),
                      reset=reactive({FALSE}),
                      slave=reactive({NULL}))
  
  
  # observe({
  #   r$slave()
  #   print(r$slave())
  # })
  
  
  output$master_out <- renderUI({
    r$master()
    
     tagList(
      p(paste0('type = ', r$master()$type)),
      p(paste0('list = ', paste0(r$master()$listSelect, collapse=', '))),
      p(paste0('rand = ', r$master()$randSelect)),
      p(paste0('col = ', r$master()$colSelect)),
      p(paste0('list.indices = ', paste0(r$master()$list.indices, collapse=', '))),
      p(paste0('rand.indices = ', paste0(r$master()$rand.indices, collapse=', '))),
      p(paste0('col.indices = ', paste0(r$master()$col.indices, collapse=', ')))
    )
  })

  
  output$slave_out <- renderUI({
    r$slave()
    
    tagList(
      p(paste0('type = ', r$slave()$type)),
      p(paste0('list = ', paste0(r$slave()$listSelect, collapse=', '))),
      p(paste0('rand = ', r$slave()$randSelect)),
      p(paste0('col = ', r$slave()$colSelect)),
      p(paste0('list.indices = ', paste0(r$slave()$list.indices, collapse=', '))),
      p(paste0('rand.indices = ', paste0(r$slave()$rand.indices, collapse=', '))),
      p(paste0('col.indices = ', paste0(r$slave()$col.indices, collapse=', ')))
    )
  })
  
  
}


shinyApp(ui, server)
