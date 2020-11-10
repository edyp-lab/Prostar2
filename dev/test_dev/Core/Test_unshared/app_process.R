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



#----------------------- Class ProcessManager ----------------------------------
source(file.path('.', 'class_abstract_ProcessManager.R'), local=TRUE)$value
source(file.path('.', 'class_Process.R'), local=TRUE)$value
source(file.path('.', 'class_ProcessA.R'), local=TRUE)$value
source(file.path('.', 'class_ProcessDescription.R'), local=TRUE)$value


#----------------------------------------------------------------------------





Pipeline <- R6Class(
  "Pipeline",
  public = list(
    id = NULL,
    tmp.return = reactiveValues(),
    rv = reactiveValues(
      dataIn = NULL,
      remoteReset = NULL,
      skipped = NULL
      ),
    ll.process = list(
      #ProcessDescription = NULL,
      ProcessA = NULL
    ),
    initialize = function(id){
      self$id <- id
    },
    
ui = function() {
  ns <- NS(self$id)
  fluidPage(
    wellPanel(style="background-color: green;",
              h3('Prostar'),
              actionButton(ns('remoteReset'), 'Simulate remote reset'),
              actionButton(ns('skip'), 'Simulate skip entire process'),
              uiOutput(ns('show_ui')),
              fluidRow(
                column(width=2,
                       tags$b(h4(style = 'color: blue;', "Input")),
                       uiOutput(ns('show_dataIn'))),
                column(width=2,
                       tags$b(h4(style = 'color: blue;', "Output")),
                       uiOutput(ns('show_rv_dataOut')))
                # column(width=4,
                #        tags$b(h4(style = 'color: blue;', "status")),
                #        uiOutput(ns('show_status')))
              )
    )
  )
},
server = function(dataIn ) {
  ns <- NS(self$id)
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  observeEvent(dataIn(),{self$rv$dataIn <- dataIn()})
  
  browser()
  self$ll.process <- setNames(lapply(names(self$ll.process),
                                     function(x){
                                       assign(x, get(x))$new(x)
                                       }),
                              names(self$ll.process)
  )
  browser()
  # self$tmp.return[['ProcessDescription']] <- self$ll.process[['ProcessDescription']]$server(dataIn = reactive({self$rv$dataIn}),
  #                                                                       remoteReset = reactive({self$rv$remoteReset}),
  #                                                                       isSkipped = reactive({self$rv$skipped %%2 == 0}))
  # 
  self$tmp.return[['ProcessA']] <- self$ll.process[['ProcessA']]$server(dataIn = reactive({self$rv$dataIn}),
                                                                        remoteReset = reactive({self$rv$remoteReset}),
                                                                        isSkipped = reactive({self$rv$skipped %%2 == 0}))
  
  
  # observeEvent(self$tmp.return[['ProcessDescription']]()$trigger, {
  #   print("change in ProcessDescription")
  #   print(paste0("self$rv$tmp.return[['ProcessDescription']]()= ", paste0(self$tmp.return[['ProcessDescription']]()$value, collapse=' ')))
  # })
  
  observeEvent(self$tmp.return[['ProcessA']]()$trigger, {
    print("change in ProcessA")
    print(paste0("self$rv$tmp.return[['ProcessA']]() = ", paste0(names(self$tmp.return[['ProcessA']]()$value), collapse=' ')))
  })
  
  
  moduleServer(self$id, function(input, output, session) {
    ns <- NS(self$id)
    
  observeEvent(input$remoteReset,{rv$remoteReset <- input$remoteReset})
  observeEvent(input$skip,{rv$skipped <- input$skip})
  
  output$show_ui <- renderUI({
    tagList(
     # wellPanel(h3('ProcessDescription'), self$ll.process[['ProcessDescription']]$ui()),
      wellPanel(h3('ProcessA A'), self$ll.process[['ProcessA']]$ui())
    )
  })
  
  output$show_dataIn <- renderUI({
    req(dataIn())
    tagList(
      h4('show data sent to processes'),
      lapply(names(dataIn()), function(x){tags$p(x)})
    )
  })
  
  
  output$show_rv_dataOut <- renderUI({
    req(self$tmp.return[['ProcessA']]()$trigger)
    tagList(
      h4('show return of processes'),
      lapply(names(self$ll.process),function(x){
         tags$p(paste0(x, ' -> ',paste0(names(self$tmp.return[[x]]()$value), collapse=' ')))
        
      })
    )
  })
  

  })
}
)
)

rv <- reactiveValues()
Pipeline <- Pipeline$new('App')
ui = fluidPage(
  tagList(
    actionButton('changeDataset','Simulate new dataset'),
    Pipeline$ui()
    )
)
  
server = function(input, output){
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  Pipeline$server(dataIn = reactive({rv$dataIn}))
  
  
  observeEvent(input$changeDataset,{
    if (input$changeDataset%%2 ==0)
      rv$dataIn <- Exp1_R25_prot[1:10, , -1]
    else
      rv$dataIn <- NA
  })
  
  }
shiny::shinyApp(ui, server)