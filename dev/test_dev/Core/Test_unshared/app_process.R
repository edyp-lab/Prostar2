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





Super <- R6Class(
  "Super",
  public = list(
    id = NULL,
    tmp.return = reactiveValues(),
    rv = reactiveValues(
      dataIn = NULL,
      remoteReset = NULL,
      skipped = NULL
      ),
    ll.process = list(
      #Description = NULL,
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
              uiOutput(ns('show_ui'))
    )
  )
},
server = function(dataIn ) {
  ns <- NS(self$id)
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  observeEvent(dataIn(),{self$rv$dataIn <- dataIn()})
  
  
  lapply(names(self$ll.process), function(x){
    self$ll.process[[x]] <- ProcessA$new(ns(x))
  })
  
  #self$tmp.return[['Description']] <- self$ll.process[['Description']]$server()
  self$tmp.return[['ProcessA']] <- self$ll.process[['ProcessA']]$server(dataIn = reactive({self$rv$dataIn}),
                                                                        remoteReset = reactive({self$rv$remoteReset}),
                                                                        isSkipped = reactive({self$rv$skipped %%2 == 0}))
  
  
  # observeEvent(self$tmp.return[['Description']]()$trigger, {
  #   print("change in Description")
  #   print(paste0("self$rv$tmp.return[['Description']]()= ", paste0(self$tmp.return[['Description']]()$value, collapse=' ')))
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
      #wellPanel(h3('Description'), self$ll.process[['Description']]$ui()),
      wellPanel(h3('ProcessA A'), self$ll.process[['ProcessA']]$ui())
    )
  })
  

  })
}
)
)

rv <- reactiveValues()
super <- Super$new('App')
ui = fluidPage(
  tagList(
    actionButton('changeDataset','Simulate new dataset'),
    super$ui()
    )
)
  
server = function(input, output){
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  super$server(dataIn = reactive({rv$dataIn}))
  
  
  observeEvent(input$changeDataset,{
    if (input$changeDataset%%2 ==0)
      rv$dataIn <- Exp1_R25_prot[1:10, , -1]
    else
      rv$dataIn <- NA
  })
  
  }
shiny::shinyApp(ui, server)