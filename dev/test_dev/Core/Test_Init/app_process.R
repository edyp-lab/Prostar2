library(shiny)
library(R6)
library(tibble)

verbose = F

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
source(file.path('.', 'class_Generic_Process.R'), local=TRUE)$value
source(file.path('.', 'class_ProcessA.R'), local=TRUE)$value
source(file.path('.', 'class_ProcessDescription.R'), local=TRUE)$value


#----------------------------------------------------------------------------





Pipeline <- R6Class(
  "Pipeline",
  public = list(
    id = NULL,
    tmp.return = reactiveValues(),
    reset = NULL,
    rv = reactiveValues(
      dataIn = NULL,
      reset = NULL,
      skipped = NULL
      ),
    ll.process = list(
      #ProcessDescription = NULL
      ProcessA = NULL
    ),
    initialize = function(id){
      self$id <- id
    },
    
ui = function() {
  ns <- NS(self$id)
  fluidPage(
    #wellPanel(style="background-color: green;",
              h3('Prostar'),
              actionButton(ns('reset'), 'Simulate remote reset'),
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
  #)
},
server = function(dataIn ) {
  cat(paste0(class(self)[1], '::server()\n'))
  ns <- NS(self$id)
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  observeEvent(dataIn(),{
    cat(paste0(class(self)[1], '::observeEvent(dataIn())\n'))
    self$rv$dataIn <- dataIn()
    })
  

  self$ll.process <- setNames(lapply(names(self$ll.process),
                                     function(x){
                                       assign(x, get(x))$new(x)
                                       }),
                              names(self$ll.process)
  )

  lapply(names(self$ll.process), function(x){
    self$tmp.return[[x]] <- self$ll.process[[x]]$server(dataIn = reactive({self$rv$dataIn}),
                                                        reset = reactive({self$rv$reset}),
                                                        isSkipped = reactive({self$rv$skipped}))
  })
  

  observeEvent(lapply(names(self$ll.process), function(x){self$tmp.return[[x]]()$trigger}), {
    print(lapply(names(self$ll.process), function(x){self$tmp.return[[x]]()$value}))
  })
  

  moduleServer(self$id, function(input, output, session) {
    ns <- NS(self$id)
    
  
  observeEvent(input$reset,{self$rv$reset <- input$reset})
  observeEvent(input$skip,{self$rv$skipped <- input$skip})
  
  output$show_ui <- renderUI({
    tagList(
     lapply(names(self$ll.process), function(x){
        wellPanel(h3(x), self$ll.process[[x]]$ui())
      })
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

    req(lapply(names(self$ll.process), function(x){self$tmp.return[[x]]()$trigger}))
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


## Main app
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
    print(input$changeDataset)
    if (input$changeDataset%%2 != 0)
  rv$dataIn <- 1
    else
      rv$dataIn <- NULL
  })
  
  }
shiny::shinyApp(ui, server)