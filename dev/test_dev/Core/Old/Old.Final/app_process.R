library(shiny)
library(R6)
library(tibble)

verbose = F

options(shiny.fullstacktrace = T)

#------------------------ Class TimelineDraw --------------------------------------
source(file.path('.', 'class_TimelineDraw.R'), local=TRUE)$value

source(file.path('../../../../R', 'mod_insert_md.R'), local=TRUE)$value
source(file.path('../../../../R', 'global.R'), local=TRUE)$value
source(file.path('.', 'class_global.R'), local=TRUE)$value

#----------------------- Class ProcessManager ----------------------------------
source(file.path('.', 'class_ProcessManager.R'), local=TRUE)$value
source(file.path('.', 'class_Process.R'), local=TRUE)$value
source(file.path('.', 'class_ProcessA.R'), local=TRUE)$value
#source(file.path('.', 'class_ProcessOriginal.R'), local=TRUE)$value


#----------------------------------------------------------------------------





Pipeline <- R6Class(
  "Pipeline",
  public = list(
    id = NULL,
    
    tmp.return = reactiveValues(),
    
    rv = reactiveValues(
      dataIn = NULL,
      isReseted = FALSE,
      isSkipped = FALSE
      ),
    
    child.process = list(
      #ProcessOriginal = NULL
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
              actionButton(ns('skip'), 'Simulate skip process'),
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
  

  self$child.process <- setNames(lapply(names(self$child.process),
                                     function(x){
                                       assign(x, get(x))$new(x)
                                       }),
                              names(self$child.process)
  )

  lapply(names(self$child.process), function(x){
    self$tmp.return[[x]] <- self$child.process[[x]]$server(dataIn = reactive({Exp1_R25_prot}))
  })
  

  observeEvent(lapply(names(self$child.process), function(x){self$tmp.return[[x]]()$trigger}), {
    print(lapply(names(self$child.process), function(x){self$tmp.return[[x]]()$value}))
  })
  

  moduleServer(self$id, function(input, output, session) {
    ns <- NS(self$id)
    
    observe({ 
      lapply(names(self$child.process), function(x){
      self$child.process[[x]]$SetSkipped(self$rv$isSkipped )
        })
      
      lapply(names(self$child.process), function(x){
        self$child.process[[x]]$SetReseted(self$rv$isReseted )
      })
    })
    
  observeEvent(input$reset,{self$rv$isReseted <- input$reset})
  observeEvent(input$skip,{self$rv$isSkipped <- input$skip%%2!=0})
  
  output$show_ui <- renderUI({
    tagList(
     lapply(names(self$child.process), function(x){
        wellPanel(h3(x), self$child.process[[x]]$ui())
      })
    )
  })
  
  output$show_dataIn <- renderUI({
    req(dataIn())
    tagList(
      lapply(names(dataIn()), function(x){tags$p(x)})
    )
  })
  
  
  output$show_rv_dataOut <- renderUI({

    req(lapply(names(self$child.process), function(x){self$tmp.return[[x]]()$trigger}))
    tagList(
      lapply(names(self$child.process),function(x){
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
  Pipeline$ui()
)
  
server = function(input, output){
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  Pipeline$server(dataIn = reactive({rv$dataIn}))
  
  observeEvent(input$changeDataset,{
    if (input$changeDataset%%2 != 0)
      rv$dataIn <- Exp1_R25_prot
    else
      rv$dataIn <- NULL
  })
  
  }
shiny::shinyApp(ui, server)