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

#----------------------- Class ScreenManager ----------------------------------
source(file.path('.', 'class_ScreenManager.R'), local=TRUE)$value
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
      isReseted = FALSE,
      isSkipped = FALSE
      ),
    child.process = list(
      #ProcessDescription = NULL,
      ProcessA = NULL
    ),
    initialize = function(id){
      self$id <- id
      self$child.process <- setNames(lapply(names(self$child.process),
                                            function(x){
                                              assign(x, get(x))$new(x)
                                            }),
                                     names(self$child.process)
      )
    },
    
    
    ActionOn_Reset = function(){
      cat(paste0(class(self)[1], '::', 'ActionsOnReset() from - ', self$id, '\n'))
      #browser()
      #browser()
      # Say to all child processes to reset themselves
      if (!is.null(self$child.process))
        lapply(names(self$child.process), function(x){
          self$child.process[[x]]$Set_All_Reset()
        })

    },
    
    ActionOn_Skipped = function(){
      cat(paste0(class(self)[1], '::', 'ActionsOnReset() from - ', self$id, '\n'))
      #browser()
      #browser()
      # Say to all child processes to reset themselves
      if (!is.null(self$child.process))
        lapply(names(self$child.process), function(x){
          self$child.process[[x]]$Set_All_Skipped()
        })
      
    },
    
    Launch_Servers = function(data){

      lapply(names(self$child.process), function(x){
        self$tmp.return[[x]] <- self$child.process[[x]]$server(dataIn = reactive({data()}))
      })
      
      observeEvent(lapply(names(self$child.process), function(x){self$tmp.return[[x]]()$trigger}), {
        print(lapply(names(self$child.process), function(x){self$child.process[[x]]$Get_Result()}))
      })
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
                       tags$b(h4(style = 'color: blue;', "Input of pipeline")),
                       uiOutput(ns('show_dataIn'))),
                column(width=2,
                       tags$b(h4(style = 'color: blue;', "Output of pipeline")),
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
  
  self$Launch_Servers(data = reactive({dataIn()}))
  
  moduleServer(self$id, function(input, output, session) {
    ns <- NS(self$id)
    
    observeEvent(self$rv$isSkipped , ignoreInit=T,{ 
      lapply(names(self$child.process), function(x){
      self$child.process[[x]]$SetSkipped(self$rv$isSkipped )
        })
    })

  observeEvent(input$reset,{self$ActionOn_Reset()})
  
  observeEvent(input$skip,{self$ActionOn_Skipped()})
  
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
         tags$p(paste0(x, ' -> ',paste0(names(self$child.process[[x]]$Get_Result()), collapse=' ')))
        
      })
    )
  })
  

  })
}
)
)


## Main app
AddItemToDataset <- function(dataset, name){
  addAssay(dataset, 
           dataset[[length(dataset)]], 
           name=name)
}

rv <- reactiveValues()
Pipeline <- Pipeline$new('App')
ui = fluidPage(
  tagList(
    actionButton('send', 'Send dataset'),
    Pipeline$ui()
  )
)
  
server = function(input, output){
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  Pipeline$server(dataIn = reactive({rv$dataIn}))
  
  observeEvent(input$send,{
    if (input$send%%2 != 0)
      rv$dataIn <- Exp1_R25_prot[,,2]
    else
      rv$dataIn <- NULL
  })
  
  }
shiny::shinyApp(ui, server)