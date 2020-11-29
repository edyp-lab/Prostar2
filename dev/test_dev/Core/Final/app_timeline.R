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

# ------------- Class TimelineDataManager  --------------------------------------
source(file.path('.', 'class_TimelineManager.R'), local=TRUE)$value
source(file.path('.', 'class_TimelineForProcess.R'), local=TRUE)$value


##
## Main app
##

TestTL <- R6Class(
  'TestTL',
  public = list(
    id = NULL,
    rv = "<reactiveValues>",
    config = "<reactiveValues>",
    timeline = NULL,
    
    initialize = function(id){
      self$id <- id
      self$rv <- reactiveValues()
      self$config <- reactiveValues(
        name = 'ProcessA',
        steps = c('Description', 'Step1', 'Step2', 'Step3'),
        mandatory = c(T,F,F,F),
        status = c(0,0,0,0),
        screens = NULL
      )
      
      
    },
    
Description = function(){
  ns <- NS(self$id)
  
  observe({
    mod_insert_md_server(paste0(self$config$name, "_md"), 
                         paste0('./md/', self$config$name, '.md'))
  })
  
  observeEvent(self$input$btn_validate_Description, {
    self$InitializeDataIn()
    self$ValidateCurrentPos()
  })
  
  tagList(
    actionButton(ns('btn_validate_Description'), 
                 paste0('Start ', self$config$name),
                 class = btn_success_color),
    mod_insert_md_ui(ns(paste0(self$config$name, "_md")))
  )
},

############### SCREEN 2 ######################################

Step1 = function(){
  ns <- NS(self$id)
  name <- 'Step1'
  
  observeEvent(self$input$btn_validate_Step1, {
    self$ValidateCurrentPos()
  })
  
  tagList(
    div(id=ns(name),
        div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
            tags$h2(name)),
        div(style="display:inline-block; vertical-align: middle; padding-right: 40px;",
            selectInput(ns('select1'), 'Select step 1', 
                        choices = 1:5, 
                        selected = 1,
                        width = '150px')
        ),
        div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
            actionButton(ns(paste0('btn_validate_', name)), 'Perform'))
    )
  )
  
},

############### SCREEN 3 ######################################
Step2 = function(){
  ns <- NS(self$id)
  name <- 'Step2'
  
  
  ## Logics to implement: here, we must take the last data not null
  # in previous datas. The objective is to take account
  # of skipped steps
  observeEvent(self$input$btn_validate_Step2, {
    self$ValidateCurrentPos()
  })
  
  tagList(
    div(id=ns(name),
        div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
            tags$h3(name)),
        div(style="display:inline-block; vertical-align: middle;padding-right: 40px;",
            selectInput(ns('select2'), 'Select step 2',
                        choices = 1:5,
                        selected = 1,
                        width = '150px')),
        div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
            actionButton(ns(paste0('btn_validate_', name)), 'Perform'))
    )
  )
},

############### SCREEN 4 ######################################
Step3 = function(){
  ns <- NS(self$id)
  name <- 'Step3'
  
  observeEvent(self$input$btn_validate_Step3, {
    # rv$dataIn <- AddItemToDataset(self$rv$dataIn, self$config$process.name)
    self$ValidateCurrentPos()
  })
  
  tagList(
    div(id=ns(name),
        div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
            tags$h3(name)),
        div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
            actionButton(ns(paste0('btn_validate_', name)), 'Validate'))
    )
  )
  
},


ui = function(){
  ns <- NS(self$id)
  fluidPage(
  tagList(
    uiOutput(ns('show_TL'))
  )
)
  },

server = function(){
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  observe({
    self$config$screens <- 
      setNames(lapply(self$config$steps, function(x){
        eval(parse(text = paste0('self$', x, '()')))
      }),
      self$config$steps)
    
    self$timeline <- TimelineForProcess$new(
      id = 'timeline',
      name = self$config$name,
      steps = self$config$steps,
      mandatory = self$config$mandatory,
      screens = self$config$screens
      )
    
    self$timeline$server(status = reactive({self$config$status}))
    
    
  })
  
  
  
  moduleServer(self$id, function(input, output, session) {

  output$show_TL <- renderUI({ 
    req(self$timeline)
    self$timeline$ui()
    })
  })

}
)
)



### Main App

test <- TestTL$new('test')
ui = fluidPage(
  tagList(
    test$ui()
  )
)

server = function(input, output){
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  test$server()
}



shiny::shinyApp(ui, server)