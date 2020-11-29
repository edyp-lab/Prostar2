library(shiny)
library(R6)


source(file.path('../../../../R', 'mod_insert_md.R'), local=TRUE)$value
source(file.path('../../../../R', 'global.R'), local=TRUE)$value


ScreenManagerClass <- R6Class(
  "ScreenManagerClass",
  private = list(
    config = list(),
    rv = reactiveValues(
      current.pos = 1,
      reset_OK = NULL
    )
  ),
  public = list(
    id = NULL,
    initialize = function(id){
      self$id <- id
    },
    
    ui = function(){
      ns <- NS(self$id)
      tagList(
        shinyjs::useShinyjs(),
        actionButton(ns('action'), 'Action'),
        uiOutput(ns('showScreen'))
        )
    },
    
    server = function(show, config){
      ns <- NS(self$id)
      
      
      observeEvent(config,{
        private$config <- config
        private$config$screens <- lapply(1:length(private$config$screens),
                                         function(x){
                                           private$config$screens[[x]] <- div(id = ns(paste0("div_screen", x)),  private$config$screens[[x]])
                                         })
      })
      
      
      
      moduleServer(self$id, function(input, output, session) {
        ns <- NS(self$id)
        
        
        observeEvent(show(), ignoreNULL=T,{
          lapply(1:length(private$config$screens), function(x){
            shinyjs::toggle(paste0('div_screen', x), condition = show())})
        })
        
        output$showScreen <- renderUI({ tagList(private$config$screens)})

        
        reactive({
          list(current.pos = input$action,
             reset = private$rv$reset_OK
        )
        })
      }
      )
    }
  )
)



##################################################################################################
ChildClass <- R6Class(
  "ChildClass",
  inherit = MotherClass,
  public = list(
    initialize = function(id, config){
      self$id <- id
      self$child <- ScreenManagerClass$new(NS(self$id)('child'))
      lapply(names(config), function(x){self$config[[x]] <- config[[x]]})
      self$steps <- names(config$mandatory)
       
    }
    
  )
)

##################################################################################################

MotherClass <- R6Class(
  "MotherClass",
  public = list(id = NULL,
                config = reactiveValues(),
                child = NULL,
                steps = NULL,
                rv = reactiveValues(
                  showScreenBtn=0,
                  launch = NULL,
                  timeline = NULL,
                  timeline.res = NULL
                  ),
                
                initialize = function(){
                  stop("AbstractClass is an abstract class that can't be initialized.")
                },
                
                ui = function(){
                  ns <- NS(self$id)
                  tagList(
                    shinyjs::useShinyjs(),
                    actionButton(ns('showScreenBtn'), "Show/hide screen"),
                    actionButton(ns('launch'), 'Launch screens'),
                    uiOutput(ns('showUI'))
                  )
                },

                
                #---------------------------------------------------------------------------
                CreateScreens = function(){
                  ns <- NS(self$id)
                  setNames(
                    lapply(1:length(self$steps), 
                           function(x){
                             do.call(uiOutput, list(outputId=ns(self$steps)[x]))}),
                    self$steps)
                },
                
                InitializeTimeline = function(){
                  self$rv$timeline.res <- self$child$server(show = reactive({self$rv$showScreenBtn}),
                                                            config = self$config
                  )
                  
                  observeEvent(self$rv$timeline.res(), {
                    print(paste0('new event : ', paste0(self$rv$timeline.res(), collapse=' ')))
                  })
                },
                server = function( logics){
                  ns <- NS(self$id)
                  
                  observeEvent(self$rv$launch,{self$InitializeTimeline()})

                  self$config$screens <- self$CreateScreens()
                 
                  moduleServer(self$id, function(input, output, session) {
                    
                    observeEvent(input$launch,{self$rv$launch <- input$launch})
                    
                    observeEvent(input$showScreenBtn,{
                      self$rv$showScreenBtn <- (input$showScreenBtn %% 2 ) == 0
                      })
                    
                    
                      logics(self, input, output)
                      output$showUI <- renderUI({ self$child$ui()})

                  }
                  )
                }
  )
)






##################################################################################################
source(file.path('.', 'process_A.R'), local=TRUE)$value

app <- ChildClass$new('toto', config)


ui = fluidPage(app$ui())

server = function(input, output){
  #browser()
  

    app$server(ProcessLogics)
}

shiny::shinyApp(ui, server)