library(shiny)
library(R6)


source(file.path('../../../../R', 'mod_insert_md.R'), local=TRUE)$value
source(file.path('../../../../R', 'global.R'), local=TRUE)$value


ScreenManagerClass <- R6Class(
  "ScreenManagerClass",
  private = list(
    config = list()
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
                  showScreenBtn=0
                  ),
                
                initialize = function(){
                  stop("AbstractClass is an abstract class that can't be initialized.")
                },
                
                ui = function(){
                  ns <- NS(self$id)
                  tagList(
                    shinyjs::useShinyjs(),
                    actionButton(ns('showScreenBtn'), "Show/hide screen"),
                    self$child$ui()
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
                
                server = function( logics){
                  ns <- NS(self$id)

                  self$child$server(show = reactive({self$rv$showScreenBtn}),
                                    config = self$config
                                    )
                  
                  self$config$screens <- self$CreateScreens()
                 
                  moduleServer(self$id, function(input, output, session) {
                    
                    observeEvent(input$showScreenBtn,{
                      self$rv$showScreenBtn <- (input$showScreenBtn %% 2 ) == 0
                      })
                    
                    logics(self$id, input, output)
                    #browser()
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
  CheckLogics = function(FUN){
    FUN('foo', input, output)
    out <- outputOptions(output)
  all(names(config$status) %in% names(out))
  }

  if (!CheckLogics(ProcessLogics)){
    warning("Your logics function is malformed. Some of renderUI functions are missing")
  return(NULL)
}

    app$server(ProcessLogics)
}

shiny::shinyApp(ui, server)