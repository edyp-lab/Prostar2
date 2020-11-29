ProcessManager <- R6Class(
  "ProcessManager",
  private = list(
    id = NULL,
    config = list(),
    steps = NULL,
    length = NULL,
    logics = NULL
    
  ),
  
  public = list(
    initialize = function(id){
      private$id <- id
      configProcess <- source(file.path('.', paste0('config_', id, '.R')), local=TRUE)$value
      
      lapply(names(configProcess$config), function(x){private$config[[x]] <- configProcess$config[[x]]})
    },
    
    
    ui = function(){
      tagList(
        actionButton(NS(private$id)('change'), paste0("change in ", private$id)),
        actionButton(NS(private$id)('add'), paste0("add in ", private$id))
      )
    },
    
    GetConfig = function(config){
      print(config$steps)
    },
    Add = function(){},
    server = function(){
      config = reactiveValues()
      moduleServer(private$id, function(input, output, session) {
        
        observeEvent(private$config,{
          lapply(names(private$config), function(x){config[[x]] <- private$config[[x]]})
        })
        
        
        observeEvent(config, { print("change ")})
        
        observeEvent(input$change, {config$steps[1] <- as.character(input$change)})
        observeEvent(input$add, {config$steps[1] <- self$Add(length(config$steps)) })
        observeEvent(config$steps, { self$GetConfig(config) })
      }
      )
    }
    
  )
)