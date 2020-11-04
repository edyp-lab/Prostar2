#Timeline_R6.R
source(file.path('.', 'class_abstract_ProcessManager.R'), local=TRUE)$value

Process = R6Class(
  "Process",
  inherit = ProcessManager,
  private = list(

  ),
  
  public = list(
    initialize = function(id){
        private$id <- id
      configProcess <- source(file.path('.', paste0('config_', id, '.R')), local=TRUE)$value

      lapply(names(configProcess$config), function(x){private$config[[x]] <- configProcess$config[[x]]})
    },

    
    GetConfig = function(config){
      print(config$steps)
    },
    
    Add = function(n){ as.numeric(n)+1}
  
    # server = function(){
    #   config = reactiveValues()
    #   moduleServer(private$id, function(input, output, session) {
    #     
    #     observeEvent(private$config,{
    #       lapply(names(private$config), function(x){config[[x]] <- private$config[[x]]})
    #     })
    #     
    #     
    #   observeEvent(config, { print("change ")})
    #   
    #   observeEvent(input$change, {config$steps[1] <- as.character(input$change)})
    #   
    #   observeEvent(config$steps, { self$GetConfig(config) })
    #   }
    #   )
    # }
    
  )
)