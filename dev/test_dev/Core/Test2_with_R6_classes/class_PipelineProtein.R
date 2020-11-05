
#Timeline_R6.R
PipelineProtein = R6Class(
  "PipelineProtein",
  inherit = Pipeline,
  private = list(
    config = reactiveValues(),
    
    # This function creates the UI parts of the screens (dynamically set 
    # renderUIs). 
    # In the process child class, this method corresponds to the logics function
    Add_RenderUIs_Definitions = function(input, output){
      output$Filtering <- renderUI(
        tagList(
          div(id=NS(private$id)('Filtering'),
              private$logics[['Filtering']]$ui(),
          )
        )
      )
      
      output$Description <- renderUI(
        tagList(
          div(id=NS(private$id)('Description'),
              private$logics[['Description']]$ui(),
          )
        )
      )
      
    }
    
  ),
  
  public = list(
    initialize = function(id) {
      private$id <- id
      
      observe({
        tmp <- list(steps = c('Description', 'Filtering'),
                    mandatory = setNames(c(F, F), c('Description', 'Filtering')),
                    status = setNames(c(0, 0), c('Description', 'Filtering')),
                    process.name = 'Pipeline')
        lapply(names(tmp), function(x){private$config[[x]] <- tmp[[x]]})
        
        #  private$config$steps = c('Description', 'Step1', 'Step2', 'Step3')
        # private$config$mandatory = setNames(c(F,F,F,F), c('Description', 'Step1', 'Step2', 'Step3'))
        # private$config$status = setNames(c(0, 0, 0, 0), c('Description', 'Step1', 'Step2', 'Step3'))
        # private$config$process.name <- 'Filtering' 
        
        private$steps <-  private$config$steps
        private$length <- length(private$config$steps)
        

        private$config$status <- setNames(rep(0, private$length), private$config$steps)
        
        private$CreateTimeline()
        
        private$logics[['Description']] <- ProcessDescription$new(NS(private$id)('ProcessDescription'))
        private$logics[['Filtering']] <- ProcessFiltering$new(NS(private$id)('ProcessFiltering'))
      })
      
    }
    
  )
)



