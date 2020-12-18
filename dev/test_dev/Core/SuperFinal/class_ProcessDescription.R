#Timeline_R6.R
ProcessDescription = R6Class(
  "ProcessDescription",
  inherit = Process,
  private = list(
    .config = list(name = 'ProcessDescription',
                   steps = c('Description'),
                   mandatory = c(T)
    )
  ),
  
  public = list(
    
    Description_server = function(input, output){
      observeEvent(input$btn_validate_Description, ignoreInit = T, ignoreNULL=T, {
        cat(paste0(class(self)[1], "::observeEvent(input$btn_validate_Description from - ", self$id, '\n'))
        private$InitializeDataIn()
        private$ValidateCurrentPos()
      })
      
      output$datasetDescription <- renderUI({
        tagList(
          p(paste0('Dataset description: ', paste0(names(self$rv$temp.dataIn), collapse=", ")))
        )
      })
    },
    
    
    Description_ui = function(){

      wellPanel(
        tagList(
        actionButton(self$ns('btn_validate_Description'), 
                     paste0('Start ', self$config$name),
                     class = btn_success_color),
        includeMarkdown(paste0('./md/',self$config$name, ".md")),
        uiOutput(self$ns('datasetDescription'))
      )
      )
    }
  )
)