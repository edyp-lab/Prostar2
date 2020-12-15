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
    
    Description = function(){
      
      observeEvent(self$input$btn_validate_Description, ignoreInit = T, {
        cat(paste0(class(self)[1], '::observeEvent(self$input$btn_validate_Description from - ', self$id, '\n'))
        self$InitializeDataIn()
        self$ValidateCurrentPos()
      })
      
      
      tagList(
        actionButton(self$ns('btn_validate_Description'), 
                     paste0('Start ', self$config$name),
                     class = btn_success_color),
        includeMarkdown(paste0('./md/',self$config$name, ".md"))
      )
    }
    
  )
)