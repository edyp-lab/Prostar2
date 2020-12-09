
ProcessOriginal = R6Class(
  "ProcessOriginal",
  inherit = Process,
  private = list(
    .config = list(name = 'ProcessOriginal',
                   steps = c('Description'),
                   mandatory = c(T)
    )
  ),
  
  public = list(
    
    Description = function(){
      
      observe({
        mod_insert_md_server(paste0(self$name, "_md"), 
                             paste0('./md/', self$name, '.md'))
      })
      
      observeEvent(self$input$btn_validate_Description, {
        self$InitializeDataIn()
        self$ValidateCurrentPos()
      })
      
      tagList(
        actionButton(self$ns('btn_validate_Description'), 
                     paste0('Start ', self$name),
                     class = btn_success_color),
        mod_insert_md_ui(self$ns(paste0(self$name, "_md")))
      )
    }
    
  )
)