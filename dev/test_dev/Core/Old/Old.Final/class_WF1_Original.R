WF1_Original = R6Class(
  "WF1_Original",
  inherit = Process,
  private = list(
    .config = list(name = 'Original',
                   steps = c('Description'),
                   mandatory = c(T)
    )
  ),
  
  public = list(
    Description = function(){
      mod_insert_md_server(paste0(self$config$name, "_md"), 
                           paste0('./md/', self$config$name, '.md'))
      
      observeEvent(self$input$btn_validate_Description, {
        self$InitializeDataIn()
        self$ValidateCurrentPos()
      })
      
      tagList(
        actionButton(self$ns('btn_validate_Description'), 
                     paste0('Start ', self$config$name),
                     class = btn_success_color),
        mod_insert_md_ui(self$ns(paste0(self$config$name, "_md")))
      )
    }
    
  )
)







