#Timeline_R6.R
ProcessDescription = R6Class(
  "ProcessDescription",
  inherit = Process,
  
  private = list(
    .config = list(name = 'Description',
                   steps = c('Description'),
                   mandatory = c(T)
    )
  ),
  
  public = list(

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
        selectInput(ns('selectStep'), 'Test', choices=1:4),
        mod_insert_md_ui(ns(paste0(self$config$name, "_md")))
      )
      
    }
    
  )
)