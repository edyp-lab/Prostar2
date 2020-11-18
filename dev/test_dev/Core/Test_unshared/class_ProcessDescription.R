#Timeline_R6.R
ProcessDescription = R6Class(
  "ProcessDescription",
  inherit = Process,
  
  private = list(
    .config = list(name = 'Description',
                   type = 'process',
                   steps = c('Description'),
                   mandatory = setNames(c(T), c('Description'))
    )
  ),
  
  public = list(

    Add_RenderUIs_Definitions = function(input, output){
      ns <- NS(self$id)
      output$Description <- renderUI({
        tagList(
          actionButton(ns('btn_validate_Description'), 
                       paste0('Start ', self$config$name),
                       class = btn_success_color),
          mod_insert_md_ui(ns(paste0(self$config$name, "_md")))
        )
      })
      
      observe({
        mod_insert_md_server(paste0(self$config$name, "_md"), 
                             paste0('./md/', self$config$name, '.md'))
      })
      
      observeEvent(input$btn_validate_Description, {
        self$InitializeDataIn()
        self$ValidateCurrentPos()
      })
    }
    
  )
)