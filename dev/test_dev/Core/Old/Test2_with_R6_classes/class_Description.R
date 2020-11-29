
#Timeline_R6.R
ProcessDescription = R6Class(
  "ProcessDescription",
  inherit = Process,
  private = list(
    config = reactiveValues(),
    Add_RenderUIs_Definitions = function( input, output){
      ns <- NS(private$id)
      output$Description <- renderUI({
        tagList(
          actionButton(ns('btn_validate_Description'), 
                       paste0('Start ', private$config$process.name),
                       class = btn_success_color),
          mod_insert_md_ui(ns(paste0(private$config$process.name, "_md")))
        )
      })
      
      observe({
        mod_insert_md_server(paste0(private$config$process.name, "_md"), 
                             paste0('./md/', private$config$process.name, '.md'))
      })
      
      observeEvent(input$btn_validate_Description, {
        private$InitializeDataIn()
        private$ValidateCurrentPos()
      })

    }
    
  ),
  
  public = list(
    initialize = function(id) {
      private$id <- id
      config <- list(steps = c('Description'),
                    mandatory = setNames(c(F), c('Description')),
                    status = setNames(c(0), c('Description')),
                    process.name = 'Description')
        private$SetConfig(config)
        private$InitStatus()
       observe({private$CreateTimeline()})
      }
  )
)



