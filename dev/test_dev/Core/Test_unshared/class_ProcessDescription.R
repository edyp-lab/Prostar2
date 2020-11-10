#Timeline_R6.R
ProcessDescription = R6Class(
  "ProcessDescription",
  inherit = Process,
  
  private = list(
    Add_RenderUIs_Definitions = function(input, output){
      ns <- NS(private$id)
      output$Description <- renderUI({
        tagList(
          actionButton(ns('btn_validate_Description'), 
                       paste0('Start ', private$config[[private$id]]$name),
                       class = btn_success_color),
          mod_insert_md_ui(ns(paste0(private$config[[private$id]]$name, "_md")))
        )
      })
      
      observe({
        mod_insert_md_server(paste0(private$config[[private$id]]$name, "_md"), 
                             paste0('./md/', private$config[[private$id]]$name, '.md'))
      })
      
      observeEvent(input$btn_validate_Description, {
        private$InitializeDataIn()
        private$ValidateCurrentPos()
      })
    }
    
  ),
  
  public = list(
    initialize = function(id) {
      print(paste0("----------in initialize of class Process with id = ", id))
      
      private$id <- id
      config <- list(process.name = 'Description',
                     steps = c('Description'),
                     mandatory = setNames(c(F), c('Description'))
      )
      private$steps <- config$steps
      private$length <- length(config$steps)
      lapply(names(config), function(x){private$config[[x]] <- config[[x]]})
      private$config$status <- setNames(rep(0, private$length), config$steps)
    }
    
  )
)