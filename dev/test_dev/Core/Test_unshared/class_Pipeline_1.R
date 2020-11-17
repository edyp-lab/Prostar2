#Timeline_R6.R
Pipeline1 = R6Class(
  "Pipeline1",
  inherit = Process,
  private = list(
    .config = list(process.name = 'Pipeline',
                   steps = c('process_Description', 'process_A'),
                   mandatory = setNames(c(T,F), c('process_Description', 'process_A'))
    )
  ),
  
  public = list(
    ll.process = list(
      ProcessDescription = NULL,
      ProcessA = NULL
    ),
    
    
    Add_RenderUIs_Definitions = function(input, output){
      cat(paste0(class(self)[1], '::', 'Add_RenderUIs_Definitions()\n'))
      ns <- NS(self$id)
      
      print("In class_Pipeline::Add_RenderUIs_Definitions()")
      output$process_A <- renderUI(
        tagList(
          div(id=NS(self$id)('processA'),
              self$ll.process[['processA']]$ui(),
          )
        )
      )
      
      output$process_Description <- renderUI(
        tagList(
          div(id=NS(self$id)('processDescription'),
              self$ll.process[['processDescription']]$ui(),
          )
        )
      )
    }
    
    
    
  )
)