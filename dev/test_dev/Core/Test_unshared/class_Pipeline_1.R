#Timeline_R6.R
Pipeline1 = R6Class(
  "Pipeline1",
  inherit = Pipeline,
  private = list(
    .config = list(name = 'Pipeline',
                   steps = c('ProcessDescription', 'ProcessA'),
                   mandatory = setNames(c(T,F), c('ProcessDescription', 'ProcessA'))
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
      output$ProcessA <- renderUI({
        #req(self$ll.process[['ProcessA']])
        tagList(
          div(id=NS(self$id)('ProcessA'),
              self$ll.process[['ProcessA']]$ui()
          )
        )
      })
      
      output$ProcessDescription <- renderUI({
        #req(self$ll.process[['ProcessDescription']])
        tagList(
          div(id=NS(self$id)('ProcessDescription'),
              self$ll.process[['ProcessDescription']]$ui()
          )
        )
      })
    }
    
    
    
  )
)