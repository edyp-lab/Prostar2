# InfoBox.R
InfoBox = R6Class(
  "InfoBox",
  public = list(
    # attributes
    id = NULL,
    
    # initializer
    initialize = function(id){
      self$id = id
    },
    
    # UI
    ui = function(){
      
      # the ns function here will prepend a prefix to all the ids in the app.
      ns = NS(self$id)
      
      tagList(
        
        # The id in each UI element must be wrapped in the ns function, in
        # order to be correctly recognized in the server function inside 
        # the module.
        textOutput(ns('text'))
      )
    },
    
    # server
    server = function(input, output, session, msg){
      output$text = renderText({ msg })
    },
    
    # call
    call = function(input, ouput, session, msg){
      callModule(self$server, self$id, msg)
    }
  )
)