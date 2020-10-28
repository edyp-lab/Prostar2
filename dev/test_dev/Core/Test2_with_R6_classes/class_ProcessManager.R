source(file.path('.', 'class_TimelineManager.R'), local=TRUE)$value

ProcessManager <- R6Class(
  "ProcessManager",
 
  
  InitScreens = function(n){
    setNames(lapply(1:n,
                    function(x){T}),
             paste0('screen', 1:n)
    )
  },
  
  server = function(dataIn, dataOut) {
                  ns <- NS(self$id)
                  
                  source(file.path('.', 'private_methods.R'), local=TRUE)$value
                 
                  
                  
                  # observe({
                   
                 #   self$Initialize_Status_Process()
                 #   self$config$screens <- InitScreens(self$length)
                    # Must be placed after the initialisation of the 'config$stepsNames' variable
                 #   self$config$screens <- setNames(
                 #     lapply(1:length(names(self$config$steps)), 
                 #            function(x){
                 #              do.call(uiOutput, list(outputId=names(self$config$steps)[x]))}),
                 #     paste0('screen_', names(self$config$steps)))
                 # })
                  
                  
                  
                  
                  
                    
                 
                   
                  ######
                  ###
                  ###
                  moduleServer(self$id, function(input, output, session) {
         
                    
                   
                           observeEvent(input$testWake,{wake(input$testWake)})


                  }
                  ) }
  )
)
