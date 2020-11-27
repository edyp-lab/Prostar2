#Timeline_R6.R
Process = R6Class(
  "Process",
  inherit = ProcessManager,
  private = list(),
  
  public = list(

    ActionsOn_NoTmp_Input = function(){
      cat(paste0(class(self)[1], '::', 'ActionsOn_NoTmp_Input() from - ', self$id, '\n'))
      self$config$screens <- self$GetScreensDefinition()
      
      self$InitializeModule()
      self$InitializeTimeline()
    },
    
    
    GetScreensDefinition = function(){
      #browser()
      setNames(lapply(self$config$steps, function(x){
        eval(parse(text = paste0("self$", x, '()')))
      }),
      self$config$steps)
    },
    
    
    ActionsOnIsSkipped = function(){
      cat(paste0(class(self)[1], '::', 'ActionsOnIsSkipped() from - ', self$id, '\n'))
      if(verbose=='skip') browser()
      
      self$config$status <- setNames(rep(self$global$SKIPPED, self$length),
                                     self$config$steps)
      if(verbose=='skip') browser()
    },
    
    CreateTimeline = function(){
      cat(paste0(class(self)[1], '::', 'CreateTimeline() from - ', self$id, '\n'))
      self$timeline <- TimelineForProcess$new(
        id = NS(self$id)('timeline'),
        mandatory = self$config$mandatory
      )
    }

  )
)