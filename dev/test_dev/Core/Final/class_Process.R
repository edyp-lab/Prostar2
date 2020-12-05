Process = R6Class(
  "Process",
  inherit = ProcessManager,
  private = list(),
  
  public = list(
    
    GetScreensDefinition = function(){
      cat(paste0(class(self)[1], '::GetScreensDefinition() from - ', self$id, '\n'))
      #browser()
      setNames(lapply(self$config$steps, function(x){
        eval(parse(text = paste0("self$", x, '()')))
      }),
      self$config$steps)
    },
    
    ActionOn_isSkipped = function(){
      cat(paste0(class(self)[1], '::', 'ActionsOnIsSkipped() from - ', self$id, '\n'))
      #if(verbose=='skip') 
      value <- if (self$rv$isSkipped) global$SKIPPED else global$UNDONE
      self$rv$status <- setNames(rep(value, self$length), self$config$steps)
    },
    
    
    Discover_Skipped_Status = function(){
      cat(paste0(class(self)[1], '::Discover_Skipped_Status() from - ', self$id, '\n'))
      if(verbose=='skip') browser()
      for (i in 1:self$length)
        if (self$rv$status[i] != global$VALIDATED && self$GetMaxValidated_AllSteps() > i){
          self$rv$status[i] <- global$SKIPPED
        }
    },
    
    CreateTimeline = function(){
      cat(paste0(class(self)[1], '::CreateTimeline() from - ', self$id, '\n'))
      self$timeline <- TimelineForProcess$new(self$ns('TL'), 
                                              config = self$config,
                                              screens = self$screens,
                                              style = style)
    }
  )
)