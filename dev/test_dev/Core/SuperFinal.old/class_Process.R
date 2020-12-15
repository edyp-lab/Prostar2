Process = R6Class(
  "Process",
  inherit = ScreenManager,
  private = list(),
  
  public = list(
    modal_txt = "This action will reset this process. The input dataset will be the output of the last previous
                      validated process and all further datasets will be removed",

    
    ToggleState_Screens = function(cond, range){
      cat(paste0(class(self)[1], '::ToggleState_Steps() from - ', self$id, '\n'))
      #browser()
      lapply(range, function(x){
        shinyjs::toggleState(self$ns(self$config$steps[x]), condition = cond)
        #Send to TL the enabled/disabled tags
        self$rv$tl.tags.enabled[x] <- cond
      })
    },

    Set_All_Skipped = function(){
      cat(paste0(class(self)[1], '::', 'Set_All_Skipped() from - ', self$id, '\n'))
      self$rv$status <- setNames(rep(global$SKIPPED, self$length), self$config$steps)
    },
    
    
    Discover_Skipped_Steps = function(){
      cat(paste0(class(self)[1], '::Discover_Skipped_Status() from - ', self$id, '\n'))
      if(verbose=='skip') browser()
      for (i in 1:self$length)
        if (self$rv$status[i] != global$VALIDATED && self$GetMaxValidated_AllSteps() > i){
          self$rv$status[i] <- global$SKIPPED
        }
    },
    
    Set_All_Reset = function(){
      cat(paste0(class(self)[1], '::', 'Set_All_Reset() from - ', self$id, '\n'))
      #browser()
      
      self$ResetScreens()
      self$rv$dataIn <- NULL
      self$rv$current.pos <- 1
      self$Initialize_Status_Process()
      self$Send_Result_to_Caller()
    },
    
    
    
    ValidateCurrentPos = function(){
      cat(paste0(class(self)[1], '::', 'ValidateCurrentPos() from - ', self$id, '\n'))
      #if(verbose=='skip')
      # browser()
      self$rv$status[self$rv$current.pos] <- global$VALIDATED
      
      # Either the process has been validated, one can prepare data to be sent to caller
      # Or the module has been reseted
      if (self$rv$current.pos == self$length)
        self$Send_Result_to_Caller()
    },

    #---------------------------------------------------------------------
    GetScreens_ui = function(){
      cat(paste0(class(self)[1], '::GetScreens() from - ', self$id, '\n'))
      #browser()
      setNames(lapply(self$config$steps, function(x){
        eval(parse(text = paste0("self$", x, '_ui()')))
      }),
      self$config$steps)
    },

    GetScreens_listeners = function(){
      cat(paste0(class(self)[1], '::GetScreens() from - ', self$id, '\n'))
      setNames(lapply(self$config$steps, function(x){
        eval(parse(text = paste0("self$", x, '_listeners()')))
      }),
      self$config$steps)
    }
  )
)