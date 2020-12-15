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
      for (i in 1:self$length)
        if (self$rv$status[i] != global$VALIDATED && 
            !is.null(self$GetMaxValidated_AllSteps()) && 
            self$GetMaxValidated_AllSteps() > i){
          self$rv$status[i] <- global$SKIPPED
        }
    },
    
    Set_All_Reset = function(){
      cat(paste0(class(self)[1], '::', 'Set_All_Reset() from - ', self$id, '\n'))
      browser()
      
      self$ResetScreens()
      self$rv$dataIn <- NULL
      self$rv$current.pos <- 1
      self$Initialize_Status_Process()
      self$Send_Result_to_Caller()
    },
    
    
    
    ValidateCurrentPos = function(){
      cat(paste0(class(self)[1], '::', 'ValidateCurrentPos() from - ', self$id, '\n'))

      self$rv$status[self$rv$current.pos] <- global$VALIDATED
      
      # Either the process has been validated, one can prepare data to be sent to caller
      # Or the module has been reseted
      if (self$rv$current.pos == self$length)
        self$Send_Result_to_Caller()
    },
    
    Status_Listener = function(){
      observeEvent(self$rv$status, ignoreInit = T, {
        cat(paste0(class(self)[1], '::observe((self$rv$status) from - ', self$id, '\n'))
        browser()
        self$Discover_Skipped_Steps()
        self$rv$isAllSkipped <- sum(rep(global$SKIPPED, self$length)==self$rv$status)==self$length
        self$rv$isAllUndone <- sum(rep(global$UNDONE, self$length)==self$rv$status)==self$length
        browser()
        # Disable all steps if all steps are skipped
        if (self$rv$isAllSkipped){
          self$ToggleState_Screens(FALSE, 1:self$length)
          self$ToggleState_ResetBtn(FALSE)
        }
        # Disable all steps if all steps are undone (such as after a reset)
        # Same action as for new dataIn() value
        if (self$rv$isAllUndone){
          self$ToggleState_Screens(TRUE, 1:self$length)
          self$ToggleState_ResetBtn(TRUE)
          
          # # Disable all screens after the first mandatory not validated
          # firstM <- self$GetFirstMandatoryNotValidated()
          # if (!is.null(firstM) && self$length > 1) {
          #   offset <- as.numeric(firstM != self$length)
          #   self$ToggleState_Screens(cond = FALSE, range = (firstM + offset):self$length)
          # }
        }
        
        # Disable all previous steps from each VALIDATED step
        # and enable all further steps 
        ind.max <- self$GetMaxValidated_AllSteps()
        if (!is.null(ind.max)){
          self$ToggleState_Screens(cond = FALSE, range = 1:ind.max)
          
          if (ind.max < self$length){
            # Enable all steps after the current one but the ones
            # after the first mandatory not validated
            firstM <- self$GetFirstMandatoryNotValidated((ind.max+1):self$length)
            if (is.null(firstM)){
              self$ToggleState_Screens(cond = TRUE, range = (1 + ind.max):(self$length))
            } else {
              self$ToggleState_Screens(cond = TRUE, range = (1 + ind.max):(ind.max + firstM))
              if (ind.max + firstM < self$length)
                self$ToggleState_Screens(cond = FALSE, range = (ind.max + firstM + 1):self$length)
            }
          }
        }
 
      })
    },
    
    EncapsulateScreens = function(){
      cat(paste0(class(self)[1], '::EncapsulateScreens() from - ', self$id, '\n'))
      lapply(1:self$length, function(i) {
        shinyjs::disabled(
          if (i==1)
            div(id = self$ns(self$config$steps[i]),
                class = paste0("page_", self$id),
                self$screens[[i]]
            )
          else
            shinyjs::hidden(
              div(id = self$ns(self$config$steps[i]),
                  class = paste0("page_", self$id),
                  self$screens[[i]]
              )
            )
        )
      }
      )
    },
    GetScreens_ui = function(){
      cat(paste0(class(self)[1], '::GetScreens() from - ', self$id, '\n'))
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
