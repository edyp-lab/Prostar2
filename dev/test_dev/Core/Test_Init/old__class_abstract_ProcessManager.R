
    GetStringStatus = function(status){
      cat(paste0(class(self)[1], '::GetStringStatus() from - ', self$id, '\n'))
      if (status==self$global$VALIDATED) "Validated"
      else if (status==self$global$UNDONE) "Undone"
      else if (status==self$global$SKIPPED) 'Skipped'
    },
    
    
    
    
    
    
    
    
    
    
    # Test if a process module (identified by its name) has been skipped.
    # This function is called each time the list config$isDone is updated
    # because one can know the status 'Skipped' only when a further module
    # has been validated
    is.skipped = function(name){
      cat(paste0(class(self)[1], '::', 'is.skipped() from - ', self$id, '\n'))
      if(verbose=='skip') browser()
      pos <- which(name == self$config$steps)
      return(self$GetStatusPosition(pos) == self$global$SKIPPED)
    },
 },

    
    
    Unskip = function(pos){
      cat(paste0(class(self)[1], '::Unskip() from - ', self$id, '\n'))
      self$config$status[pos] <- self$global$UNDONE
    },
    
    GetStatusPosition = function(pos){
      cat(paste0(class(self)[1], '::GetStatusPosition() from - ', self$id, '\n'))
      self$config$status[pos]
    },

   
    
    
    

    

    
    Actions_On_New_DataIn = function(data){
      cat(paste0(class(self)[1], '::', 'Actions_On_New_DataIn() from - ', self$id, '\n'))
      if (verbose=='skip') browser()
      # This variable serves as a tampon while waiting the user click on the
      # validate button in the Description screen.
      # Once done, this variable is observed and the real rv$dataIn function can be
      # instanciated
      self$rv$temp.dataIn <- data
      
      
      # Test if input is NA or not
      inputExists <- !is.null(data)
      
      #Test if a dataset is already loaded
      tmpExists <- !is.null(self$rv$dataIn)
      
      
      if (tmpExists && inputExists){
        # this case is either the module is skipped or validated
        #self$rv$current.pos <- length(self$config$status)
        self$ActionsOn_Tmp_Input()
      } else if (tmpExists && !inputExists) {
        # The module has been reseted
        browser()
        self$ActionsOn_Tmp_NoInput()
      } else if (!tmpExists && inputExists){
        # The current position is pointed on a new module
        self$ActionsOn_NoTmp_Input()
      } else if (!tmpExists && !inputExists){
        # Initialization of Prostar
        self$ActionsOn_NoTmp_NoInput()
      }
    },
  
    
    
