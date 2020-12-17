
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
