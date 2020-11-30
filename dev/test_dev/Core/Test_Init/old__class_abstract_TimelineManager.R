
    Init_Default_Positions = function(){
      cat(paste0(class(self)[1], '::Init_Default_Positions() from - ', self$id, '\n'))
      self$default_pos <- list(VALIDATED = self$nbSteps,
                               SKIPPED = self$nbSteps,
                               UNDONE = 1
      )
    },

  
    
    Update_Cursor_position = function(){
      cat(paste0(class(self)[1], '::Update_Cursor_position() from - ', self$id, '\n'))
      if (verbose==T) browser()
      req(self$config$status)
      if (self$config$status[self$nbSteps] == self$global$VALIDATED)
        self$rv$current.pos <- self$default_pos$VALIDATED

    },
 
