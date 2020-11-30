
    Init_Default_Positions = function(){
      cat(paste0(class(self)[1], '::Init_Default_Positions() from - ', self$id, '\n'))
      self$default_pos <- list(VALIDATED = self$nbSteps,
                               SKIPPED = self$nbSteps,
                               UNDONE = 1
      )
    },
    
    NextBtn_logics = function(){
      cat(paste0(class(self)[1], '::NextBtn_logics() from - ', self$id, '\n'))
      # Compute status for the Next button
      end_of_tl <- self$rv$current.pos == self$nbSteps
      #mandatory_step <- isTRUE(self$config$mandatory[self$rv$current.pos])
      validated <- self$config$status[self$rv$current.pos] == self$global$VALIDATED
      skipped <- self$config$status[self$rv$current.pos] == self$global$SKIPPED
      #NextBtn_logics <- !end_of_tl && (!mandatory_step || (mandatory_step && (validated || skipped)))
      NextBtn_logics <- !end_of_tl
      NextBtn_logics
    },
    
    PrevBtn_logics = function(){
      cat(paste0(class(self)[1], '::PrevBtn_logics() from - ', self$id, '\n'))
      # Compute status for the Previous button
      start_of_tl <- self$rv$current.pos == 1
      PrevBtn_logics <- !start_of_tl 
      PrevBtn_logics
    },
    
  
    
    Update_Cursor_position = function(){
      cat(paste0(class(self)[1], '::Update_Cursor_position() from - ', self$id, '\n'))
      if (verbose==T) browser()
      req(self$config$status)
      if (self$config$status[self$nbSteps] == self$global$VALIDATED)
        self$rv$current.pos <- self$default_pos$VALIDATED
      else if (self$rv$isAllSkipped)
        self$rv$current.pos <- self$default_pos$SKIPPED
     # else if (self$config$status[self$nbSteps] == self$global$UNDONE)
     #   self$rv$current.pos <- self$default_pos$UNDONE
    },
 
    
    

    
    
    
    
   
    
    
    GetMaxValidated_AllSteps = function(){
      cat(paste0(class(self)[1], '::', 'GetMaxValidated_AllSteps() from - ', self$id, '\n'))
      val <- 0
      ind <- which(self$config$status == self$global$VALIDATED)
      if (length(ind) > 0)
        val <- max(ind)
      val
    },
    
    # UI
    ui = function() {},
    
    SetModalTxt = function(txt){self$modal_txt <- txt},
    
    # SERVER
    server = function(config, dataLoaded) {
     
      observeEvent(dataLoaded(),ignoreNULL=F,{
        browser()
        self$ToggleState_Steps(dataLoaded(), 1:self$nbSteps)  
      })
      
      
      # MODULE SERVER
      moduleServer(self$id, function(input, output, session) {
        ns <- NS(self$id)
        
        observeEvent(input$rstBtn, {
          cat(paste0(class(self)[1], '::observeEvent(input$rstBtn) from - ', self$id, '\n'))
          showModal(dataModal())
          })
        
        ###############################
        
        
        
        #-------------------------------------------------------
        # Return the UI for a modal dialog with data selection input. If 'failed' is
        # TRUE, then display a message that the previous value was invalid.
        dataModal <- function() {
          modalDialog(
            span(self$modal_txt),
            footer = tagList(
              modalButton("Cancel"),
              actionButton(ns("modal_ok"), "OK")
            )
          )
        }
        
       
        
        # When OK button is pressed, update the reactive value which will be sent
        # to the caller
        observeEvent(req(c(input$modal_ok)), ignoreInit=T,{
          cat(paste0(class(self)[1], '::observeEvent(req(c(input$modal_ok))) from - ', self$id, '\n'))
          self$rv$reset_OK <- input$rstBtn
          self$rv$current.pos <- 1
          removeModal()
        })
        
        
        
        
        
        
        
        

          list(current.pos = reactive({self$rv$current.pos}),
               tl.reset = reactive({self$rv$reset_OK})
          )
      })
    }
  )
)