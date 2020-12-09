Process = R6Class(
  "Process",
  inherit = ProcessManager,
  private = list(),
  
  public = list(
    modal_txt = "This action will reset this process. The input dataset will be the output of the last previous
                      validated process and all further datasets will be removed",
    
    ui = function(){
      color <- "blue"
      fluidPage(
        shinyjs::useShinyjs(),
        wellPanel(
          style=paste0("background: white; border-width: 2px; border-color: ", color, ";"),
          self$Main_UI()
        )
      )
    },
    
    EncapsulateScreens = function(){
      req(self$screens)
      cat(paste0(class(self)[1], '::GetScreens() from - ', self$id, '\n'))
      lapply(1:self$length, function(i) {
        if (i==1)
          shinyjs::disabled(
            div(
              class = paste0("page_", self$id),
              id = self$ns(self$config$steps[i]),
              self$screens[[i]]
            )
          )
        else
          shinyjs::disabled(
            shinyjs::hidden(
              div(
                class = paste0("page_", self$id),
                id = self$ns(self$config$steps[i]),
                self$screens[[i]])
            )
          )
      }
      )
    },
    
    Force_ToggleState_Screens = function(){
      cat(paste0(class(self)[1], '::Force_ToggleState_Steps() from - ', self$id, '\n'))
      #if (verbose==T) 
      # browser()
      
      # self$ToggleState_Screens(cond = TRUE, range = 1:self$length)
      # 
      # if (self$rv$isAllSkipped){
      #   # Disable all steps if all steps are skipped
      #   self$ToggleState_Screens(cond = FALSE, range = 1:self$length)
      # }
      # 
      # firstM <- self$GetFirstMandatoryNotValidated()
      # if (!is.null(firstM) && self$length > 1) {
      #   offset <- as.numeric(firstM != self$length)
      #   # Disable all further screens
      #   self$ToggleState_Screens(cond = FALSE, range = (firstM + offset):self$length)
      #   }
      # 
      # # Disable all previous steps from each VALIDATED step
      # # and enable all further steps (in case of current.pos is mandatory)
      # ind.max <- self$GetMaxValidated_AllSteps()
      # if (!is.null(ind.max)){
      #   self$ToggleState_Screens(cond = FALSE, range = 1:ind.max)
      #   if (ind.max < self$length){
      #         offset <- 1
      #         self$ToggleState_Screens(cond = TRUE, range = (offset + ind.max):self$length)
      #       }
      #     }
    },
    
    
    
    #---------------------------------------------------------------------
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
    }
  )
)