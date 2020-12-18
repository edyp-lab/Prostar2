#' Add together two numbers
#'
#' @param x A number
#' @param y A number
#' @return The sum of \code{x} and \code{y}
#' 
#' @import R6
#' 
#' @export
#' 
#' @examples
#' add(1, 1)
Pipeline = R6Class(
  "Pipeline",
  inherit = ScreenManager,
  private = list(
    
    #' @field A text which appears in the popup modal when the reset button is clicked.
    modal_txt = "This action will reset this pipeline (and all the subsequent processes). The input dataset will be the output of the last previous
                      validated process and all further datasets will be removed.",
    
    
    #' @description
    #' Contains specific functions that must be integrated in the
    #' initialize function of the parent class. Using this tip avoids writing
    #' the full `initialize()` method which most part is common for all children
    #' classes.
    #' 
    #' @return Nothing.
    #' 
    Additional_Initialize_Class = function(){
      if(verbose) cat(paste0(class(self)[1], '::Additional_Initialize_Class() from - ', self$id, '\n\n'))
      
      self$rv$data2send <- NULL
      self$tmp.return <- reactiveValues()
      self$child.process <- setNames(lapply(self$config$steps,
                                            function(x){
                                              assign(x, get(x))$new(self$ns(x))
                                            }),
                                     self$config$steps
      )
    },
    
    #' @description
    #' Functions that are executed when a new dataset is received.
    #'
    #' @return Nothing.
    #' 
    ActionOn_New_DataIn = function(){
      if(verbose) cat(paste0(class(self)[1], '::', 'ActionOn_New_DataIn() from - ', self$id, '\n\n'))
      self$PrepareData2Send()
    },
    
    #' @description
    #' Contains specific functions that must be integrated in the
    #' `server()` function of the parent class. Using this tip avoids rewriting
    #' the full `server()` method which most part is common for all children
    #' classes.
    #'
    #' @return Nothing.
    #' 
    Additional_Server_Funcs = function(){
      if(verbose) cat(paste0(class(self)[1], '::Additional_Server_Funcs() from - ', self$id, '\n\n'))
      self$Launch_Module_Server()
    },
    
    #' @description
    #' Actions run when a step (from either a pipeline or a process) is validated.
    #' 
    #' @return Nothing.
    #' 
    ValidateCurrentPos = function(){
      if(verbose) cat(paste0(class(self)[1], '::', 'ValidateCurrentPos() from - ', self$id, '\n\n'))
      
      self$rv$status[self$rv$current.pos] <- global$VALIDATED
      private$Send_Result_to_Caller()
    },
    
    #' @description
    #' Encapsulates each screen definition in a <div> with the namespace of the 
    #' current instance to be able to show/hide the entire screen when needed.
    #' 
    #' @return Nothing.
    #'
    EncapsulateScreens = function(){
      if(verbose) cat(paste0(class(self)[1], '::EncapsulateScreens() from - ', self$id, '\n\n'))
      lapply(1:self$length, function(i) {
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
      }
      )
    },
    
    #' @description
    #' Functions run when a new position is observed in the timeline (the current screen
    #' has changed).
    #' 
    #' @return Nothing.
    #'
    ActionOn_NewPosition = function(){
      if(verbose) cat(paste0(class(self)[1], '::ActionOn_NewPosition() from - ', self$id, '\n\n'))
      
      # Send dataset to child process only if the current position is enabled
      if(self$rv$tl.tags.enabled[self$rv$current.pos])
        self$PrepareData2Send()
      #browser()
      # If the current step is validated, set the child current position to the last step
      if (self$rv$status[self$rv$current.pos] == global$VALIDATED)
        self$child.process[[self$rv$current.pos]]$Change_Current_Pos(self$child.process[[self$rv$current.pos]]$length)
    },
    
    
    #' @description
    #' After a change in the status vector, checks if there are new skipped steps.
    #' 
    #' @return Nothing.
    #'
    Discover_Skipped_Steps = function(){
      if(verbose) cat(paste0(class(self)[1], '::Discover_Skipped_Steps() from - ', self$id, '\n\n'))
      
      for (i in 1:self$length){
        max.val <- private$GetMaxValidated_AllSteps()
        if (self$rv$status[i] != global$VALIDATED && max.val > i){
          self$rv$status[i] <- global$SKIPPED
          self$child.process[[i]]$Set_All_Skipped()
        }
      }
    }
  ),
  
  public = list(
    
    #' @field xxx
    tmp.return = "<reactiveValues>",
    
    #' @description
    #' 
    #' @param cond xxx
    #' 
    #' @param range xxx
    #' 
    #' @return Nothing.
    #'
    ToggleState_Screens = function(cond, range){
      if(verbose) cat(paste0(class(self)[1], '::ToggleState_Steps() from - ', self$id, '\n\n'))
      #browser()
      
      #Send to local TL the enabled/disabled tags
      lapply(range, function(x){
        cond <- cond && !(self$rv$status[x] == global$SKIPPED)
        self$rv$tl.tags.enabled[x] <- cond
      })
      
      # Send to the child processes specified by 'range' what to do with their screens
      lapply(range, function(x){
        name <- self$config$steps[x]
        child.length <- self$child.process[[name]]$length
        self$child.process[[name]]$ToggleState_Screens(cond, 1:child.length)
        #Send to TL the enabled/disabled tags
        self$rv$tl.tags.enabled[x] <- cond
        })
        
      # shinyjs::toggleState(paste0(self$ns(self$config$steps[1]), '-TL_LeftSide'), T)
      # shinyjs::toggleState(paste0(self$ns(self$config$steps[1]), '-TL_RightSide'), T)
      # shinyjs::toggleState(paste0(self$ns(self$config$steps[1]), '-Screens'), T)
    },

    
    #' @description
    #' xxx
    #' 
    #' @return Nothing.
    #'
    Set_All_Reset = function(){
      if(verbose) cat(paste0(class(self)[1], '::', 'ActionsOnReset() from - ', self$id, '\n\n'))
      #browser()
      
      private$BasicReset()
      
      # Say to all child processes to reset themselves
      lapply(self$config$steps, function(x){
          self$child.process[[x]]$Set_All_Reset()
        })
    },
    
    #' @description
    #' xxx
    #' 
    #' @return Nothing.
    #'
    GetScreens_ui = function(){
      if(verbose) cat(paste0(class(self)[1], '::', 'GetScreens() from - ', self$id, '\n\n'))
      
      setNames(lapply(self$config$steps, function(x){
        self$child.process[[x]]$ui()
      }),
      self$config$steps)
      },

    
    #' @description
    #' This function calls the server part of each module composing the pipeline
    #' 
    #' @return Nothing.
    #'
    Launch_Module_Server = function(){
      if(verbose) cat(paste0(class(self)[1], '::', 'Launch_Module_Server() from - ', self$id, '\n\n'))
      
      lapply(self$config$steps, function(x){
        self$tmp.return[[x]] <- self$child.process[[x]]$server(
          dataIn = reactive({ self$rv$data2send[[x]] })
          )
      })
      
     # self$PrepareData2Send()
      
      # Catch the returned values of the process                                                           
      observeEvent(lapply(names(self$child.process), function(x){self$tmp.return[[x]]()$trigger}), ignoreInit=T,{
        if(verbose) cat(paste0(class(self)[1], '::', 'observeEvent(trigger) from - ', self$id, '\n\n'))
        #browser()
        self$ActionOn_Data_Trigger()
      })

    },
    

    #' @description
    #' Catch the return value of a module and update the list of isDone modules
    #' This list is updated with the names of datasets present in the rv$tmp
    #' variable. One set to TRUE all the elements in isDone which have a corresponding
    #' element in names(rv$tmp).
    #' One cannot simply set to TRUE the last element of rv$tmp because it will does
    #' not work in case of a reseted module (it is not in the names(rv$tmp) list
    #' anymore)
    #' If a value (not NULL) is received, then it corresponds to the module
    #' pointed by the current position
    #' This function also updates the list isDone
    #' This function updates the current dataset (self$rv$dataIn)
    #' 
    #' @return Nothing.
    #'
    ActionOn_Data_Trigger = function(){
      if(verbose) cat(paste0(class(self)[1], '::', 'ActionOn_Data_Trigger from - ', self$id, '\n\n'))
      #browser()
      processHasChanged <- newValue <- NULL
      
      return.trigger.values <- setNames(lapply(names(self$child.process), function(x){self$tmp.return[[x]]()$trigger}),
                                names(self$child.process))
                                
      triggerValues <- unlist(return.trigger.values)
      if (sum(triggerValues)==0){ # Init of core engine
        
      } else {
        processHasChanged <- self$config$steps[which(max(triggerValues)==triggerValues)]
        ind.processHasChanged <- which(self$config$steps==processHasChanged)
        newValue <- self$child.process[[processHasChanged]]$Get_Result()
      
      
      if (is.null(newValue)){ # process has been reseted
        self$rv$status[ind.processHasChanged:self$length] <- global$UNDONE
        # browser()
        # One take the last validated step (before the one corresponding to processHasChanges
        # but it is straightforward because we juste updates self$rv$status
        ind.last.validated <- NULL
        validated.steps <- which(self$rv$status == global$VALIDATED)
        if (length(validated.steps) !=0)
          ind.last.validated <- max(validated.steps)
        
        #There is no validated step (the first step has been reseted)
        if(is.null(ind.last.validated) || ind.last.validated == 1)
          self$rv$dataIn <- self$rv$temp.dataIn
        else{
          name.last.validated <- self$config$steps[ind.last.validated]
          dataIn.ind.last.validated <- which(names(self$rv$dataIn) == name.last.validated)
          self$rv$dataIn <- self$rv$dataIn[ , , 1:dataIn.ind.last.validated]
        }
        
        # In this case, one force the update of the input dataset
        self$PrepareData2Send()
      } else {
        # process has been validated
        self$rv$status[processHasChanged] <- global$VALIDATED
        if (ind.processHasChanged < self$length)
          self$rv$status[(ind.processHasChanged+1):self$length] <- global$UNDONE
        
        private$Discover_Skipped_Steps()
        self$rv$dataIn <- newValue
      }
        private$Send_Result_to_Caller()
      }
     
    },
    
    #' @description 
    #' xxx
    #' 
    #' @return Nothing.
    #' 
    GetMaxValidated_BeforeCurrentPos = function(){
      if(verbose) cat(paste0(class(self)[1], '::', 'GetMaxValidated_BeforeCurrentPos() from - ', self$id, '\n\n'))
      ind.max <- NULL
      indices.validated <- which(self$rv$status == global$VALIDATED)
      if (length(indices.validated) > 0){
        ind <- which(indices.validated < self$rv$current.pos)
        if(length(ind) > 0)
          ind.max <- max(ind)
      }
      ind.max
    },
    
    #' @description 
    #' 
    #' @param pos xxx
    #' 
    #' @return Nothing.
    #'
    GetMaxValidated_BeforePos = function(pos){
      if(verbose) cat(paste0(class(self)[1], '::', 'GetMaxValidated_BeforeCurrentPos() from - ', self$id, '\n\n'))
      ind.max <- NULL
      indices.validated <- which(self$rv$status == global$VALIDATED)
      if (length(indices.validated) > 0){
        ind <- which(indices.validated < pos)
        if(length(ind) > 0)
          ind.max <- max(ind)
      }
      ind.max
    },
    
    #' @description 
    #' Returns NULL to all modules except the one pointed by the current position
    #' Initialization of the pipeline : one send dataIn() to the
    #' original module
    #' 
    #' @return Nothing.
    #'
    PrepareData2Send = function(){
      if(verbose) cat(paste0(class(self)[1], '::', 'PrepareData2Send() from - ', self$id, '\n\n'))
     # browser()

        update <- function(name){
        data <- NULL
        if (name == self$currentStepName()){
          # One treat the dataset for the current position
          #ind.last.validated <- self$GetMaxValidated_BeforeCurrentPos()
          name.last.validated <- names(self$rv$dataIn)[length(self$rv$dataIn)]
          ind.last.validated <- which(names(self$rv$dataIn)== name.last.validated)
          
          if (is.null(ind.last.validated)){
            data <- self$rv$temp.dataIn
          } else {
            data <- self$rv$dataIn[ , , 1:ind.last.validated]
          }
        }
        return(data)
      }
      
        self$rv$data2send <- setNames(
          lapply(names(self$child.process), function(x){NULL}),
          names(self$child.process))
        
        if (is.null(self$rv$dataIn)) # Init of core engine
          self$rv$data2send[[1]] <- self$rv$temp.dataIn
        else
          self$rv$data2send <- setNames(
            lapply(names(self$child.process), function(x){update(x)}),
          names(self$child.process))
      
      print("--- data2 send ---")
      print(self$rv$data2send)
    }
    
  )
)