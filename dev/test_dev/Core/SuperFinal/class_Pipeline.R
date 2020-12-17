Pipeline = R6Class(
  "Pipeline",
  inherit = ScreenManager,
  private = list(),
  
  public = list(
    tmp.return = "<reactiveValues>",
    
    modal_txt = "This action will reset this pipeline (and all the subsequent processes). The input dataset will be the output of the last previous
                      validated process and all further datasets will be removed.",
    
    
    ToggleState_Screens = function(cond, range){
      cat(paste0(class(self)[1], '::ToggleState_Steps() from - ', self$id, '\n'))
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
    
    
    Additional_Initialize_Class = function(){
      cat(paste0(class(self)[1], '::Additional_Initialize_Class() from - ', self$id, '\n'))
      
      self$rv$data2send <- NULL
      self$tmp.return <- reactiveValues()
      self$child.process <- setNames(lapply(self$config$steps,
                                            function(x){
                                              assign(x, get(x))$new(self$ns(x))
                                            }),
                                     self$config$steps
      )
           },

    Discover_Skipped_Steps = function(){
      cat(paste0(class(self)[1], '::Discover_Skipped_Steps() from - ', self$id, '\n'))

      for (i in 1:self$length)
        if (self$rv$status[i] != global$VALIDATED && self$GetMaxValidated_AllSteps() > i){
          self$rv$status[i] <- global$SKIPPED
          self$child.process[[i]]$Set_All_Skipped()
        }
    },
    
    
    Set_All_Reset = function(){
      cat(paste0(class(self)[1], '::', 'ActionsOnReset() from - ', self$id, '\n'))
      browser()
      
      self$BasicReset()
      
      # Say to all child processes to reset themselves
      lapply(self$config$steps, function(x){
          self$child.process[[x]]$Set_All_Reset()
        })
      
      self$Send_Result_to_Caller()
    },
    
    
    ValidateCurrentPos = function(){
      cat(paste0(class(self)[1], '::', 'ValidateCurrentPos() from - ', self$id, '\n'))
      
      self$rv$status[self$rv$current.pos] <- global$VALIDATED
      self$Send_Result_to_Caller()
    },
    
    Additional_Server_Funcs = function(){
      cat(paste0(class(self)[1], '::Additional_Server_Funcs() from - ', self$id, '\n'))
      self$Launch_Module_Server()
    },

    ActionOn_NewPosition = function(){
      cat(paste0(class(self)[1], '::ActionOn_NewPosition() from - ', self$id, '\n'))
      
      # Send dataset to child process only if the current position is enabled
      if(self$rv$tl.tags.enabled[self$rv$current.pos])
        self$PrepareData2Send()
      
      # If the current step is validated, set the child current position to the last step
      if (self$rv$status[self$rv$current.pos] == global$VALIDATED)
        self$child.process[[self$rv$current.pos]]$Change_Current_Pos(self$child.process[[self$rv$current.pos]]$length)
    },
    
    EncapsulateScreens = function(){
      cat(paste0(class(self)[1], '::EncapsulateScreens() from - ', self$id, '\n'))
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
    
   
    
    GetScreens_ui = function(){
      cat(paste0(class(self)[1], '::', 'GetScreens() from - ', self$id, '\n'))
      
      setNames(lapply(self$config$steps, function(x){
        self$child.process[[x]]$ui()
      }),
      self$config$steps)
      },

    ActionOn_New_DataIn = function(){
      self$PrepareData2Send()
    },
    
    # This function calls the server part of each module composing the pipeline
    Launch_Module_Server = function(){
      cat(paste0(class(self)[1], '::', 'Launch_Module_Server() from - ', self$id, '\n'))
      
      lapply(self$config$steps, function(x){
        self$tmp.return[[x]] <- self$child.process[[x]]$server(
          dataIn = reactive({ self$rv$data2send[[x]] })
          )
      })
      
     # self$PrepareData2Send()
      
      # Catch the returned values of the process                                                           
      observeEvent(lapply(names(self$child.process), function(x){self$tmp.return[[x]]()$trigger}), {
        cat(paste0(class(self)[1], '::', 'observeEvent(trigger) from - ', self$id, '\n'))
        #browser()
        self$ActionOn_Data_Trigger()
      })

    },
    
    # Catch the return value of a module and update the list of isDone modules
    # This list is updated with the names of datasets present in the rv$tmp
    # variable. One set to TRUE all the elements in isDone which have a corresponding
    # element in names(rv$tmp).
    # One cannot simply set to TRUE the last element of rv$tmp because it will does
    # not work in case of a reseted module (it is not in the names(rv$tmp) list
    # anymore)
    # If a value (not NULL) is received, then it corresponds to the module
    # pointed by the current position
    # This function also updates the list isDone
    ActionOn_Data_Trigger = function(){
      cat(paste0(class(self)[1], '::', 'ActionOn_Data_Trigger from - ', self$id, '\n'))
      #browser()
      processHasChanged <- newValue <- NULL
      
      return.trigger.values <- setNames(lapply(names(self$child.process), function(x){self$tmp.return[[x]]()$trigger}),
                                names(self$child.process))
                                
      toto <- unlist(return.trigger.values)
      if (sum(toto) != 0){
        processHasChanged <- names(self$child.process)[which(max(toto)==toto)]
        newValue <- self$child.process[[processHasChanged]]$Get_Result()
      }
      
      if (is.null(newValue)){
        # process has been reseted
        self$rv$status[processHasChanged] <- global$UNDONE
        # browser()
        # One take the last dataset not NULL
        last.validated <- self$GetMaxValidated_BeforeCurrentPos()
        
        #There is no validated step (the first step has been reseted)
        if(is.null(last.validated))
          self$rv$dataIn <- NULL
        else
          self$rv$dataIn <- self$rv$dataIn[ , , 1:(self$original.offset + last.validated)]
      }
      else{
        # process has been validated
        self$rv$status[processHasChanged] <- global$VALIDATED
        self$Discover_Skipped_Steps()
        self$rv$dataIn <- newValue
      }
      
      self$Send_Result_to_Caller()
      
    },
    
    GetMaxValidated_BeforeCurrentPos = function(){
      cat(paste0(class(self)[1], '::', 'GetMaxValidated_BeforeCurrentPos() from - ', self$id, '\n'))
      ind.max <- NULL
      indices.validated <- which(self$rv$status == global$VALIDATED)
      if (length(indices.validated) > 0){
        ind <- which(indices.validated < self$rv$current.pos)
        if(length(ind) > 0)
          ind.max <- max(ind)
      }
      ind.max
    },
    
    PrepareData2Send = function(){
      cat(paste0(class(self)[1], '::', 'PrepareData2Send() from - ', self$id, '\n'))
     # browser()
      # Returns NULL to all modules except the one pointed by the current position
      # Initialization of the pipeline : one send dataIn() to the
      # original module
     #browser()

        update <- function(name){
        data <- NULL
        if (name == self$currentStepName()){
          # One treat the dataset for the current position
          ind.last.validated <- self$GetMaxValidated_BeforeCurrentPos()
          if (is.null(ind.last.validated)){
            data <- self$rv$temp.dataIn
          } else {
            data <- self$rv$dataIn[ , , 1:(self$original.offset + ind.last.validated)]
          }
        }
        return(data)
      }
      
        self$rv$data2send <- setNames(
          lapply(names(self$child.process), function(x){NULL}),
          names(self$child.process))
        
        #browser()
        self$rv$data2send <- setNames(
          lapply(names(self$child.process), function(x){update(x)}),
          names(self$child.process))
      
      print("--- data2 send ---")
      print(self$rv$data2send)
      


    }
    
  )
)