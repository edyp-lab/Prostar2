Pipeline = R6Class(
  "Pipeline",
  inherit = ProcessManager,
  private = list(),
  
  public = list(
    tmp.return = "<reactiveValues>",
    
    
    #---------------------------------------------------------------------------------
    
    
    EncapsulateScreens = function(){
      req(self$screens)
      cat(paste0(class(self)[1], '::GetScreens() from - ', self$id, '\n'))
      lapply(1:self$length, function(i) {
        if (i==1)
          div(
            class = paste0("page_", self$id),
            id = self$ns(self$config$steps[i]),
            self$screens[[i]]
          )
        else
          shinyjs::hidden(
            div(
              class = paste0("page_", self$id),
              id = self$ns(self$config$steps[i]),
              self$screens[[i]])
          )
      }
      )
    },
    
    Force_ToggleState_Screens = function(){
      cat(paste0(class(self)[1], '::Force_ToggleState_Steps() from - ', self$id, '\n'))
      #if (verbose==T) 
      #browser()
      
      if (self$rv$isAllSkipped){
        # Disable all steps if all steps are skipped
        self$ToggleState_Screens(cond = FALSE, range = 1:self$length)
      }
      
      firstM <- self$GetFirstMandatoryNotValidated()
      if (!is.null(firstM) && self$length > 1) {
        offset <- as.numeric(firstM != self$length)
        # Disable all further screens
        self$ToggleState_Screens(cond = FALSE, range = (firstM + offset):self$length)
      }
      
      # Disable all previous steps from each VALIDATED step
      # and enable all further steps (in case of current.pos is mandatory)
      ind.max <- self$GetMaxValidated_AllSteps()
      if (!is.null(ind.max)){
        #self$ToggleState_Screens(cond = FALSE, range = 1:ind.max)
        if (ind.max < self$length){
          offset <- 1
          self$ToggleState_Screens(cond = TRUE, range = (offset + ind.max):self$length)
        }
      }
    },
    #---------------------------------------------------------------------------------
    Additional_Initialize_Class = function(){
      cat(paste0(class(self)[1], '::Additional_Initialize_Class() from - ', self$id, '\n'))
      
      self$child.process <- setNames(lapply(self$config$steps, function(x){x <- NULL}),
                                     self$config$steps)
      self$rv$data2send <- NULL
      self$tmp.return <- reactiveValues()
           },
    
    Discover_Skipped_Status = function(){
      cat(paste0(class(self)[1], '::Discover_Skipped_Status() from - ', self$id, '\n'))
      if(verbose=='skip') browser()
      for (i in 1:self$length)
        if (self$rv$status[i] != global$VALIDATED && self$GetMaxValidated_AllSteps() > i){
          self$rv$status[i] <- global$SKIPPED
          self$child.process[[i]]$SetSkipped(global$SKIPPED)
        }
    },
    
    
    Additional_Server_Funcs = function(){
      self$Launch_Module_Server()
    },
    
    ActionOn_NewPosition = function(){
      cat(paste0(class(self)[1], '::ActionOn_NewPosition() from - ', self$id, '\n'))
      self$PrepareData2Send()
      },
    
    CreateTimeline = function(){
      cat(paste0(class(self)[1], '::', 'CreateTimeline() from - ', self$id, '\n'))
      self$timeline <- TimelineForPipeline$new(self$ns('TL'), 
                                               config = self$config,
                                               screens = self$screens,
                                               style = style
                                               )
    },

    GetScreensDefinition = function(){
      cat(paste0(class(self)[1], '::', 'GetScreensDefinition() from - ', self$id, '\n'))
      req(self$child.process)
      #browser()
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
      #browser()
      self$child.process <- setNames(lapply(self$config$steps,
                                         function(x){
                                           assign(x, get(x))$new(self$ns(x))
                                         }),
                                  self$config$steps
      )

      #browser()
      self$PrepareData2Send()
      
      
      lapply(self$config$steps, function(x){
        self$tmp.return[[x]] <- self$child.process[[x]]$ToggleState_Screens(F, 1, prefix=paste0(self$id, '-TL-'))
      })
      
      
      lapply(self$config$steps, function(x){
        self$tmp.return[[x]] <- self$child.process[[x]]$server(
          dataIn = reactive({ self$rv$data2send[[x]] })
          )
      })
      
      
      #browser()                                                                      
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
      # browser()
      # cat("----- self$old.tmp.return ------\n")
      # print(setNames(lapply(names(self$child.process), function(x){self$old.tmp.return[[x]]}),
      #                names(self$child.process))
      # )
      # cat("----- self$tmp.return ------\n")
      # print(setNames(lapply(names(self$child.process), function(x){self$tmp.return[[x]]()$value}),
      #                names(self$child.process))
      # )
      #browser()
      processHasChanged <- newValue <- NULL
      
      toto <- unlist(lapply(names(self$child.process), function(x){self$tmp.return[[x]]()$trigger}))
      if (sum(toto) != 0){
        processHasChanged <- names(self$child.process)[which(max(toto)==toto)]
        newValue <- self$child.process[[processHasChanged]]$Get_Result()
      }
      
      
      #ind <- which(!is.null(lapply(names(self$child.process), function(x){self$tmp.return[[x]]()$value})))
      #processHasChanged <- names(self$child.process)[ind]
      
      # if(is.null(processHasChanged)){
      #   #No process have been modified
      #   self$rv$dataIn <- NULL
      # } else if ( 
      #   sum(unlist(lapply(names(self$child.process), function(x){is.null(self$tmp.return[[x]]()$value)}))) == length(self$child.process)){
      #   # All the child processes have been reseted
      #   self$rv$dataIn <- self$rv$temp.dataIn
      #   self$rv$dataIn <- NULL
      #   
      # } else{
        # Update the status of process
      #  if (length(processHasChanged)==1)
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
              self$rv$dataIn <- self$rv$dataIn[ , , 1:self$GetMaxValidated_BeforeCurrentPos()]
          }
        else{
          # process has been validated
          self$rv$status[processHasChanged] <- global$VALIDATED
          self$Discover_Skipped_Status()
          self$rv$dataIn <- newValue
        }
        
      #}
      
      #update self$old.tmp.return
      # self$old.tmp.return <- setNames(lapply(names(self$child.process), 
      #                                        function(x){self$tmp.return[[x]]()$value}),
      #                                 names(self$child.process))
      # 
      self$Send_Result_to_Caller()
      
    },
    
    GetCurrentStepName = function(){
      cat(paste0(class(self)[1], '::GetCurrentStepName() from - ', self$id, '\n'))
      #browser()
      self$config$steps[self$rv$current.pos]
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
      #browser()
      # Returns NULL to all modules except the one pointed by the current position
      # Initialization of the pipeline : one send dataIn() to the
      # original module
      update <- function(name){
        data <- NULL
        
        if (name == self$GetCurrentStepName()){
          # One treat the dataset for the current position
          ind.last.validated <- self$GetMaxValidated_BeforeCurrentPos()
          if (is.null(ind.last.validated)){
            data <- self$rv$temp.dataIn
          } else {
            data <- self$rv$dataIn[,,c(1:ind.last.validated)]
          }
        }
        return(data)
      }
      #browser()
      lapply(names(self$child.process), function(x){
        self$rv$data2send[[x]] <- update(x)})
      
      #browser()
      lapply(names(self$child.process), function(x){
        self$rv$data2send[[x]]})
    }
  )
)