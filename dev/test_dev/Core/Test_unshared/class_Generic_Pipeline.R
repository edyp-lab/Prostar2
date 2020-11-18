#Timeline_R6.R
Pipeline = R6Class(
  "Pipeline",
  inherit = ProcessManager,
  private = list(),
  public = list(
    rv = reactiveValues(
      processManagerList = NULL,
      data2send = NULL
    ),
    listUIs = NULL,
    ll.process = NULL,
    tmp.return = reactiveValues(),
    old.tmp.return = NULL,
    
    CreateTimeline = function(){
      cat(paste0(class(self)[1], '::', 'CreateTimeline()\n'))
      self$timeline <- TimelineForPipeline$new(
        id = NS(self$id)('timeline'),
        mandatory = self$config$mandatory
      )
    },
    
    Additional_Funcs_In_Server = function(){},
    
    Additional_Funcs_In_ModuleServer = function(){},
    
    ActionsOnReset = function(){
      cat(paste0(class(self)[1], '::', 'ActionsOnReset()\n'))
      browser()
      self$ResetScreens()
      self$rv$dataIn <- NA
      self$Initialize_Status_Process()
      self$Send_Result_to_Caller()
      self$InitializeDataIn()
    },
    
    ActionsOn_NoTmp_Input = function(){
      print("ActionsOn_NoTmp_Input() on class_Pipeline.R")
      print("-----------------------------------------------")

      self$rv$data2send <- setNames(lapply(1:self$length,  function(x){NA}), self$config$steps)
      self$Launch_Module_Server()
      self$InitializeModule()
      self$InitializeTimeline()
      
      self$PrepareData2Send()
    },
    
    InitializeModule = function(){
      self$config$screens <- self$CreateScreens()
      self$rv$current.pos <- 1
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
    ActionsOnDataTrigger = function(){
      valid_obj_val <-  class(self$dataOut$obj) == 'QFeatures'
      # Update the status
      self$config$status[self$dataOut$name] <- valid_obj_val
      self$Set_Skipped_Status()
      
      # Store the result of a process module
      if (valid_obj_val)
        self$rv$dataIn <- self$dataOut$obj
      else
        self$rv$dataIn <- rv$dataIn[,,1:self$GetMaxValidated_BeforeCurrentPos()]
      
      self$Send_Result_to_Caller()
    },
    
    ActionsOnNewPosition = function(){
      self$PrepareData2Send()
    },
    

    
    # This function calls the server part of each module composing the pipeline
    Launch_Module_Server = function(){
      ns <- NS(self$id)

      self$ll.process <- setNames(lapply(names(self$ll.process),
                                         function(x){
                                           assign(x, get(x))$new(ns(x))
                                         }),
                                  names(self$ll.process)
      )

      lapply(names(self$ll.process), function(x){
        self$tmp.return[[x]] <- self$ll.process[[x]]$server(dataIn = reactive({self$rv$data2send[[x]]}),
                                                            remoteReset = reactive({NULL}),
                                                            isSkipped = reactive({NULL}))
      })
                                                                     
                                                                            
      # Catch the returned values of the process                                                           
      observeEvent(lapply(names(self$ll.process), function(x){self$tmp.return[[x]]()$trigger}), {
        
        print(setNames(lapply(names(self$ll.process), function(x){self$old.tmp.return[[x]]$value}),
                       names(self$ll.process))
        )
        print(setNames(lapply(names(self$ll.process), function(x){self$tmp.return[[x]]()$value}),
                       names(self$ll.process))
        )
       
        processHasChanged <- unlist(lapply(names(self$ll.process), function(x){
          if (length(self$tmp.return[[x]]()$value) != length(self$old.tmp.return[[x]]$value)) {x}
        }
          ))

        if (!is.null(processHasChanged))
          if (is.na(self$tmp.return[[processHasChanged]]()$value))
            # process has been reseted
            self$config$status[processHasChanged] <- self$global$UNDONE
          else
            #process has been validated
            self$config$status[processHasChanged] <- self$global$VALIDATED
        
        
        #update self$old.tmp.return
        self$old.tmp.return <- setNames(lapply(names(self$ll.process), function(x){self$tmp.return[[x]]()$value}),
                                          names(self$ll.process))

      })
    },
    
    #To avoid "intempestive" initializations of modules due to dataIn changes
    # one define the following logics :
    #  A dataset is loaded in a module only if this module is not yet
    # validated and if it has not been skipped (this is why we use the
    # max True function
    # To rerun a validated module, the user has to reset it
    # This function returns a non-NULL value to the module which name
    # corresponds to the current position and one send always the last
    # non-NULL dataset before current position
    PrepareData2Send = function(){

      # Returns NULL to all modules except the one pointed by the current position
      # Initialization of the pipeline : one send dataIn() to the
      # original module
      update <- function(name){
        data <- NA
        
        if (name == self$GetCurrentStepName()){
          # One treat the dataset for the current position
          ind.last.validated <- self$GetMaxValidated_BeforeCurrentPos()
          if (is.null(ind.last.validated)){
            data <- self$rv$temp.dataIn
          } else {
            data <- self$rv$temp.dataIn[,,c(1:ind.last.validated)]
          }
        }
        return(data)
      }
      
      lapply(names(self$ll.process), function(x){
        self$rv$data2send[[x]] <- update(x)})
    }

    
  )
)