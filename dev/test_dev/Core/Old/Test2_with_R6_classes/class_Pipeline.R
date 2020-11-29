#Timeline_R6.R
Pipeline = R6Class(
  "Pipeline",
  inherit = ProcessManager,
  private = list(
logics = list(),
    ActionsOn_NoTmp_Input = function(){
      print("ActionsOn_NoTmp_Input() on class_Pipeline.R")
      private$InitializeModule()
      private$InitializeTimeline()
      private$rv$data2send <- setNames(lapply(1:private$length, 
                                      function(x){NA}), 
                               private$config$steps)
      private$Launch_Module_Server()
      private$PrepareData2Send()
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
      valid_obj_val <-  class(private$dataOut$obj) == 'QFeatures'
      # Update the status
      private$config$status[private$dataOut$name] <- valid_obj_val
      private$Set_Skipped_Status()
      
      # Store the result of a process module
      if (valid_obj_val)
        private$rv$dataIn <- private$dataOut$obj
      else
        private$rv$dataIn <- rv$dataIn[,,1:private$GetMaxValidated_BeforeCurrentPos()]

      private$Send_Result_to_Caller()
    },
    
    ActionsOnNewPosition = function(){
      private$PrepareData2Send()
    },

    
    # This function calls the server part of each module composing the pipeline
    Launch_Module_Server = function(){
        lapply(names(private$logics), function(x){
          private$logics[[x]]$server(
            dataIn = reactive({private$rv$data2send[[x]]}),
            dataOut = dataOut,
            remoteReset = reactive({NULL}),
            isSkipped = reactive({NULL})
            )
        }
        )
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

        if (name == private$GetCurrentStepName()){
          # One treat the dataset for the current position
          ind.last.validated <- private$GetMaxValidated_BeforeCurrentPos()
          if (is.null(ind.last.validated)){
            data <- private$rv$temp.dataIn
          } else {
            data <- private$rv$dataIn[,,c(1:ind.last.validated)]
          }
        }
        return(data)
      }
      
      lapply(private$config$steps, function(x){
        private$rv$data2send[[x]] <- update(x)})

      #return_of_process$obj <- NA
    }

    
  ),
  
  public = list(
    initialize = function() {
      stop(" Process is an abstract class that can't be initialized.")
    }

  )
)