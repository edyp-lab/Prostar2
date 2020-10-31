#Timeline_R6.R
Pipeline = R6Class(
  "Pipeline",
  inherit = ProcessManager,
  private = list(
    
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
        if (name == GetCurrentStepName()){
          # One treat the dataset for the current position
          ind.last.validated <- GetMaxValidated_BeforeCurrentPos()
          if (is.null(ind.last.validated)){
            data <- dataIn()
          } else {
            data <- rv$dataIn[,,c(1:ind.last.validated)]
          }
        }
        
        return(data)
      }
      
      lapply(names(rv$data2send), function(x){rv$data2send[[x]] <- update(x)})
    },
    
  ),
  
  public = list(
    initialize = function(id, config=NULL) {
      private$id <- id
      private$steps <- names(config$status)
      private$length <- length(config$status)
      lapply(names(config), function(x){private$config[[x]] <- config[[x]]})
      private$Initialize_Status_Process()
    }
    
  )
)