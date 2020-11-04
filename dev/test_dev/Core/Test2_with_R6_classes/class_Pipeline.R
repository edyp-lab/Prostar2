#Timeline_R6.R
Pipeline = R6Class(
  "Pipeline",
  inherit = ProcessManager,
  private = list(
    config = reactiveValues() ,
    dataOut = reactiveValues(),
    rv = reactiveValues(
      processManagerList = NULL,
      data2send = NULL,
      dataIn = NULL,
      current.pos = NULL,
      wake = F,
      reset = NULL,
      isSkipped = FALSE,
      timeline.res = NULL),
    listUIs = NULL,
    
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
    
    InitializeModule = function(){
      private$config$screens <- private$CreateScreens()
      private$rv$current.pos <- 1
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

    # This function creates the UI parts of the screens (dynamically set 
    # renderUIs). 
    # In the process child class, this method corresponds to the logics function
    Add_RenderUIs_Definitions = function(fun = NULL, input, output){
      
      print("In class_Pipeline::Add_RenderUIs_Definitions()")
     # req(is.null(fun))
      
      #private$logics

      output$process_A <- renderUI(
        tagList(
          div(id=NS(private$id)('process_A'),
              private$logics[['process_A']]$ui(),
              )
          )
        )
      
      output$process_Description <- renderUI(
        tagList(
          div(id=NS(private$id)('process_Description'),
              private$logics[['process_Description']]$ui(),
              )
        )
      )

    },
    
    # This function calls the server part of each module composing the pipeline
    Launch_Module_Server = function(){

      # print("In class_Pipeline::Launch_Module_Server()")
      # source(file.path('.', 'process_A.R'), local=TRUE)$value
      # 
      private$logics[['process_A']]$server(
        dataIn = reactive({private$rv$data2send[['process_A']]}),
        dataOut = dataOut,
        remoteReset = reactive({NULL}),
        isSkipped = reactive({NULL}),
        logics = ProcessLogics_processA)
      
      private$logics[['process_Description']]$server(
        dataIn = reactive({private$rv$data2send[['process_Description']]}),
        dataOut = dataOut,
        remoteReset = reactive({NULL}),
        isSkipped = reactive({NULL}),
        logics = ProcessLogics_Description)
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
    },

    
    TimelineUI = function(){
      private$timeline$ui()
    },
    
    CreateTimeline = function(){

      private$timeline <- TimelineForPipeline$new(
        id = NS(private$id)('timeline'),
        mandatory = private$config$mandatory
      )
    }
    
  ),
  
  public = list(
    initialize = function(id) {

      private$id <- id
      conf <- list(process.name = 'Pipeline',
                     steps = c('process_Description', 'process_A'),
                     mandatory = setNames(c(T,F), c('process_Description', 'process_A'))
      )
      
      private$steps <-  conf$steps
      private$length <- length(conf$steps)

      lapply(names(conf), function(x){private$config[[x]] <- conf[[x]]})
      private$config$status <- setNames(rep(0, private$length),
                                        conf$steps)
      
      
      
      private$logics <- list(process_Description = Process$new("process_Description"),
                             process_A = Process$new("process_A")
      )

    }
    
  )
)