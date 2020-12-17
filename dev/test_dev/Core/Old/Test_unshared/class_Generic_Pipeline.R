#Timeline_R6.R
Pipeline = R6Class(
  "Pipeline",
  inherit = ProcessManager,
  private = list(),
  public = list(
    
    tmp.return = "<reactiveValues>",
    old.tmp.return = NULL,
    
    Additional_Funcs_In_Server = function(){},
    
    Additional_Funcs_In_ModuleServer = function(){},
    
    Additional_Initialize_Class = function(){
      self$rv$data2send <- NULL
      self$tmp.return <- reactiveValues()
    },
    
    ActionsOn_NoTmp_Input = function(){
      print("ActionsOn_NoTmp_Input() on class_Pipeline.R")
      print("-----------------------------------------------")

      self$rv$data2send <- setNames(lapply(1:self$length,  function(x){NULL}), self$config$steps)
      self$Launch_Module_Server()
      self$config$screens <- self$GetScreensDefinition()
      
      self$InitializeModule()
      self$InitializeTimeline()
      
      self$PrepareData2Send()
    },
    
    CreateTimeline = function(){
      cat(paste0(class(self)[1], '::', 'CreateTimeline() from - ', self$id, '\n'))
      self$timeline <- TimelineForPipeline$new(
        id = NS(self$id)('timeline'),
        mandatory = self$config$mandatory
      )
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
    Actions_On_Data_Trigger = function(){
      #browser()
      print(setNames(lapply(names(self$ll.process), function(x){self$old.tmp.return[[x]]}),
                     names(self$ll.process))
      )
      print(setNames(lapply(names(self$ll.process), function(x){self$tmp.return[[x]]()$value}),
                     names(self$ll.process))
      )
      
      processHasChanged <- unlist(lapply(names(self$ll.process), function(x){
        if (length(self$tmp.return[[x]]()$value) != length(self$old.tmp.return[[x]])) {x}
      }
      ))
      
     if(is.null(processHasChanged)){
       #No process have been modified
       self$rv$dataIn <- NULL
     } else if ( 
          sum(unlist(lapply(names(self$ll.process), function(x){is.null(self$tmp.return[[x]]()$value)}))) == length(self$ll.process)){
    # All the child processes have been reseted
    self$rv$dataIn <- self$rv$temp.dataIn
    self$rv$dataIn <- NULL
    
  } else{
      # Update the status of process
      if (length(processHasChanged)==1)
        if (is.null(self$tmp.return[[processHasChanged]]()$value)){
          # process has been reseted
          self$config$status[processHasChanged] <- self$global$UNDONE
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
          self$config$status[processHasChanged] <- self$global$VALIDATED
          self$Set_Skipped_Status()
          self$rv$dataIn <- self$tmp.return[[processHasChanged]]()$value
        }
      
  }
      
      #update self$old.tmp.return
      self$old.tmp.return <- setNames(lapply(names(self$ll.process), 
                                             function(x){self$tmp.return[[x]]()$value}),
                                      names(self$ll.process))
      
      self$Send_Result_to_Caller()
      
    },
    
    ActionsOnNewPosition = function(){
      self$PrepareData2Send()
    },
    

    
    # This function calls the server part of each module composing the pipeline
    Launch_Module_Server = function(){
      ns <- NS(self$id)

      self$ll.process <- setNames(lapply(self$config$steps,
                                         function(x){
                                           assign(x, get(x))$new(ns(x))
                                         }),
                                  self$config$steps
      )

      lapply(self$config$steps, function(x){
        self$tmp.return[[x]] <- self$ll.process[[x]]$server(dataIn = reactive({self$rv$data2send[[x]]}),
                                                            reset = reactive({self$rv$reset}),
                                                            isSkipped = reactive({self$config$status[x] == self$global$SKIPPED})
                                                            )
      })
                                                                     
      #browser()                                                                      
      # Catch the returned values of the process                                                           
      observeEvent(lapply(names(self$ll.process), function(x){self$tmp.return[[x]]()$trigger}), {
        self$Actions_On_Data_Trigger()
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
      lapply(names(self$ll.process), function(x){
        self$rv$data2send[[x]] <- update(x)})
    },
  
  InitConfig = function(config){
    cat(paste0(class(self)[1], '::', 'InitConfig() from - ', self$id, '\n'))
    check <- self$CheckConfig(config)
    if (!check$passed)
      stop(paste0("Errors in 'config'", paste0(check$msg, collapse=' ')))
    
    self$length <- length(config$steps)
    
    observeEvent(config, {
      cat(paste0(class(self)[1], '::', 'observe() in InitConfig(config) from - ', self$id, '\n'))
      lapply(names(config), function(x){self$config[[x]] <- config[[x]]})
      self$config$type = class(self)[2]
      self$config$status <- setNames(rep(0, self$length), config$steps)
      
      #self$config$screens <- self$GetScreensDefinition()
      self$config$mandatory <- setNames(self$config$mandatory, self$config$steps)
      
      self$ll.process <- setNames(lapply(self$config$steps, function(x){x <- NULL}),
                                  self$config$steps)
      self$CreateTimeline()
    })
    
  },
  
  
  GetScreensDefinition = function(){
    browser()
    setNames(lapply(self$config$steps, function(x){
      self$ll.process[[x]]$ui()
    }),
    self$config$steps)

    # setNames(lapply(self$config$steps, function(x){
    #   eval(parse(text = paste0("self$", x, '()')))
    # }),
    # self$config$steps)
    
  }
  
  # 
  # Add_RenderUIs_Definitions = function(input, output){
  #   cat(paste0(class(self)[1], '::', 'Add_RenderUIs_Definitions()\n'))
  #   ns <- NS(self$id)
  #   
  #   
  #   lapply(names(self$ll.process), function(x){
  #     output[[x]] <- renderUI({
  #       tagList(
  #         div(id=NS(self$id)(x),
  #             self$ll.process[[x]]$ui()
  #         )
  #       )
  #     })
  #   }
  #   )
  #   
  # }

    
  )
)