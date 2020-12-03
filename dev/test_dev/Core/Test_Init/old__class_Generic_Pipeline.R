#Timeline_R6.R
Pipeline = R6Class(
  "Pipeline",
  inherit = ProcessManager,
  private = list(),
  public = list(

    
    
    ActionsOn_NoTmp_Input = function(){
      print("ActionsOn_NoTmp_Input() on class_Pipeline.R")
      print("-----------------------------------------------")

      self$config$screens <- self$GetScreensDefinition()
      
      self$InitializeModule()
      self$InitializeTimeline()
      
      self$PrepareData2Send()
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
  
  
  

    
  )
)