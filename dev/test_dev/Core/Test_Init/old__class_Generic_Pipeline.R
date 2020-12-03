
    
    ActionsOn_NoTmp_Input = function(){
      print("ActionsOn_NoTmp_Input() on class_Pipeline.R")
      print("-----------------------------------------------")

      self$config$screens <- self$GetScreensDefinition()
      
      self$InitializeModule()
      self$InitializeTimeline()
      
      self$PrepareData2Send()
    },
    
    
    