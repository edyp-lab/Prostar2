Pipeline = R6Class(
  "Pipeline",
  inherit = ProcessManager,
  private = list(),
  
  public = list(
    tmp.return = "<reactiveValues>",
    
    modal_txt = "This action will reset this process. The input dataset will be the output of the last previous
                      validated process and all further datasets will be removed.",
    
    ui = function(){
      cat(paste0(class(self)[1], '::ui() from - ', self$id, '\n'))
      self$screens <- self$GetScreens()
      
      fluidPage(
        self$Main_UI()
      )
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

    
    Additional_Server_Funcs = function(){
      cat(paste0(class(self)[1], '::Additional_Server_Funcs() from - ', self$id, '\n'))
      self$Launch_Module_Server()
    },

    GetScreens = function(){
      cat(paste0(class(self)[1], '::', 'GetScreens() from - ', self$id, '\n'))
      
      setNames(lapply(self$config$steps, function(x){
        self$child.process[[x]]$ui()
      }),
      self$config$steps)
       },
    

    # This function calls the server part of each module composing the pipeline
    Launch_Module_Server = function(){
      cat(paste0(class(self)[1], '::', 'Launch_Module_Server() from - ', self$id, '\n'))
      
      lapply(self$config$steps, function(x){
        self$tmp.return[[x]] <- self$child.process[[x]]$server(
          dataIn = reactive({ self$rv$data2send[[x]] })
          )
      })

    }
    
  )
)