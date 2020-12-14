Process = R6Class(
  "Process",
  inherit = ProcessManager,
  private = list(),
  
  public = list(
    modal_txt = "This action will reset this process. The input dataset will be the output of the last previous
                      validated process and all further datasets will be removed",
    
    ui = function(){
      #browser()
      self$screens <- self$GetScreens()
      
      color <- "blue"
      fluidPage(
        shinyjs::useShinyjs(),
        wellPanel(
          style=paste0("background: white; border-width: 2px; border-color: ", color, ";"),
          self$Main_UI()
        )
      )
    },
    
    #---------------------------------------------------------------------
    GetScreens = function(){
      cat(paste0(class(self)[1], '::GetScreens() from - ', self$id, '\n'))
      #browser()
      setNames(lapply(self$config$steps, function(x){
        eval(parse(text = paste0("self$", x, '()')))
      }),
      self$config$steps)
    }
  )
)