# Use of the tip in this page to unshare reactiveValues between different instances
# of the same class
# https://community.rstudio.com/t/r6-class-reactivevalues-property-and-instantiation/31025/2


redBtnClass <- "btn-danger"
PrevNextBtnClass <- "btn-info"
btn_success_color <- "btn-success"
optionsBtnClass <- "info"

btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"




ProcessManager <- R6Class(
  "ProcessManager",
  private = list(),
  public = list(
    
    # Initialize class
    initialize = function(id) {
      self$rv$status <- setNames(rep(global$UNDONE, self$length), self$config$steps)
      
      self$timeline <- TimelineDraw$new(self$ns('TL_draw'), 
                                        mandatory = self$config$mandatory)
      
      self$Additional_Initialize_Class()
      
    },
    
    Additional_Initialize_Class = function(){},
    
    
    EncapsulateScreens = function(){
      fsdfdsfdsf
    },
    
    Additional_Server_Funcs = function(){},
    

    Main_UI = function(){
          self$timeline$ui()
          uiOutput(self$ns('show_screens'))
    },
    
    server = function(dataIn = reactive({NULL})) {
      
      self$timeline$server()

      moduleServer(self$id, function(input, output, session) {
        
        self$Additional_Server_Funcs()
        
        output$show_screens <- renderUI({
          self$EncapsulateScreens()
        })

      })
    }
  )
)
