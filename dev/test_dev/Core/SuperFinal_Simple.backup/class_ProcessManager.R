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
    # Declaration of variables
    input = NULL,
    id = NULL,
    ns = NULL,
    style = NULL,
    child.process = NULL,
    length = NULL,
    config = NULL,
    screens = NULL,
    timeline  = NULL,

    rv = "<reactiveValues>",
    
    
    # Initialize class
    initialize = function(id) {
      cat(paste0(class(self)[1], '::initialize() from - ', self$id, '\n'))
      self$id <- id
      self$ns <- NS(id)
      self$config <- private$.config
      self$length <- length(self$config$mandatory)
      self$config$type = class(self)[2]
      self$config$mandatory <- setNames(self$config$mandatory, self$config$steps)
      self$rv = reactiveValues(
        dataIn = NULL,
        status = setNames(rep(global$UNDONE, self$length), self$config$steps),
        tl.tags.enabled = setNames(rep(FALSE, self$length), self$config$steps)
        )
      
   
      self$rv$status <- setNames(rep(global$UNDONE, self$length), self$config$steps)
      
      self$timeline <- TimelineDraw$new(self$ns('TL_draw'), mandatory = self$config$mandatory)
      
      self$Additional_Initialize_Class()
      self$screens <- self$GetScreens_ui()
    },
    
    Additional_Initialize_Class = function(){},
    Additional_Server_Funcs = function(){},
    GetScreens_listeners = function(){},
    
    
    EncapsulateScreens = function(){
      lapply(1:self$length, function(i) {
          div(id = self$ns(self$config$steps[i]),
              class = paste0("page_", self$id),
              self$screens[[i]]
            )
      }
      )
    },
    
    
    ui = function(){
      cat(paste0(class(self)[1], '::', 'Main_UI() from - ', self$id, '\n'))
       tagList(
        shinyjs::useShinyjs(),
        self$timeline$ui(),
        div(id = self$ns('Screens'),
            self$EncapsulateScreens() )
        )
    },

    ###############################################################
    ###                          SERVER                         ###
    ###############################################################
    server = function(dataIn = reactive({NULL})) {
      cat(paste0(class(self)[1], '::server(dataIn) from - ', self$id, '\n'))

      self$timeline$server(status = reactive({self$rv$status}),
                           position = reactive({self$rv$current.pos}),
                           enabled = reactive({self$rv$tl.tags.enabled})
      )
      
      self$Additional_Server_Funcs()
      
      ###############################################################
      ###                    MODULE SERVER                        ###
      ###############################################################
      moduleServer(self$id, function(input, output, session) {
        cat(paste0(class(self)[1], '::moduleServer(input, output, session) from - ', self$id, '\n'))
        
        self$input <- input
        
        #Used to get the observeEvent functions
        self$GetScreens_listeners()
      })
    }
)
)
