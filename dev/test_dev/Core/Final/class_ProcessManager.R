# Use of the tip in this page to unshare reactiveValues between different instances
# of the same class
# https://community.rstudio.com/t/r6-class-reactivevalues-property-and-instantiation/31025/2

ProcessManager <- R6Class(
  "ProcessManager",
  private = list(),
  public = list(
    input = NULL,
    id = NULL,
    ns = NULL,
    timeline = NULL,
    timeline.res = NULL,
    ll.process = NULL,
    length = NULL,
    config = NULL,
    screens = NULL,
    status = NULL,
    
    dataOut = "<reactiveValues>",
    rv = "<reactiveValues>",
    
    initialize = function(id) {
      cat(paste0(class(self)[1], '::initialize() from - ', self$id, '\n'))
      self$id <- id
      self$ns <- NS(id)
      
      check <- self$CheckConfig(private$.config)
      if (!check$passed)
        stop(paste0("Errors in 'config'", paste0(check$msg, collapse=' ')))
      else
        self$config <- private$.config
      
      
      
      
      self$config$mandatory <- setNames(self$config$mandatory, self$config$steps)
      self$length <- length(self$config$mandatory)
      
      self$dataOut = reactiveValues(
        value = NULL,
        trigger = NULL
      )
      
      self$rv = reactiveValues(
        dataIn = NULL,
        current.pos = NULL,
        status = NULL,
        reset = NULL,
        isSkipped = FALSE,
        dataLoaded = FALSE)
      browser()
      self$screens <- self$GetScreensDefinition()
      
      self$timeline <- TimelineForProcess$new(self$ns('TL_draw'), 
                                            config = self$config,
                                            screens = self$screens,
                                            style = style)

      },

    
    CheckConfig = function(conf){
      cat(paste0(class(self)[1], '::CheckConfig() from - ', self$id, '\n'))
      passed <- T
      msg <- ""
      if (!is.list(conf)){
        passed <- F
        msg <- c(msg, "'config' is not a list")
      }
      if (length(conf)!=3){
        passed <- F
        msg <- c(msg, "The length of 'config' is not equal to 4")
      }
      names.conf <- c("name", "steps", "mandatory")
      if (!all(sapply(names.conf, function(x){x %in% names(conf)}))){
        passed <- F
        msg <- c(msg, "The names of elements in 'config' must be the following: 'name', 'steps', 'mandatory'")
      }
      if (length(conf$steps) != length(conf$mandatory)){
        passed <- F
        msg <- c(msg, "The length of 'steps' and 'mandatory' must be equal.")
      }
      
      passed <- T
      list(passed=passed,
           msg = msg)
    },
    GetScreensDefinition = function(){},
    
    # UI
    ui = function() {

    },
    
    # SERVER
    server = function(dataIn, reset, isSkipped) {
     
      self$timeline$server(status = reactive({self$rv$status}),
                           dataLoaded = reactive({FALSE})
                           )
      
      
      # MODULE SERVER
      moduleServer(self$id, function(input, output, session) {
       
        observe({
          self$input <- input
        })
        
        
        
      reactive({self$dataOut})
      })
    }
)
)