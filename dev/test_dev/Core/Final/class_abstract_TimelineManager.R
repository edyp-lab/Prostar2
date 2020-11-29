redBtnClass <- "btn-danger"
PrevNextBtnClass <- "btn-info"
btn_success_color <- "btn-success"
optionsBtnClass <- "info"

btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"

TimelineManager <- R6Class(
  "TimelineManager",
  private=list( ),
  
  public = list(
    
    id = NULL,
    style = NULL,
    config = "<reactiveValues>",
    rv = "<reactiveValues>",
    length = 0,
    modal_txt = NULL,
    btn_style = "display:inline-block; vertical-align: middle; padding: 7px",
    timelineDraw  = NULL,
    
    #-- Initialize class
    initialize = function(id, mandatory, style = 2) {
      cat(paste0(class(self)[1], '::initialize() from - ', id, '\n'))
      #browser()
      self$id <- id
      self$length <- length(mandatory)
      
      self$config <- reactiveValues()
      self$rv <- reactiveValues(
        current.pos = 1,
        reset_OK = NULL,
        isAllSkipped = FALSE
      )
      
      self$timelineDraw <- TimelineDraw$new(NS(self$id)('TL_draw'), 
                                            mandatory = mandatory,
                                            style = style)
    },
    
    # UI
    ui = function() {},
    
    # SERVER
    server = function(config, dataLoaded) {
      ns <- NS(self$id)
      
      # MODULE SERVER
      moduleServer(self$id, function(input, output, session) {
        ns <- NS(self$id)
        
        
        
          list(current.pos = reactive({self$rv$current.pos}),
               tl.reset = reactive({self$rv$reset_OK})
          )
      })
    }
  )
)