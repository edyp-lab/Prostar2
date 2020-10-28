#Timeline_R6.R
TimelineManager = R6Class(
  "TimelineManager",
  private = list(
    # attributes
    verbose = T,
    style = 2,
    length = NULL,
    modal_txt = NULL,
    DEFAULT_SKIPPED_POSITION = 1,
    DEFAULT_VALIDATED_POSITION = 1,
    DEFAULT_UNDONE_POSITION = 1,
    reset_OK = 0,
    timelineDraw  = NULL,
    
    
    
    CheckConfig = function(conf){
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
      names.conf <- c("process.name", "type", "steps")
      if (!all(sapply(names.conf, function(x){x %in% names(conf)}))){
        passed <- F
        msg <- c(msg, "The names of elements in 'config' must be the following: 'process.name', 'type', 'steps'")
      }
      if (!is.list(conf$steps)){
        passed <- F
        msg <- c(msg, "The 'steps' slot is not a list")
      }

      passed <- T
      list(passed=passed,
           msg = msg)
    },
    
    
    toggleState_Steps = function(cond, i){
      if(private$verbose)
        print(paste0('TL(', self$id, ') : toggleState_Steps() : cond = ', cond, ', i = ', i))
      
      lapply(1:i, function(x){
        shinyjs::toggleState(paste0('div_screen', x), condition = cond)})
    },
    
    Analyse_status_Process = function(){},
    
    Update_Cursor_position = function(){
      req(private$length)
      
      if(private$verbose)
        print(paste0('TL(', self$id, ') : Update_Cursor_position() :'))
      
      if ((self$rv$status[[private$length]] == VALIDATED))
        self$rv$current.pos <- private$DEFAULT_VALIDATED_POSITION
      else if (self$rv$status[[private$length]] == SKIPPED)
        self$rv$current.pos <- private$DEFAULT_SKIPPED_POSITION
      else if (self$rv$status[[private$length]] == UNDONE)
        self$rv$current.pos <- private$DEFAULT_UNDONE_POSITION
      if(self$rv$current.pos==0)
        browser()
    },
    Analyse_status = function(){}
    
    ),
  
  public = list(
    # attributes
    id = NULL,
    config = NULL,
    steps = NULL,
    
    rv = reactiveValues(
      rst_btn = NULL,
      current.pos = 1
    ),
    
    # initializer
    initialize = function(id, steps, style=2 ){
      self$id = id

     self$steps = steps
      source(file.path('.', 'class_TimelineDraw.R'), local=TRUE)$value
      
      private$timelineDraw <- TimelineDraw$new(NS(id)('timeline'), 
                                               steps = steps,
                                               style = style)
    },
    
    
    
    
    # UI
    ui = function(){
      
      # the ns function here will prepend a prefix to all the ids in the app.
      ns = NS(self$id)
      
      tagList(
        shinyjs::useShinyjs(),
        div(id = 'GlobalTL',
            fluidRow(
              align= 'center',
              column(width=2,div(style=btn_style,
                                 uiOutput(ns('showPrevBtn')),
                                 uiOutput(ns('showResetBtn'))
              )),
              column(width=8,div( style = btn_style, private$timelineDraw$ui())),
              column(width=2,div(style=btn_style,
                                 uiOutput(ns('showNextBtn')),
                                 uiOutput(ns('showSaveExitBtn'))
              )
              )
            )
        ),
        uiOutput(ns('show_screens'))
      )
    },
    
    # server
    server = function(config, wake) {
      ns <- NS(self$id)

      private$timelineDraw$server(
        status = reactive({config$steps$status}),
        position = reactive({self$rv$current.pos})
        )
      
      
      moduleServer(self$id, function(input, output, session) {
        
        #output$test <- renderUI({private$timeline$ui()})
  
         # observeEvent(req(config), {
         #  self$rv$steps <- config$steps
         #  self$rv$status <- config$status
         #  
         # private$timeline$server(steps = self$rv$steps,
         #                          status = self$rv$status,
         #                          pos = self$rv$current.pos
         #                          )
         # print('after calling timeline$server()')
         #  })
        
        
        #----------------------------------------------------------
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
       
      
    } # End of moduleServer function
  ) # End of moduleServer function
},

# end of server function
) # End of public=list()
) # End of R6class