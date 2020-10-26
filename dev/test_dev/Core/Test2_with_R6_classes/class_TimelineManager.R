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
    timeline = NULL,
    
    
    
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
      
      list(passed=passed,
           msg = msg)
    },
    
    
    toggleState_Steps = function(cond, i){
      if(private$verbose)
        print(paste0('TL(', self$rv$process.name, ') : toggleState_Steps() : cond = ', cond, ', i = ', i))
      
      lapply(1:i, function(x){
        shinyjs::toggleState(paste0('div_screen', x), condition = cond)})
    },
    
    Analyse_status_Process = function(){},
    
    Update_Cursor_position = function(){
      req(private$length)
      
      if(private$verbose)
        print(paste0('TL(',self$rv$process.name, ') : Update_Cursor_position() :'))
      
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
    
    rv = reactiveValues(
      rst_btn = NULL,
      position = 0,
      current.pos = 1,
      process.name = NULL,
      status = NULL,
      steps = NULL
    ),
    
    # initializer
    initialize = function(id, style=2 ){
      self$id = id
      private$style = style
      private$timeline <- TimelineStyle$new(NS(self$id)('timeline'), style = style)
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
              column(width=8,div( style = btn_style, uiOutput(ns('test')))),
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
      #browser()
      
      moduleServer(self$id, function(input, output, session) {
        
        output$test <- renderUI({private$timeline$ui()})
  
         observeEvent(req(config), {
          self$rv$steps <- config$steps
          self$rv$process.name <- config$process.name
          self$rv$status <- config$status
          
         private$timeline$server(steps = self$rv$steps,
                                  status = self$rv$status,
                                  pos = self$rv$current.pos
                                  )
         print('after calling timeline$server()')
          })
        
        output$showResetBtn <- renderUI({
          print(paste0('TL(',config$process.name, ') : output$showResetBtn <- renderUI'))
          actionButton(ns("rstBtn"), paste0("Reset ", config$type),
                       style='padding:4px; font-size:80%')
        })
        
        output$showPrevBtn <- renderUI({
          shinyjs::disabled(actionButton(ns("prevBtn"), "<<",
                                         style='padding:4px; font-size:80%'))
        })
        
        output$showNextBtn <- renderUI({
          shinyjs::disabled(actionButton(ns("nextBtn"), "next",
                                         style='padding:4px; font-size:80%'))
        })
        
        
        
        #-------------------------------------------------------
        # Return the UI for a modal dialog with data selection input. If 'failed' is
        # TRUE, then display a message that the previous value was invalid.
        dataModal <- function() {
          modalDialog(
            span(private$modal_txt),
            footer = tagList(
              modalButton("Cancel"),
              actionButton(ns("modal_ok"), "OK")
            )
          )
        }
        
        
        # Show modal when button reset is clicked
        observeEvent(input$rstBtn, {
          showModal(dataModal())
        })
        
        # When OK button is pressed, update the reactive value which will be sent
        # to the caller
        observeEvent(input$modal_ok, {
          private$reset_OK <- input$rstBtn
          removeModal()
        })
        
        #----------------------------------------------------------
        
        
        navPage <- function(direction) {
          newval <- self$rv$current.pos + direction 
          newval <- max(1, newval)
          newval <- min(newval, private$length)
          if(newval == 0)
            browser()
          
          self$rv$current.pos <- newval
        }
        
        
        observeEvent(req(wake()),{
          if(private$verbose)
            print(paste0('TL(',config$process.name, ') : observeEvent(current$wake() '))
          
          private$Update_Cursor_position()
        })
        
        
        observeEvent(req(config), ignoreInit=F,{
          if(private$verbose)
            print(paste0('TL(',config$process.name, ') : observeEvent(req(config)() '))
          req(length(config$screens)>0)
          
          if (!private$CheckConfig(config)$passed)
            stop(paste0("Errors in 'config'", paste0(private$CheckConfig(config)$msg, collapse=' ')))
          
          InitScreens()
          
        })
        
        
        
        Init_Default_Positions <- reactive({
          private$DEFAULT_VALIDATED_POSITION <- private$length
          private$DEFAULT_SKIPPED_POSITION <- private$length
          private$DEFAULT_UNDONE_POSITION <- 1
        })
        
        
        # Initialization of the screens by integrating them into a div specific
        # to this module (name prefixed with the ns() function
        # Those div englobs the div of the caller where screens are defined
        InitScreens <- reactive({
          if(private$verbose)
            print(paste0('TL(',config$process.name, ') : call to InitScreens() '))
          req(config$screens)
          private$length <- length(config$steps)
          Init_Default_Positions() 
          config$screens <- lapply(1:private$length,
                                   function(x){
                                     config$screens[[x]] <- if (x == 1) 
                                       div(id = ns(paste0("div_screen", x)),  config$screens[[x]])
                                     else 
                                       shinyjs::hidden(div(id = ns(paste0("div_screen", x)),  config$screens[[x]]))
                                   })
        })
        
        
        
        observeEvent(input$prevBtn, ignoreInit = TRUE, {navPage(-1)})
        observeEvent(input$nextBtn, ignoreInit = TRUE, {navPage(1)})
        
        
        output$show_screens <- renderUI({tagList(config$screens)})
        
        
        Update_Buttons <- reactive({
          # Compute status for the Next button
          end_of_tl <- self$rv$current.pos == private$length
          mandatory_step <- isTRUE(config$steps[[self$rv$current.pos]])
          validated <- config$status[[self$rv$current.pos]] == VALIDATED
          skipped <- config$status[[self$rv$current.pos]] == SKIPPED
          entireProcessSkipped <- config$status[[private$length]] == SKIPPED
          NextBtn_logics <- !end_of_tl && !entireProcessSkipped && (!mandatory_step || (mandatory_step && (validated || skipped)))
          
          # Compute status for the Previous button
          start_of_tl <- self$rv$current.pos == 1
          entireProcessSkipped <- config$status[[private$length]] == SKIPPED
          PrevBtn_logics <- !start_of_tl && !entireProcessSkipped
          
          shinyjs::toggleState('prevBtn', cond = PrevBtn_logics)
          shinyjs::toggleState('nextBtn', cond = NextBtn_logics)
        })
        
        # Catch a new position or a change in the status list
        observeEvent(req(c(self$rv$current.pos, config$status)), {
          req(private$length)
          if(private$verbose){
            print(paste0('TL(',config$process.name, ') : observeEvent(req(c(self$rv$current.pos, config$status)) : '))
            print(paste0('TL(',config$process.name, ') : status = ', paste0(config$status, collapse=' ')))
          }
          
          private$Analyse_status()
          Update_Buttons()
        })
       
      
    } # End of moduleServer function
  ) # End of moduleServer function
},

GetCurrentPosition = function(){invisible(self$rv$current.pos)},

GetResetAction = function(){invisible(private$reset_OK)},

SetModalTxt = function(txt){private$modal_txt <- txt}# end of server function
) # End of public=list()
) # End of R6class