TimelineManager <- R6Class(
  "TimelineManager",
  private=list( ),
  
  public = list(
    initialize = function(){
      stop(" TimelineManager is an abstract class that can't be initialized.")
    },
    
    id = NULL,
    style = 2,
    type = 'generic',
    config = "<reactiveValues>",
    rv = "<reactiveValues>",
    
    global = list(VALIDATED = 1,
                  SKIPPED = -1,
                  UNDONE = 1
    ),
    default_pos =list(VALIDATED = 1,
                      SKIPPED = 1,
                      UNDONE = 1
    ), 
    nbSteps = 0,
    modal_txt = NULL,
    btn_style = "display:inline-block; vertical-align: middle; padding: 7px",
    timelineDraw  = NULL,
    
    
    Analyse_status = function(){},
    
    Init_Default_Positions = function(){
      cat(paste0(class(self)[1], '::Init_Default_Positions()\n'))
      self$default_pos <- list(VALIDATED = self$nbSteps,
                               SKIPPED = self$nbSteps,
                               UNDONE = 1
      )
    },
    
    NextBtn_logics = function(){
      cat(paste0(class(self)[1], '::NextBtn_logics()\n'))
      # Compute status for the Next button
      end_of_tl <- self$rv$current.pos == self$nbSteps
      mandatory_step <- isTRUE(self$config$mandatory[self$rv$current.pos])
      validated <- self$config$status[self$rv$current.pos] == self$global$VALIDATED
      skipped <- self$config$status[self$rv$current.pos] == self$global$SKIPPED
      entireProcessSkipped <- self$config$status[self$nbSteps] == self$global$SKIPPED
      NextBtn_logics <- !end_of_tl && !entireProcessSkipped && (!mandatory_step || (mandatory_step && (validated || skipped)))
      NextBtn_logics
    },
    
    PrevBtn_logics = function(){
      cat(paste0(class(self)[1], '::PrevBtn_logics()\n'))
      # Compute status for the Previous button
      start_of_tl <- self$rv$current.pos == 1
      entireProcessSkipped <- self$config$status[self$nbSteps] == self$global$SKIPPED
      PrevBtn_logics <- !start_of_tl && !entireProcessSkipped
      PrevBtn_logics
    },
    
    
    CheckConfig = function(conf){
      cat(paste0(class(self)[1], '::CheckConfig()\n'))
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
    
    Update_Cursor_position = function(){
      cat(paste0(class(self)[1], '::Update_Cursor_position()\n'))
      req(self$config$status)
      if (self$config$status[self$nbSteps] == self$global$VALIDATED)
        self$rv$current.pos <- self$default_pos$VALIDATED
      else if (self$config$status[self$nbSteps] == self$global$SKIPPED)
        self$rv$current.pos <- self$default_pos$SKIPPED
      else if (self$config$status[self$nbSteps] == self$global$UNDONE)
        self$rv$current.pos <- self$default_pos$UNDONE
    },
    
    Display_Current_Step = function(){},
    
    Analyse_Status = function(){},
    # Initialization of the screens by integrating them into a div specific
    # to this module (name prefixed with the ns() function
    # Those div englobs the div of the caller where screens are defined
    EncapsulateScreens = function(){
      cat(paste0(class(self)[1], '::EncapsulateScreens()\n'))
      req(self$config$screens)
      ns <- NS(self$id)
      self$Init_Default_Positions() 
      self$config$screens <- lapply(1:self$nbSteps,
                                    function(x){
                                      self$config$screens[[x]] <- if (x == 1) 
                                        div(id = ns(paste0("div_screen", x)),  self$config$screens[[x]])
                                      else 
                                        shinyjs::hidden(div(id = ns(paste0("div_screen", x)),  self$config$screens[[x]]))
                                    })
    },
    
    toggleState_Steps = function(cond, i){
      cat(paste0(class(self)[1], '::toggleState_Steps()\n'))
      lapply(1:i, function(x){
        shinyjs::toggleState(paste0('div_screen', x), condition = cond)})
    },
    
    
    # UI
    ui = function() {
      ns <- NS(self$id)
      fluidPage(
        wellPanel(style="background-color: lightblue;",
                  tagList(
                    uiOutput(ns('title')),
                    shinyjs::useShinyjs(),
                    div(id = 'GlobalTL',
                        fluidRow(
                          align= 'center',
                          column(width=2,div(style = self$btn_style,
                                             uiOutput(ns('showPrevBtn')),
                                             uiOutput(ns('showResetBtn'))
                          )
                          ),
                          column(width=8,div( style = self$btn_style,
                                              self$timelineDraw$ui())),
                          column(width=2,div(style = self$btn_style,
                                             uiOutput(ns('showNextBtn')),
                                             uiOutput(ns('showSaveExitBtn'))
                          )
                          )
                        ),
                        uiOutput(ns('show_screens'))
                    )
                  )
        )
      )
    },
    
    SetModalTxt = function(txt){self$modal_txt <- txt},
    
    # SERVER
    server = function(config, wake, remoteReset) {
      ns <- NS(self$id)
      cat(paste0(class(self)[1], '::server()\n'))
      
      observeEvent(config,{
        cat(paste0(class(self)[1], '::observeEvent(config)\n'))
        self$config <- config
        self$rv$current.pos <- 1
      })
      
      observeEvent(config$status,{
        cat(paste0(class(self)[1], '::observeEvent(config$status)\n'))
        self$config$status <- config$status
      })
      
      observeEvent(req(wake()),{
        cat(paste0(class(self)[1], '::observeEvent(req(wake()))\n'))
        self$Update_Cursor_position()
        })
      
      observeEvent(remoteReset(),ignoreInit = T, {
        cat(paste0(class(self)[1], '::observeEvent(remoteReset())\n'))
        self$rv$current.pos <- 1
      })
      
      cat(paste0(class(self)[1], '::self$timelineDraw$server()\n'))
      self$timelineDraw$server(
        status = reactive({self$config$status}),
        position = reactive({self$rv$current.pos})
      )
      
      # MODULE SERVER
      moduleServer(self$id, function(input, output, session) {
        ns <- NS(self$id)
        
        cat(paste0(class(self)[1], '::moduleServer()\n'))
        # Show modal when button reset is clicked
        observeEvent(input$rstBtn, {
          cat(paste0(class(self)[1], '::observeEvent(input$rstBtn)\n'))
          showModal(dataModal())
          })
        
        ###############################
        output$showResetBtn <- renderUI({
          actionButton(ns("rstBtn"), paste0("Reset ", self$type),
                       style='padding:4px; font-size:80%')
        })
        
        output$showPrevBtn <- renderUI({
          shinyjs::disabled(actionButton(ns("prevBtn"), "<<",
                                         style='padding:4px; font-size:80%'))
        })
        
        output$showNextBtn <- renderUI({
          actionButton(ns("nextBtn"), "next",
                       style='padding:4px; font-size:80%')
        })
        
        output$title <- renderUI({ h3(paste0('self$id = ',self$id)) })
        
        #-------------------------------------------------------
        # Return the UI for a modal dialog with data selection input. If 'failed' is
        # TRUE, then display a message that the previous value was invalid.
        dataModal <- function() {
          modalDialog(
            span(self$modal_txt),
            footer = tagList(
              modalButton("Cancel"),
              actionButton(ns("modal_ok"), "OK")
            )
          )
        }
        
       
        
        # When OK button is pressed, update the reactive value which will be sent
        # to the caller
        observeEvent(req(c(input$modal_ok)), ignoreInit=T,{
          cat(paste0(class(self)[1], '::observeEvent(req(c(input$modal_ok)))\n'))
         # browser()
          self$rv$reset_OK <- input$rstBtn
          self$rv$current.pos <- 1
          removeModal()
        })
        
        navPage <- function(direction) {
          newval <- self$rv$current.pos + direction 
          newval <- max(1, newval)
          newval <- min(newval, self$nbSteps)
          if(newval == 0)
            browser()
          
          self$rv$current.pos <- newval
        }
        
        
        observeEvent(input$prevBtn, ignoreInit = TRUE, {navPage(-1)})
        observeEvent(input$nextBtn, ignoreInit = TRUE, {navPage(1)})
        
        output$show_screens <- renderUI({
          cat(paste0(class(self)[1], '::output$show_screens\n'))
          tagList(self$config$screens)
          })
        
        
        # Catch a new position or a change in the status list
        observeEvent(req(c(self$rv$current.pos, self$config$status)), {
           cat(paste0(class(self)[1], '::observeEvent(req(c(self$rv$current.pos, self$config$status))\n'))
          #browser()
          self$Update_Cursor_position()
          self$Analyse_Status()
          self$Display_Current_Step()
          
          shinyjs::toggleState('prevBtn', cond = self$PrevBtn_logics())
          shinyjs::toggleState('nextBtn', cond = self$NextBtn_logics())
        })
        
        
        observeEvent(req(self$config), ignoreInit=F,{
          cat(paste0(class(self)[1], '::observeEvent(req(self$config)\n'))
          req(self$nbSteps>0)
          check <- self$CheckConfig(self$config)
          if (!check$passed)
            stop(paste0("Errors in 'config'", paste0(check$msg, collapse=' ')))
          
          self$EncapsulateScreens()
        })
        
        
          list(current.pos = reactive({self$rv$current.pos}),
               reset = reactive({self$rv$reset_OK})
          )
      })
    }
  )
)