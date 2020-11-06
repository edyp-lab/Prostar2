TimelineManager <- R6Class(
  "TimelineManager",
  private=list(id = NULL,
               style = 2,
               config = reactiveValues(),
               rv = reactiveValues(
                 current.pos = 1,
                 reset_OK = NULL
               ),
               global = list(VALIDATED = 1, SKIPPED = -1, UNDONE = 1),
               default_pos = list(VALIDATED = 1, SKIPPED = 1, UNDONE = 1), 
               modal_txt = NULL,
               btn_style = "display:inline-block; vertical-align: middle; padding: 7px",
               timelineDraw  = NULL,
               
               
               GetConfig = function(){private$config[[private$id]]},
               SetConfig = function(value){private$config[[private$id]] <- value},
               GetSteps = function(){private$config[[private$id]]$steps},
               SetSteps = function(pos, value){private$config[[private$id]]$steps[pos] <- value},
               SetCompleteSteps = function(value){private$config[[private$id]]$steps <- value},
               GetStatus = function(){private$config[[private$id]]$status},
               SetStatus = function(pos, value){private$config[[private$id]]$status[pos] <- value},
               SetCompleteStatus = function( value){private$config[[private$id]]$status <- value},
               GetMandatory = function(){private$config[[private$id]]$mandatory},
               SetMandatory = function(pos, value){private$config[[private$id]]$mandatory[pos] <- value},
               SetCompleteMandatory = function(value){private$config[[private$id]]$mandatory <- value},
               GetName = function(){private$config[[private$id]]$name},
               SetName = function(value){private$config[[private$id]]$name <- value},
               GetScreens = function(){private$config[[private$id]]$screens},
               SetScreens = function(pos, value){private$config[[private$id]]$screens[[pos]] <- value},
               SetCompleteScreens = function(value){private$config[[private$id]]$screens <- value},
               GetDataOut = function() {private$dataOut[[private$id]]},
               SetDataOut = function(data){private$dataOut[[private$id]] <- data},
               Length = function(){length(private$config[[private$id]]$steps)},
               SetCurrentPos = function(value){private$rv[[private$id]]$current.pos <- value},
               GetCurrentPos = function(){private$rv[[private$id]]$current.pos},
               SetResetOk = function(value){private$rv[[private$id]]$reset_OK <- value},
               GetResetOk = function(){private$rv[[private$id]]$reset_OK},
               
               Analyse_status = function(){},
               
               Init_Default_Positions = function(){
                 private$default_pos <- list(VALIDATED = private$Length(),
                                             SKIPPED = private$Length(),
                                             UNDONE = 1
                 )
               },
               
               NextBtn_logics = function(){
                 # Compute status for the Next button
                 end_of_tl <- private$GetCurrentPos() == private$Length()
                 mandatory_step <- isTRUE(private$GetMandatory()[private$GetCurrentPos()])
                 validated <- private$GetStatus()[private$GetCurrentPos()] == private$global$VALIDATED
                 skipped <- private$GetStatus()[private$GetCurrentPos()] == private$global$SKIPPED
                 entireProcessSkipped <- private$GetStatus()[private$Length()] == private$global$SKIPPED
                 NextBtn_logics <- !end_of_tl && !entireProcessSkipped && (!mandatory_step || (mandatory_step && (validated || skipped)))
                 NextBtn_logics
               },
               
               PrevBtn_logics = function(){
                 # Compute status for the Previous button
                 start_of_tl <- private$GetCurrentPos() == 1
                 entireProcessSkipped <- private$GetStatus()[private$Length()] == private$global$SKIPPED
                 PrevBtn_logics <- !start_of_tl && !entireProcessSkipped
                 PrevBtn_logics
               },
               
               
               CheckConfig = function(){
                 conf <- private$GetConfig()
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
                 req(private$GetStatus())
                 if (private$GetStatus()[private$Length()] == private$global$VALIDATED)
                   private$SetCurrentPos(private$default_pos$VALIDATED)
                 else if (private$GetStatus()[private$Length()] == private$global$SKIPPED)
                   private$SetCurrentPos(private$default_pos$SKIPPED)
                 else if (private$GetStatus()[private$Length()] == private$global$UNDONE)
                   private$SetCurrentPos(private$default_pos$UNDONE)
               },
               
               Display_Current_Step = function(){},
               
               Analyse_Status = function(){},
               # Initialization of the screens by integrating them into a div specific
               # to this module (name prefixed with the ns() function
               # Those div englobs the div of the caller where screens are defined
               EncapsulateScreens = function(){
                 print("encapsulate screens")
                 browser()
                 req(private$GetScreens())
                 ns <- NS(private$id)
                 private$Init_Default_Positions() 
                 private$SetCompleteScreens(setNames(
                   lapply(1:private$Length(),
                        function(x){
                          if (x == 1) 
                            div(id = ns(paste0("div_screen", x)),  private$GetScreens()[[x]])
                            else 
                              shinyjs::hidden(div(id = ns(paste0("div_screen", x)),  private$GetScreens()[[x]]))
                          }),
                   private$GetSteps())
                 )
               }
               
               
  ),
  
  public = list(
    initialize = function(){
                  stop(" TimelineManager is an abstract class that can't be initialized.")
                },
    toggleState_Steps = function(cond, i){
                  lapply(1:i, function(x){
                    shinyjs::toggleState(paste0('div_screen', x), condition = cond)})
                },
    
    # UI
    ui = function() {
      ns <- NS(private$id)
      fluidPage(
        wellPanel(style="background-color: lightblue;",
                  tagList(
                    p('abstract TimelineManager'),
                    uiOutput(ns('title')),
                    shinyjs::useShinyjs(),
                    div(id = 'GlobalTL',
                        fluidRow(
                          align= 'center',
                          column(width=2,div(style = private$btn_style,
                                             uiOutput(ns('showPrevBtn')),
                                             uiOutput(ns('showResetBtn'))
                                             )
                                 ),
                          column(width=8,div( style = private$btn_style,
                                              private$timelineDraw$ui())),
                          column(width=2,div(style = private$btn_style,
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
    
    SetModalTxt = function(txt){private$modal_txt <- txt},
    
    # SERVER
    server = function(config, wake, remoteReset) {
      ns <- NS(private$id)
      
      
      observeEvent(config$steps,{
        print("---------> Catch event on config")
        private$SetCompleteSteps(config$steps)
        })
      
      observeEvent(config$mandatory,{
        print("---------> Catch event on config")
        private$SetCompleteMandatory(config$mandatory)
      })
      
      observeEvent(config$screens,{
        print("---------> Catch event on config")
        private$SetCompleteScreens(config$screens)
      })
      
      observeEvent(req(config$status),{
        print(paste0("Catch event on config$status, new value = ", paste0(config$status, collapse=' ')))
        private$SetCompleteStatus(config$status)
        })
      
      observeEvent(req(wake()),{private$Update_Cursor_position()})
      
      private$timelineDraw$server(
        status = reactive({private$GetStatus()}),
        position = reactive({private$GetCurrentPos()})
        )
      
      
      navPage <- function(direction) {
        newval <- private$GetCurrentPos() + direction
        newval <- max(1, newval)
        newval <- min(newval, private$Length())
        if(newval == 0)
          browser()
        private$SetCurrentPos(newval)
      }
      
      # MODULE SERVER
      moduleServer(private$id, function(input, output, session) {
        ns <- NS(private$id)
        
        # Show modal when button reset is clicked
        observeEvent(input$rstBtn, {showModal(dataModal())})
        
        output$showResetBtn <- renderUI({
          actionButton(ns("rstBtn"), paste0("Reset ", private$type),
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
        
        output$title <- renderUI({ h3(paste0('title=private$id = ', private$id)) })
        
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
        
        # When OK button is pressed, update the reactive value which will be sent
        # to the caller
        observeEvent(req(c(input$modal_ok, remoteReset()!=0)), ignoreInit=T, {
          private$SetResetOk(input$rstBtn)
          private$SetCurrentPos(1)
          removeModal()
          })
        
        
        
        observeEvent(input$prevBtn, ignoreInit = TRUE, {navPage(-1)})
        
        observeEvent(input$nextBtn, ignoreInit = TRUE, {navPage(1)})
        
        output$show_screens <- renderUI({
          #browser()
          tagList(
            print("In show_screens"),
            private$GetScreens()
            )
          })
        
        # Catch a new position or a change in the status list
        observeEvent(req(c(private$GetCurrentPos(), private$GetStatus())), {
          private$Update_Cursor_position()
          private$Analyse_Status()
          private$Display_Current_Step()
          shinyjs::toggleState('prevBtn', cond = private$PrevBtn_logics())
          shinyjs::toggleState('nextBtn', cond = private$NextBtn_logics())
          })
        
        observeEvent(req(private$GetConfig()$steps), ignoreInit=F,{
          print("observeEvent(req(private$GetConfig())")

          req(private$Length()>0)
          check <- private$CheckConfig()
          if (!check$passed)
            stop(paste0("Errors in 'config'", paste0(check$msg, collapse=' ')))
          
          private$EncapsulateScreens()
          })
        
        reactive({
          list(current.pos = private$GetCurrentPos(),
               reset = private$GetResetOk()
               )
          })
      })
      }
  )
)