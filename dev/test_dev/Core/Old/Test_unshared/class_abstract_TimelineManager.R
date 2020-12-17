redBtnClass <- "btn-danger"
PrevNextBtnClass <- "btn-info"
btn_success_color <- "btn-success"
optionsBtnClass <- "info"

btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"

TimelineManager <- R6Class(
  "TimelineManager",
  private=list( ),
  
  public = list(
    initialize = function(){
      stop(" TimelineManager is an abstract class that can't be initialized.")
    },
    
    id = NULL,
    style = 2,
    config = "<reactiveValues>",
    rv = "<reactiveValues>",
    
    global = list(VALIDATED = 1,
                  SKIPPED = -1,
                  UNDONE = 0
    ),
    default_pos =list(VALIDATED = 1,
                      SKIPPED = 1,
                      UNDONE = 1
    ), 
    nbSteps = 0,
    modal_txt = NULL,
    btn_style = "display:inline-block; vertical-align: middle; padding: 7px",
    timelineDraw  = NULL,
    
    
    Force_ToggleState_Steps = function(){},
    
    Init_Default_Positions = function(){
      cat(paste0(class(self)[1], '::Init_Default_Positions() from - ', self$id, '\n'))
      self$default_pos <- list(VALIDATED = self$nbSteps,
                               SKIPPED = self$nbSteps,
                               UNDONE = 1
      )
    },
    
    NextBtn_logics = function(){
      cat(paste0(class(self)[1], '::NextBtn_logics() from - ', self$id, '\n'))
      # Compute status for the Next button
      end_of_tl <- self$rv$current.pos == self$nbSteps
      mandatory_step <- isTRUE(self$config$mandatory[self$rv$current.pos])
      validated <- self$config$status[self$rv$current.pos] == self$global$VALIDATED
      skipped <- self$config$status[self$rv$current.pos] == self$global$SKIPPED
      NextBtn_logics <- !end_of_tl && (!mandatory_step || (mandatory_step && (validated || skipped)))
      NextBtn_logics
    },
    
    PrevBtn_logics = function(){
      cat(paste0(class(self)[1], '::PrevBtn_logics() from - ', self$id, '\n'))
      # Compute status for the Previous button
      start_of_tl <- self$rv$current.pos == 1
      PrevBtn_logics <- !start_of_tl 
      PrevBtn_logics
    },
    
  
    
    Update_Cursor_position = function(){
      cat(paste0(class(self)[1], '::Update_Cursor_position() from - ', self$id, '\n'))
      if (verbose==T) browser()
      req(self$config$status)
      if (self$config$status[self$nbSteps] == self$global$VALIDATED)
        self$rv$current.pos <- self$default_pos$VALIDATED
      else if (self$rv$isAllSkipped)
        self$rv$current.pos <- self$default_pos$SKIPPED
     # else if (self$config$status[self$nbSteps] == self$global$UNDONE)
     #   self$rv$current.pos <- self$default_pos$UNDONE
    },
 
    Display_Current_Step = function(){
      cat(paste0(class(self)[1], '::Display_Current_Step() from - ', self$id, '\n'))
      req(c(self$nbSteps, self$rv$current.pos))
      
      shinyjs::hide(selector = paste0(".page_", self$id))
      shinyjs::show(names(self$config$screens)[self$rv$current.pos])
    },
    

    GetScreens = function(){
      cat(paste0(class(self)[1], '::GetScreens() from - ', self$id, '\n'))
      ns <- NS(self$id)
      
      lapply(1:self$nbSteps, function(i) {
        if (i==1) div(
          class = paste0("page_", self$id),
          id = ns(names(self$config$screens)[i]),
          self$config$screens[[i]]
        )
        else 
          shinyjs::hidden(div(
            class = paste0("page_", self$id),
            id = ns(names(self$config$screens)[i]),
            self$config$screens[[i]]
          )
          )
      }
      )
    },
    
    ToggleState_Steps = function(cond, i){
      ns <- NS(self$id)
      cat(paste0(class(self)[1], '::ToggleState_Steps() from - ', self$id, '\n'))
      #if (verbose==T)  browser()
      lapply(1:i, function(x){
        shinyjs::toggleState(names(self$config$screens)[x],
                             condition = cond)
        })
    },
    
    Update_Buttons_Status = function(){
      if (verbose==T) browser()
      shinyjs::toggleState('prevBtn', cond = self$PrevBtn_logics())
      shinyjs::toggleState('nextBtn', cond = self$NextBtn_logics())
    },
    
    Main_UI = function(){
      cat(paste0(class(self)[1], '::', 'Main_UI() from - ', self$id, '\n'))
      ns <- NS(self$id)
      tagList(
        uiOutput(ns('show_currentPos')),
        shinyjs::useShinyjs(),
        div(id = ns('GlobalTL'),
            fluidRow(
              align= 'center',
              column(width=2, div(style = self$btn_style,
                                  uiOutput(ns('showPrevBtn')),
                                  uiOutput(ns('showResetBtn'))
              )
              ),
              column(width=8, div( style = self$btn_style,
                                   self$timelineDraw$ui())),
              column(width=2, div(style = self$btn_style,
                                  uiOutput(ns('showNextBtn')),
                                  uiOutput(ns('showSaveExitBtn'))
              )
              )
            ),
            uiOutput(ns('SkippedInfoPanel')),
            self$GetScreens()
        )
      )
    },
    
    GetMaxValidated_AllSteps = function(){
      cat(paste0(class(self)[1], '::', 'GetMaxValidated_AllSteps() from - ', self$id, '\n'))
      val <- 0
      ind <- which(self$config$status == self$global$VALIDATED)
      if (length(ind) > 0)
        val <- max(ind)
      val
    },
    
    # UI
    ui = function() {},
    
    SetModalTxt = function(txt){self$modal_txt <- txt},
    
    # SERVER
    server = function(config) {
      ns <- NS(self$id)
      cat(paste0(class(self)[1], '::server() from - ', self$id, '\n'))
      #browser()
      
      observeEvent(config(), {
        cat(paste0(class(self)[1], '::observeEvent(config) from - ', self$id, '\n'))
        if (verbose=='skip') browser()
        self$config <- config()
        req(self$nbSteps>0)
        self$Update_Buttons_Status()
        browser()
      })
      
      cat(paste0(class(self)[1], '::self$timelineDraw$server() from - ', self$id, '\n'))

      self$timelineDraw$server(
        status = reactive({self$config$status}),
        position = reactive({self$rv$current.pos})
      )
      
      # MODULE SERVER
      moduleServer(self$id, function(input, output, session) {
        ns <- NS(self$id)
        
        cat(paste0(class(self)[1], '::moduleServer() from - ', self$id, '\n'))
        # Show modal when button reset is clicked
        observeEvent(input$rstBtn, {
          cat(paste0(class(self)[1], '::observeEvent(input$rstBtn) from - ', self$id, '\n'))
          showModal(dataModal())
          })
        
        ###############################
        output$showResetBtn <- renderUI({
          actionButton(ns("rstBtn"), paste0("Reset entire ", self$config$type),
                       class = redBtnClass,
                       style='padding:4px; font-size:80%')
        })
        
        output$showPrevBtn <- renderUI({
          cond <- self$PrevBtn_logics()
          if(!cond)
            shinyjs::disabled(actionButton(ns("prevBtn"), "<<",
                                         class = PrevNextBtnClass,
                                         style='padding:4px; font-size:80%'))
          else
            actionButton(ns("prevBtn"), "<<",
                         class = PrevNextBtnClass,
                         style='padding:4px; font-size:80%')
        })

        
        output$showNextBtn <- renderUI({
           cond <- self$NextBtn_logics()
            if(!cond)
            shinyjs::disabled(
            actionButton(ns("nextBtn"), 
                         "next",
                         class = PrevNextBtnClass,
                         style='padding:4px; font-size:80%')
            )
          else
            actionButton(ns("nextBtn"),
                         "next",
                         class = PrevNextBtnClass,
                         style='padding:4px; font-size:80%')
        })
        
        
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
          cat(paste0(class(self)[1], '::observeEvent(req(c(input$modal_ok))) from - ', self$id, '\n'))
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
        
        output$toto <- renderUI({
          req(self$config$screens)
          self$Main_UI()
        })
        
        output$SkippedInfoPanel <- renderUI({
          req(!isTRUE(sum(self$config$status) == self$global$SKIPPED * self$nbSteps))
          req(self$config$status[self$rv$current.pos] == self$global$SKIPPED)
          wellPanel(
            style = "background-color: #7CC9F0; opacity: 0.72; padding: 0px; align: center; vertical-align: center;",
            height = 100,
            width=300,
            align="center",
            p(style = "color: black;",
              'Info: you skipped this step.')
          )
        })
        
        # Catch a new position
        observeEvent(req(self$rv$current.pos), ignoreInit=T, {
           cat(paste0(class(self)[1], '::observeEvent(req(self$rv$current.pos)) from - ', self$id, '\n'))
          if (verbose==T) browser()
          
          self$Update_Buttons_Status()
          self$Display_Current_Step()
        })
        
        observeEvent(req(self$config$status), {
          cat(paste0(class(self)[1], '::observeEvent(req(self$config$status)) from - ', self$id, '\n'))
          if (verbose==TRUE) browser()
          self$rv$isAllSkipped <- sum(rep(self$global$SKIPPED, self$nbSteps)==self$config$status)==self$nbSteps
          self$rv$isAllUndone <- sum(rep(self$global$UNDONE, self$nbSteps)==self$config$status)==self$nbSteps
          
          self$Force_ToggleState_Steps()
          self$Update_Buttons_Status()
        })
        

          list(current.pos = reactive({self$rv$current.pos}),
               tl.reset = reactive({self$rv$reset_OK})
          )
      })
    }
  )
)