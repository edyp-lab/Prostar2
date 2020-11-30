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
    ns = NULL,
    style = NULL,
    config = NULL,
    screens = NULL,
    type = NULL,
    rv = "<reactiveValues>",
    length = 0,
    modal_txt = NULL,
    btn_style = "display:inline-block; vertical-align: middle; padding: 7px",
    timelineDraw  = NULL,
    
    #-- Initialize class
    initialize = function(id, config, screens, style = 2) {
      cat(paste0(class(self)[1], '::initialize() from - ', id, '\n'))
      #browser()
      self$id <- id
      self$ns <- NS(id)
      self$config <- config
      self$config$mandatory <- setNames(self$config$mandatory, self$config$steps)
      self$length <- length(self$config$mandatory)
      self$screens <- setNames(self$EncapsulateScreens(screens), self$config$steps)
      self$rv <- reactiveValues(
        current.pos = 1,
        status = c(0,0,0,0),
        reset_OK = NULL,
        isAllSkipped = FALSE,
        isAllUndone = TRUE
      )
      
      self$timelineDraw <- TimelineDraw$new(self$ns('TL_draw'), 
                                            mandatory = self$config$mandatory)
    },
    
    Main_UI = function(){
      cat(paste0(class(self)[1], '::', 'Main_UI() from - ', self$id, '\n'))
      tagList(
        shinyjs::useShinyjs(),
        div(id = self$ns('GlobalTL'),
            fluidRow(
              align= 'center',
              column(width=2, div(style = self$btn_style,
                                  uiOutput(self$ns('showPrevBtn')),
                                  uiOutput(self$ns('showResetBtn'))
              )
              ),
              column(width=8, div( style = self$btn_style,
                                   self$timelineDraw$ui())),
              column(width=2, div(style = self$btn_style,
                                  uiOutput(self$ns('showNextBtn')),
                                  uiOutput(self$ns('showSaveExitBtn'))
              )
              )
            ),
            uiOutput(self$ns('SkippedInfoPanel')),
            uiOutput(self$ns('show_screens'))
            
        )
      )
    },
    
    #--- Get the ui definitions from the config parameter and put them
    #--- in a div to be able to enable/disable all widgets in each screen
    EncapsulateScreens = function(screens){
      #req(self$config)
      cat(paste0(class(self)[1], '::GetScreens() from - ', self$id, '\n'))
      lapply(1:self$length, function(i) {
        if (i==1) div(
          class = paste0("page_", self$id),
          id = self$ns(self$config$steps[i]),
          screens[[i]]
        )
        else 
          shinyjs::hidden(div(
            class = paste0("page_", self$id),
            id = self$ns(self$config$steps[i]),
            screens[[i]]
          )
          )
      }
      )
    },
    
    ToggleState_Screens = function(cond, range){
      ns <- NS(self$id)
      cat(paste0(class(self)[1], '::ToggleState_Steps() from - ', self$id, '\n'))
      #if (verbose==T)  browser()
      lapply(range, function(x){
        shinyjs::toggleState(names(self$screens)[x],
                             condition = cond)
      })
    },
    
    GetFirstMandatoryNotValidated = function(){
      first <- NULL
      first <- unlist((lapply(1:self$length, 
                      function(x){self$config$mandatory[x]&&!self$rv$status[x]})))
      if (sum(first) > 0)
        min(which(first == TRUE))
      else
        NULL
    },
    
    GetMaxValidated_AllSteps = function(){
      cat(paste0(class(self)[1], '::', 'GetMaxValidated_AllSteps() from - ', self$id, '\n'))
      val <- NULL
      ind <- grep(global$VALIDATED, self$rv$status)
      val <- if (length(ind) > 0) max(ind) else NULL
      val
    },
    
    SetModalTxt = function(txt){self$modal_txt <- txt},
    
    # Display_Current_Step = function(){
    #   cat(paste0(class(self)[1], '::Display_Current_Step() from - ', self$id, '\n'))
    #   req(self$rv$current.pos)
    #   toggleState(id = "prevBtn", condition = self$rv$page > 1)
    #   toggleState(id = "nextBtn", condition = self$rv$page < NUM_PAGES)
    #   shinyjs::hide(selector = paste0(".page_", self$id))
    #   shinyjs::show(self$config$steps[self$rv$current.pos])
    # },
    
    #-------------------------------------------------------
    # Return the UI for a modal dialog with data selection input. If 'failed' is
    # TRUE, then display a message that the previous value was invalid.
    dataModal = function() {
      modalDialog(
        span(self$modal_txt),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(self$ns("modal_ok"), "OK")
        )
      )
    },
    
    NavPage = function(direction) {
      newval <- self$rv$current.pos + direction 
      newval <- max(1, newval)
      newval <- min(newval, self$length)
      if(newval == 0)
        browser()
      
      self$rv$current.pos <- newval
      cat(paste0('new position = ', self$rv$current.pos, '\n'))
    },
    
    
    # UI
    ui = function() {},
    
    # SERVER
    server = function(status, dataLoaded) {
      
      
      self$timelineDraw$server(
        status = reactive({self$rv$status}),
        position = reactive({self$rv$current.pos})
      )
      
      
      
      # MODULE SERVER
      moduleServer(self$id, function(input, output, session) {
        
        observe({
          #config()
          cat(paste0(class(self)[1], '::observe() from - ', self$id, '\n'))
          # if (verbose=='skip') 
          #browser()
          self$rv$status <- status()
          self$rv$dataLoaded <- dataLoaded()
          self$rv$isAllSkipped <- sum(rep(global$SKIPPED, self$length)==self$rv$status)==self$length
          self$rv$isAllUndone <- sum(rep(global$UNDONE, self$length)==self$rv$status)==self$length
          
          self$Force_ToggleState_Screens()
        })
        
        observeEvent(req(self$rv$status[self$length] == global$VALIDATED), {
          self$rv$current.pos <- self$length
        })
        
        
        observeEvent(input$rstBtn, {
          cat(paste0(class(self)[1], '::observeEvent(input$rstBtn) from - ', self$id, '\n'))
          showModal(self$dataModal())
        })
        
        # When OK button is pressed, update the reactive value which will be sent
        # to the caller
        observeEvent(req(input$modal_ok), ignoreInit=T,{
          cat(paste0(class(self)[1], '::observeEvent(req(c(input$modal_ok))) from - ', self$id, '\n'))
          self$rv$reset_OK <- input$rstBtn
          self$rv$current.pos <- 1
          removeModal()
        })
        
        output$show_screens <- renderUI({
          req(self$screens)
          tagList(self$screens)
        })
        
        output$showResetBtn <- renderUI({
          actionButton(self$ns("rstBtn"), paste0("Reset entire ", self$type),
                       class = redBtnClass,
                       style='padding:4px; font-size:80%')
        })
        
        output$showPrevBtn <- renderUI({
          shinyjs::disabled(actionButton(self$ns("prevBtn"), "<<",
                       class = PrevNextBtnClass,
                       style='padding:4px; font-size:80%')
          )
        })
        
        
        output$showNextBtn <- renderUI({
          actionButton(self$ns("nextBtn"),
                       ">>",
                       class = PrevNextBtnClass,
                       style='padding:4px; font-size:80%')
        })
        
        output$showUI <- renderUI({
          req(self$screens)
          self$Main_UI()
        })
        
        output$SkippedInfoPanel <- renderUI({
          req(!isTRUE(sum(self$status) == self$global$SKIPPED * self$length))
          req(self$status[self$rv$current.pos] == self$global$SKIPPED)
          wellPanel(
            style = "background-color: #7CC9F0; opacity: 0.72; padding: 0px; align: center; vertical-align: center;",
            height = 100,
            width=300,
            align="center",
            p(style = "color: black;",
              'Info: you skipped this step.')
          )
        })
        
        observeEvent(input$prevBtn, ignoreInit = TRUE, {self$NavPage(-1)})
        observeEvent(input$nextBtn, ignoreInit = TRUE, {self$NavPage(1)})
        
        # Catch a new position
        observe({
          #self$rv$current.pos
          cat(paste0(class(self)[1], '::observeEvent(req(self$rv$current.pos)) from - ', self$id, '\n'))
          if (verbose==T) browser()
          
          #self$Update_Buttons_Status()
          #self$Display_Current_Step()
          shinyjs::toggleState(id = "prevBtn", condition = self$rv$current.pos > 1)
          shinyjs::toggleState(id = "nextBtn", condition = self$rv$current.pos < self$length)
          shinyjs::hide(selector = paste0(".page_", self$id))
          shinyjs::show(self$config$steps[self$rv$current.pos])
        })
        
          list(current.pos = reactive({self$rv$current.pos}),
               tl.reset = reactive({self$rv$reset_OK})
          )
      })
    }
  )
)