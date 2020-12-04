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
    default_pos = list(VALIDATED = 1,
                       SKIPPED = 1,
                       UNDONE = 1),
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
      
      self$default_pos$VALIDATED <- self$length
      self$default_pos$SKIPPED <- 1
      self$default_pos$UNDONE <- 1
      
      self$rv <- reactiveValues(
        current.pos = 1,
        status = setNames(rep(global$UNDONE, self$length), self$config$steps),
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
        tags$head(tags$style("#modal1 .modal-body {padding: 10px}
                       #modal1 .modal-content  {-webkit-border-radius: 6px !important;-moz-border-radius: 6px !important;border-radius: 6px !important;}
                       #modal1 .modal-dialog { width: 240px; display: inline-block; text-align: left; vertical-align: top;}
                       #modal1 .modal-header {background-color: #339FFF; border-top-left-radius: 6px; border-top-right-radius: 6px}
                       #modal1 .modal { text-align: right; padding-right:10px; padding-top: 24px;}
                       #moda1 .close { font-size: 16px}")),
        div(id = self$ns('GlobalTL'),
            fluidRow(
              align= 'center',
              column(width=2, div(style = self$btn_style,
                                  shinyjs::disabled(actionButton(self$ns("prevBtn"), "<<",
                                                                 class = PrevNextBtnClass,
                                                                 style='padding:4px; font-size:80%')),
                                  shinyjs::disabled(actionButton(self$ns("rstBtn"), paste0("Reset entire ", self$type),
                                               class = redBtnClass,
                                               style='padding:4px; font-size:80%'))
              )
              ),
              column(width=8, div( style = self$btn_style,
                                   self$timelineDraw$ui())),
              column(width=2, div(style = self$btn_style,
                                  shinyjs::disabled(actionButton(self$ns("nextBtn"),
                                               ">>",
                                               class = PrevNextBtnClass,
                                               style='padding:4px; font-size:80%'))
              )
              )
            ),

            uiOutput(self$ns('SkippedInfoPanel')),
            self$screens
        )
      )
    },


    EncapsulateScreens = function(screens){
      req(screens)
      cat(paste0(class(self)[1], '::GetScreens() from - ', self$id, '\n'))
      lapply(1:self$length, function(i) {
        if (i==1) shinyjs::disabled(
          div(
            class = paste0("page_", self$id),
            id = self$ns(self$config$steps[i]),
            screens[[i]])
        )
        else 
          shinyjs::hidden(
            shinyjs::disabled(
              div(
            class = paste0("page_", self$id),
            id = self$ns(self$config$steps[i]),
            screens[[i]])
            )
          )
      }
      )
    },
    


    ToggleState_Screens = function(cond, range){
      cat(paste0(class(self)[1], '::ToggleState_Steps() from - ', self$id, '\n'))
      #if (verbose==T)  
      #browser()
      lapply(range, function(x){
        shinyjs::toggleState(self$config$steps[x],
                             condition = cond)
      })
    },
    
    Update_Cursor_Position = function(){
      cat(paste0(class(self)[1], '::Update_Cursor_position() from - ', self$id, '\n'))
      if (verbose==T) browser()
      req(self$rv$status)
      if (self$rv$status[self$length] == global$VALIDATED)
        self$rv$current.pos <- self$default_pos$VALIDATED
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

    #-------------------------------------------------------
    # Return the UI for a modal dialog with data selection input. If 'failed' is
    # TRUE, then display a message that the previous value was invalid.
    dataModal = function() {
      
      tags$div(id="modal1", 
               modalDialog(
                 span(self$modal_txt),
                 footer = tagList(
                   actionButton(self$ns("close"), "Cancel", class='btn-info'),
                   actionButton(self$ns("modal_ok"), "OK")
                   )
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
    server = function(status, 
                      dataLoaded=reactive({FALSE}), 
                      remoteReset=reactive({FALSE})) {

      self$timelineDraw$server(
        status = reactive({self$rv$status}),
        position = reactive({self$rv$current.pos}),
        dataLoaded = reactive({self$rv$dataLoaded})
      )

      
      # MODULE SERVER
      moduleServer(self$id, function(input, output, session) {
        
        observe({
          cat(paste0(class(self)[1], '::observe() from - ', self$id, '\n'))
          # if (verbose=='skip') 
          #browser()
          self$rv$status <- status()
          self$rv$dataLoaded <- dataLoaded()
          self$rv$isAllSkipped <- sum(rep(global$SKIPPED, self$length)==self$rv$status)==self$length
          self$rv$isAllUndone <- sum(rep(global$UNDONE, self$length)==self$rv$status)==self$length
          
          self$Force_ToggleState_Screens()
          
          shinyjs::toggleState(id = "rstBtn", condition = self$rv$dataLoaded)
          shinyjs::toggleState(id = "prevBtn", condition = self$rv$dataLoaded && self$rv$current.pos > 1)
          shinyjs::toggleState(id = "nextBtn", condition = self$rv$dataLoaded && self$rv$current.pos < self$length)
          shinyjs::hide(selector = paste0(".page_", self$id))
          shinyjs::show(self$config$steps[self$rv$current.pos])
        })

        
        observeEvent(req(self$rv$status[self$length] == global$VALIDATED), {
          self$rv$current.pos <- self$length
        })
        
        # observeEvent(self$rv$status, {
        #   self$Update_Cursor_Position()
        #   
        # })
        
        observeEvent(remoteReset(), {self$rv$current.pos <- 1 })
        
        observeEvent(input$rstBtn, {
          cat(paste0(class(self)[1], '::observeEvent(input$rstBtn) from - ', self$id, '\n'))
          showModal(self$dataModal())
        })
        
        observeEvent(input$close, {removeModal() })
        
        # When OK button is pressed, update the reactive value which will be sent
        # to the caller
        observeEvent(req(input$modal_ok > 0), ignoreInit=F,{
          cat(paste0(class(self)[1], '::observeEvent(req(c(input$modal_ok))) from - ', self$id, '\n'))
          self$rv$reset_OK <- input$rstBtn
          self$rv$current.pos <- 1
          removeModal()
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
        
        
        
          list(current.pos = reactive({self$rv$current.pos}),
               tl.reset = reactive({self$rv$reset_OK})
          )
      })
    }
  )
)