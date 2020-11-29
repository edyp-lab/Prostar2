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
    name = NULL,
    mandatory = NULL,
    screens = NULL,
    type = NULL,
    steps = NULL,
    rv = "<reactiveValues>",
    length = 0,
    modal_txt = NULL,
    btn_style = "display:inline-block; vertical-align: middle; padding: 7px",
    timelineDraw  = NULL,
    
    #-- Initialize class
    initialize = function(id, name, steps, mandatory, screens, style = 2) {
      cat(paste0(class(self)[1], '::initialize() from - ', id, '\n'))
      #browser()
      self$id <- id
      self$ns <- NS(id)
      self$length <- length(mandatory)
      self$name <- name
      self$steps <- steps
      self$screens <- self$EncapsulateScreens(screens)
      self$rv <- reactiveValues(
        current.pos = 1,
        status = NULL,
        reset_OK = NULL,
        isAllSkipped = FALSE
      )
      
      self$timelineDraw <- TimelineDraw$new(self$ns('TL_draw'), 
                                            mandatory = self$mandatory,
                                            style = style)
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
          id = self$ns(self$steps[i]),
          screens[[i]]
        )
        else 
          shinyjs::hidden(div(
            class = paste0("page_", self$id),
            id = self$ns(self$steps[i]),
            screens[[i]]
          )
          )
      }
      )
    },
    
    # Display_Current_Step = function(){
    #   cat(paste0(class(self)[1], '::Display_Current_Step() from - ', self$id, '\n'))
    #   req(self$rv$current.pos)
    #   toggleState(id = "prevBtn", condition = self$rv$page > 1)
    #   toggleState(id = "nextBtn", condition = self$rv$page < NUM_PAGES)
    #   shinyjs::hide(selector = paste0(".page_", self$id))
    #   shinyjs::show(self$steps[self$rv$current.pos])
    # },
    
    
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
    server = function(status) {
      
      observe({
        #config()
        cat(paste0(class(self)[1], '::observeEvent(status) from - ', self$id, '\n'))
       # if (verbose=='skip') 
         self$rv$status <- status()
      })
      
      
      
      
      
      
      # MODULE SERVER
      moduleServer(self$id, function(input, output, session) {
        
        
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
          shinyjs::show(self$steps[self$rv$current.pos])
        })
        
          list(current.pos = reactive({self$rv$current.pos}),
               tl.reset = reactive({self$rv$reset_OK})
          )
      })
    }
  )
)