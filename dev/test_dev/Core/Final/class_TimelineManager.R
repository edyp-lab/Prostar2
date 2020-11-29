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
      self$ns <- NS(id)
      self$length <- length(mandatory)
      
      self$config <- reactiveValues()
      self$rv <- reactiveValues(
        current.pos = 1,
        reset_OK = NULL,
        isAllSkipped = FALSE
      )
      
      self$timelineDraw <- TimelineDraw$new(self$ns('TL_draw'), 
                                            mandatory = mandatory,
                                            style = style)
    },
    
    Main_UI = function(){
      cat(paste0(class(self)[1], '::', 'Main_UI() from - ', self$id, '\n'))
      tagList(
        uiOutput(ns('show_currentPos')),
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
            shinyjs::disabled(self$GetScreens())
        )
      )
    },
    
    # UI
    ui = function() {},
    
    # SERVER
    server = function(config, dataLoaded) {
      
      # MODULE SERVER
      moduleServer(self$id, function(input, output, session) {
        ns <- NS(self$id)
        
        output$showResetBtn <- renderUI({
          actionButton(self$ns("rstBtn"), paste0("Reset entire ", self$config$type),
                       class = redBtnClass,
                       style='padding:4px; font-size:80%')
        })
        
        output$showPrevBtn <- renderUI({
          actionButton(self$ns("prevBtn"), "<<",
                       class = PrevNextBtnClass,
                       style='padding:4px; font-size:80%')
        })
        
        
        output$showNextBtn <- renderUI({
          actionButton(self$ns("nextBtn"),
                       "next",
                       class = PrevNextBtnClass,
                       style='padding:4px; font-size:80%')
        })
        
        output$showUI <- renderUI({
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
        
          list(current.pos = reactive({self$rv$current.pos}),
               tl.reset = reactive({self$rv$reset_OK})
          )
      })
    }
  )
)