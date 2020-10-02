

mod_wf_wf1_B_ui <- function(id){
  ns <- NS(id)
  tagList(
    useShinyjs(),
    mod_timeline_ui(ns("timeline")),
    #uiOutput(ns('show_screens')),
    hr(),
    wellPanel(
      h3('Module B'),
      fluidRow(
        column(width=2,
               tags$b(h4(style = 'color: blue;', "Data input")),
               uiOutput(ns('show_dataIn')) ),
        column(width=2,
               tags$b(h4(style = 'color: blue;', "Data output")),
               uiOutput(ns('show_rv_dataOut'))),
        column(width=2,
               tags$b(h4(style = 'color: blue;', "Current pos")),
               uiOutput(ns('show_currentPos'))),
        column(width=2,
               tags$b(h4(style = 'color: blue;', "List 'isDone'")),
               uiOutput(ns('show_isDone')))
      )
    )
  )
}

#' @param dataIn xxx
#'
#' 
#' 
mod_wf_wf1_B_server <- function(id, 
                                dataIn=NULL,
                                remoteReset=FALSE, 
                                forcePosition = NULL){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
      
      output$show_currentPos <- renderUI({
        req(rv$current.pos)
        p(as.character(rv$current.pos))
      })
      output$show_dataIn <- renderUI({
        tagList(lapply(names(dataIn()), function(x){tags$p(x)}))
      })
      output$show_rv_dataIn <- renderPrint({names(rv$dataIn)})
      output$show_rv_dataOut <- renderUI({
        tagList(
          lapply(names(rv$dataOut), function(x){tags$p(x)})
        )
      })
      output$show_isDone <- renderUI({
        config$isDone <- setNames(config$isDone, config$stepsNames)
        tagList(lapply(names(config$isDone), 
                       function(x){tags$p(paste0(x, ' - ', config$isDone[[x]]))}))
      })
      #################################################################################
      
      rv <- reactiveValues(
        current.pos = 1,
        timeline = NULL)
      
      actions <- reactiveValues(
        btns = list(
          rst = TRUE,
          nxt = TRUE,
          prv = TRUE),
        screens = NULL)
      
      config <- reactiveValues(
        type = 'process',
        name = 'Normalization',
        stepsNames = c("Description", "Step 1", "Step 2", "Step 3"),
        isDone =  c(TRUE, FALSE, FALSE, FALSE),
        mandatory =  c(FALSE, TRUE, FALSE, TRUE)
      )
      
      #################################################################################
      
      
      #--------------------------------------------------------------
      observeEvent(req(dataIn()), { 
        print(' ------- MODULE B : Initialisation du module B ------- ')
        rv$dataIn <- dataIn()
        
        actions$screens <- InitActions(nbSteps())
        config$screens <- CreateScreens(nbSteps())
        
        rv$timeline <- mod_timeline_server("timeline", 
                                           style = 2, 
                                           config = config, 
                                           actions = reactive({actions}),
                                           position = reactive({rv$current.pos})
        )
      })
      
      
      InitActions <- function(n){
        setNames(lapply(1:n,
                        function(x){T}),
                 paste0('screen', 1:n)
        )
      }
      
      CreateScreens <- function(n){
        setNames(
          lapply(1:n, 
                 function(x){
                   do.call(uiOutput, list(outputId=ns(paste0("screen", x))))}),
          paste0('screenStep', 1:n))
      }
      
      nbSteps <- reactive({
        req(config$stepsNames)
        length(config$stepsNames)
      })
      
      
      DisableAllPrevSteps <- function(screens){
        pos <- max(grep(TRUE, config$isDone))
        lapply(1:pos, function(x) screens[[x]] <- FALSE)
      }
      
      DisableAllSteps <- function(screens){
        lapply(screens, function(x) x <- FALSE)
      }
      
      EnableAllSteps <- function(screens){
        lapply(screens, function(x) x <- TRUE)
      }
      
      
      ResetScreens <- function(screens){
        lapply(1:nbSteps(), function(x){
          shinyjs::reset(paste0('screen', x))
        })
      }
      
      
      ResetActionBtns <- function(btns){lapply(btns, function(x){x <- T})}
      
      
      #Catch a new position from timeline
      observeEvent(req(rv$timeline$pos()),{ rv$current.pos <- rv$timeline$pos()})
      
      #--------------------------------------------------------------
      observeEvent(req(c(rv$timeline$rstBtn()!=0, remoteReset()!=0)), {
        print("---- MODULE B : reset activated")
        
        actions$screens <- EnableAllSteps(actions$screens)
        ResetScreens()
        
        config$isDone <- c(TRUE, rep(FALSE, nbSteps()-1))
        actions$btns <- ResetActionBtns(actions$btns)
        rv$current.pos <- 1
        
        # Update datasets
        rv$dataIn <- RemoveItemFromDataset(dataIn(), config$name)
        rv$dataOut <- NULL
      })
      
      
      
      # Catch a change in isDone (validation of a step)
      observeEvent(config$isDone,  ignoreInit = T, {
        print(' ------- MODULE B : A new step is validated ------- ')
        actions$screens <- DisableAllPrevSteps(actions$screens)
      })
      
      #####################################################################
      ## screens of the module
      ##
      ############### SCREEN 1 ######################################
      output$screen1 <- renderUI({
        tagList(
          tags$h3(paste0('Process ', config$name))
        )
      })
      
      ############### SCREEN 2 ######################################
      
      output$screen2 <- renderUI({
        
        observeEvent(input$perform_screen2_btn, {
          config$isDone[2] <- TRUE
        })
        
        tagList(
          div(id=ns('screen2'),
              div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  tags$h2('Step 1')),
              div(style="display:inline-block; vertical-align: middle; padding-right: 40px;",
                  selectInput(ns('select1'), 'Select step 1', 
                              choices = 1:5, 
                              selected = 1,
                              width = '150px')
              ),
              div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  actionButton(ns('perform_screen2_btn'), 'Perform'))
              
          )
        )
      })
      
      
      
      
      ############### SCREEN 3 ######################################
      output$screen3 <- renderUI({
        
        tagList(
          div(id=ns('screen3'),
              div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  tags$h3('Step 2')),
              div(style="display:inline-block; vertical-align: middle;padding-right: 40px;",
                  selectInput(ns('select2'), 'Select step 2',
                              choices = 1:5,
                              selected = 1,
                              width = '150px')),
              div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  actionButton(ns('perform_screen3_btn'), 'Perform'))
          )
        )
      })
      
      ## Logics to implement: here, we must take the last data not null
      # in previous datas. The objective is to take account
      # of skipped steps
      observeEvent(input$perform_screen3_btn, {
        config$isDone[3] <- TRUE
      })
      
      
      ############### SCREEN 4 ######################################
      output$screen4 <- renderUI({
        
        tagList(
          div(id=ns('screen4'),
              div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  tags$h3('Step 4')),
              div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  actionButton(ns('validate_btn'), 'Validate'))
          )
        )
      })
      
      observeEvent(input$validate_btn, {
        #isolate({
        rv$dataIn <- AddItemToDataset(rv$dataIn, config$name)
        rv$dataOut <- rv$dataIn
        config$isDone[4] <- TRUE
        # })
      })
      
      
      
      
      
      
      ##########################################################
      
      reactive({rv$dataOut})
    }
  )
}

