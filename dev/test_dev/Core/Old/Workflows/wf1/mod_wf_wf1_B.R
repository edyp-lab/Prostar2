

mod_wf_wf1_B_ui <- function(id){
  ns <- NS(id)
  tagList(
    useShinyjs(),
    mod_timeline_ui(ns("timeline")),
    uiOutput(ns('show_screens')),
    hr(),
    wellPanel(
      h3('Module B'),
      p('dataIn() :'),
      verbatimTextOutput(ns('show_dataIn')),
      p('rv$dataIn :'),
      verbatimTextOutput(ns('show_rv_dataIn')),
      p('rv$dataOut'),
      verbatimTextOutput(ns('show_rv_dataOut'))
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
      source(file.path('.', 'code_general.R'), local=TRUE)$value
      
      rv <- reactiveValues()
      
      
      # variables to communicate with the navigation module
      rv.process_config <- reactiveValues(
        type = 'process',
        process.name = 'Normalization',
        stepsNames = c("Description", "Step 1", "Step 2", "Step 3"),
        isDone =  c(TRUE, FALSE, FALSE, FALSE),
        mandatory =  c(FALSE, FALSE, TRUE,TRUE)
      )
      
      
      tl.update <- reactiveValues(
        current.pos = 1,
        actions = list(rst = TRUE,
                       nxt = TRUE,
                       prv = TRUE, 
                       skip = TRUE)
      )
      
      pos <- mod_timeline_server("timeline", 
                                 style = 2, 
                                 process_config = rv.process_config, 
                                 tl.update = tl.update,
                                 showSkip = TRUE)
      
      
      # Initialization of the process
      observeEvent(req(dataIn()), { 
        print(' ------- MODULE B : Initialisation du module B ------- ')
        
        rv$dataIn <- dataIn()
        rv$process.validated <- rv.process_config$isDone[length(rv.process_config$isDone)]
        rv$skip = 0
        
        CreateScreens()
        InitScreens()
        
        if (isTRUE(rv$process.validated)){
          tl.update$current.pos <-  length(rv.process_config$isDone)}
        else {
          tl.update$current.pos <- 1
        }
        
        tl.update$actions$nxt <- condNextBtn() && rv$skip == 0
        tl.update$actions$nxt <- condPrevBtn() && rv$skip == 0
      })
      
      
      observeEvent(req(c(pos$rstBtn()!=0, remoteReset()!=0)), {
        print('MODULE B : RESET du module B')
        ReinitScreens()
        
        rv.process_config$isDone <- c(TRUE, rep(FALSE, length(rv.process_config$stepsNames)-1))
        tl.update$current.pos <- 1
        rv$skip <- 0
        tl.update$actions$skip <- TRUE
        tl.update$actions$nxt <- TRUE
        tl.update$actions$prv <- TRUE
        
        if (!rv.process_config$isDone[length(rv.process_config$isDone)]){
          rv$dataIn <- dataIn()
          rv$dataOut <- NULL
        }
        
      })
      
      
      
      observeEvent(req(rv.process_config$isDone[tl.update$current.pos]),  ignoreInit = T, {
        rv.process_config$mandatory
        if (rv.process_config$isDone[tl.update$current.pos])
          DisableAllPrevSteps()
        
        tl.update$actions$nxt <- condNextBtn() && rv$skip == 0
        tl.update$actions$nxt <- condPrevBtn() && rv$skip == 0
      })
      
      
      observeEvent(tl.update$current.pos,  ignoreInit = T, {
        DisplayCurrentStep()
        tl.update$actions$nxt <- condNextBtn() && rv$skip == 0
        tl.update$actions$nxt <- condPrevBtn() && rv$skip == 0
      })
      
      
      observeEvent(rv.process_config$isDone[length(rv.process_config$isDone)], {
        rv$process.validated <- rv.process_config$isDone[length(rv.process_config$isDone)]
      })
      
      
      observeEvent(req(forcePosition() != 0), ignoreNULL=T, {
        rv$forcePosition <- forcePosition()})
      
      observeEvent(req(rv$forcePosition),   {
        tl.update$current.pos <- length(rv.process_config$isDone)
      })
      
      
      observeEvent(req(pos$skipBtn() != 0), ignoreNULL=T, {
        rv$skip <- pos$skipBtn()
      })
      
      
      
      #####################################################################
      ## screens of the module
      ##
      ############### SCREEN 1 ######################################
      output$screen1 <- renderUI({
        tagList(
          tags$h3(paste0('Process ', rv.process_config$name))
        )
      })
      
      
      ############### SCREEN 2 ######################################
      
      output$screen2 <- renderUI({
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
      
      observeEvent(input$perform_screen2_btn, {
        rv.process_config$isDone[2] <- TRUE
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
        rv.process_config$isDone[3] <- TRUE
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
        isolate({
          rv$dataIn <- addAssay(rv$dataIn, 
                                rv$dataIn[[length(rv$dataIn)]], 
                                name=rv.process_config$process.name)
          rv$dataOut <- rv$dataIn
          rv.process_config$isDone[4] <- TRUE
        })
      })
      
      
      observeEvent(req(pos$skipBtn() !=0),{
        print(paste0('MODULE B : Skip button activated : ', pos$skipBtn()))
        tl.update$current.pos <- length(rv.process_config$isDone)
        rv.process_config$isDone[length(rv.process_config$isDone)] <- TRUE
        tl.update$actions$skip <- FALSE
        rv$dataOut <- dataIn()
      })
      
      
      
      ##########################################################
      
      list(dataOut = reactive({rv$dataOut}),
           validated = reactive({rv.process_config$isDone[length(rv.process_config$isDone)]}),
           reseted = reactive({pos$rstBtn()})
      )
    }
  )
}

