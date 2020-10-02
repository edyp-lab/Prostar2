

mod_wf_wf1_A_ui <- function(id){
  ns <- NS(id)
  tagList(
    useShinyjs(),
    mod_timeline_ui(ns("timeline")),
    #uiOutput(ns('show_screens')),
    hr(),
    wellPanel(
      h3('Module A'),
      fluidRow(
        column(width=6,
               p('Data input :'),
               verbatimTextOutput(ns('show_dataIn'))
        ),
        column(width=6,
               p('Data output :'),
               verbatimTextOutput(ns('show_rv_dataOut')))
      )
    )
  )
}

#' @param dataIn xxx
#'
#' 
#' 
mod_wf_wf1_A_server <- function(id, 
                                dataIn=NULL,
                                remoteReset=FALSE, 
                                forcePosition = NULL){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
      
      ##########################################################################
      
      
      
      
      
      
      
      
      output$show_dataIn <- renderPrint({names(dataIn())})
      output$show_rv_dataIn <- renderPrint({names(rv$dataIn)})
      output$show_rv_dataOut <- renderPrint({names(rv$dataOut)})
      
      
      condNextBtn <- reactive({
        
        # # Conditional enabling of the next button
        end_of_tl <- rv$current.pos == length(config$stepsNames)
        mandatory_step <- isTRUE(config$mandatory[rv$current.pos])
        validated <- isTRUE(config$isDone[rv$current.pos])
        cond.next.btn <-  !mandatory_step || validated
        cond.next.btn
      })
      
      condPrevBtn <- reactive({
        start_of_tl <- rv$current.pos == 1
        cond.prev.btn <- !start_of_tl
        cond.prev.btn
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
          screens = NULL,
          position = 1
          )

      
      
      config <- reactiveValues(
        type = 'process',
        name = 'Filtering',
        stepsNames = c("Description", "Step 1", "Step 2", "Step 3"),
        isDone =  c(TRUE, FALSE, FALSE, FALSE),
        mandatory =  c(FALSE, FALSE, TRUE,TRUE)
        )

      observeEvent(req(rv$timeline$position()),{ 
        print(paste0('--- MODULE A : new position = ', rv$timeline$position()))
        rv$current.pos <- rv$timeline$position()
        })
      
      #--------------------------------------------------------------
      observeEvent(req(dataIn()), { 
        print(' ------- MODULE A : Initialisation du module A ------- ')
        rv$dataIn <- dataIn()
        
        InitActions(nbSteps())
        config$screens <- CreateScreens(nbSteps())
        rv$timeline <- mod_timeline_server("timeline", 
                                   style = 2, 
                                   config = config, 
                                   actions = actions,
                                   position = rv$current.pos
                                   )
      })
      
      
      InitActions <- function(n){
        actions$screens <- setNames(lapply(1:n,
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
      
      
      DisableAllPrevSteps <- reactive({
        pos <- max(grep(TRUE, config$isDone))
        lapply(1:pos, function(x) actions$screens[[x]] <- FALSE)
      })
      
      DisableAllSteps <- reactive({
        lapply(actions$screens, function(x) x <- FALSE)
      })
      
      EnableAllSteps <- function(){
        lapply(actions$screens, function(x) x <- TRUE)
      }
      
      
      ResetScreens <- function(){
        EnableAllSteps()
        
        lapply(1:nbSteps(), function(x){
          shinyjs::reset(paste0('screen', x))
        })
      }
      
      
      ResetActionBtns <- function(){
        lapply(actions$btns, function(x){x <- T})
        }
      
      #--------------------------------------------------------------
      observeEvent(req(config), { 
        print(' ------- MODULE A : Initialisation de la configuration A ------- ')
        
        rv$isValidated <- config$isDone[nbSteps()]
        
        #CreateScreens()
        #InitScreens()
        print(rv$screens)
        if (isTRUE(rv$isValidated))
          rv$current.pos <-  nbSteps()
        else 
          rv$current.pos <- 1

        # actions$nxt <- condNextBtn()
        # actions$prv <- condPrevBtn() 
      })
      
      
      #--------------------------------------------------------------
      observeEvent(req(c(rv$timeline$rstBtn()!=0, remoteReset()!=0)), {
        print("---- MODULE A : reset activated")
        ResetScreens()
        config$isDone <- c(TRUE, rep(FALSE, nbSteps()-1))
        ResetActionBtns()
        rv$current.pos <- 1
        rv$dataIn <- RemoveItemFromDataset(dataIn(), config$name)
        rv$dataOut <- NULL
      })
      

      
      observeEvent(config$isDone,  ignoreInit = T, {
        print(' ------- MODULE A : A new step is validated ------- ')
        
        DisableAllPrevSteps()
        rv$isValidated <- config$isDone[nbSteps()]
        # actions$nxt <- condNextBtn()
        # actions$prv <- condPrevBtn()
      })
      
     
      observeEvent(rv$current.pos,  ignoreInit = T, {
        DisplayCurrentStep()
        # actions$nxt <- condNextBtn() 
        # actions$prv <- condPrevBtn() 
      })
      

      
      observeEvent(req(forcePosition() != 0), ignoreNULL=T, { rv$forcePosition <- forcePosition()})
      observeEvent(req(rv$forcePosition), { rv$current.pos <- nbSteps() })
      

      
       #####################################################################
       ## screens of the module
       ##
       ############### SCREEN 1 ######################################
       output$screen1 <- renderUI({
         tagList(
           tags$h3(paste0('Process ', config$name)),
           actionButton(ns('rst'), 'reset'),
           actionButton(ns('screen'), 'screen'),
           actionButton(ns('prev'), 'prev'),
           actionButton(ns('next'), 'next')
         )
       })
       
      observeEvent(input$screen, { actions$screens[[3]] <- !actions$screens[[3]]})
       
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
        
  list(dataOut = reactive({rv$dataOut}),
       validated = reactive({config$isDone[nbSteps()]}),
       reseted = reactive({rv$timeline$rstBtn()})
  )
    }
  )
}

