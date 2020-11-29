

mod_wf_wf1_A_ui <- function(id){
  ns <- NS(id)
  tagList(
    useShinyjs(),
    mod_timeline_ui(ns("timeline")),
    uiOutput(ns('show_screens')),
    hr(),
    wellPanel(
      h3('Module A'),
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
mod_wf_wf1_A_server <- function(id, 
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
        process.name = 'Filtering',
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
        print(' ------- MODULE A : Initialisation du module A ------- ')
        
        rv$dataIn <- dataIn()
        rv$process.validated <- rv.process_config$isDone[size()]
        rv$skip = 0
        
        CreateScreens()
        InitScreens()
        
        if (isTRUE(rv$process.validated))
          tl.update$current.pos <-  size()
        else 
          tl.update$current.pos <- 1
        
        tl.update$actions$nxt <- condNextBtn() && rv$skip == 0
        tl.update$actions$prv <- condPrevBtn() && rv$skip == 0
      })
      
      
      observeEvent(req(c(pos$rstBtn()!=0, remoteReset()!=0)), {
        #ReinitScreens()
        lapply(1:length(rv.process_config$stepsNames), 
               function(x){
                 shinyjs::enable(paste0('screen', x))
                 shinyjs::reset(paste0('screen', x))
               })
        rv.process_config$isDone <- c(TRUE, rep(FALSE, size()-1))
        tl.update$current.pos <- 1
        
        
        rv$skip <- 0
        lapply(tl.update$actions, function(x){x <- T})
        
        if (!rv.process_config$isDone[size()])
          rv$dataIn <- dataIn()

        rv$dataOut <- NULL
      })
      

      
      
      observeEvent(req(pos$skipBtn() !=0),{
        print(' ------- MODULE A : Skipping module ------- ')
        
        tl.update$current.pos <- size()
        rv.process_config$isDone[size()] <- TRUE
        tl.update$actions$skip <- FALSE
        lapply(1:length(rv.process_config$stepsNames), 
               function(x){shinyjs::toggle(paste0('screen', x),
                                           condition = F)}) 
        rv$dataOut <- dataIn()
      })
      
      
      
      
      observeEvent(rv.process_config$isDone,  ignoreInit = T, {
        print(' ------- MODULE A : New step is validated ------- ')
        #if (rv.process_config$isDone[tl.update$current.pos])
        DisableAllPrevSteps()
        
        tl.update$actions$nxt <- condNextBtn() && rv$skip == 0
        tl.update$actions$prv <- condPrevBtn() && rv$skip == 0
      })
      
     
      observeEvent(tl.update$current.pos,  ignoreInit = T, {
        DisplayCurrentStep()
        tl.update$actions$nxt <- condNextBtn() && rv$skip == 0
        tl.update$actions$prv <- condPrevBtn() && rv$skip == 0
      })
      

      observeEvent(rv.process_config$isDone[size()], {
        rv$process.validated <- rv.process_config$isDone[size()]
      })
     
      
      observeEvent(req(forcePosition() != 0), ignoreNULL=T, { rv$forcePosition <- forcePosition()})
      observeEvent(req(rv$forcePosition), { tl.update$current.pos <- size() })
      observeEvent(req(pos$skipBtn() != 0), ignoreNULL=T, { rv$skip <- pos$skipBtn() })
      

      
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
         
         observeEvent(input$perform_screen2_btn, {
           rv.process_config$isDone[2] <- TRUE
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
            #isolate({
              rv$dataIn <- addAssay(rv$dataIn, 
                                    rv$dataIn[[length(rv$dataIn)]], 
                                    name=rv.process_config$process.name)
              rv$dataOut <- rv$dataIn
              rv.process_config$isDone[4] <- TRUE
           # })
       })
       
          
         
       
          
          
       ##########################################################
        
  list(dataOut = reactive({rv$dataOut}),
       validated = reactive({rv.process_config$isDone[size()]}),
       reseted = reactive({pos$rstBtn()})
  )
    }
  )
}

