

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
      
      # InitScreens <- reactive({
      #   # initialisation of the screens
      #   for (i in 1:length(rv.process_config$stepsNames))
      #     rv$screens[[i]] <- if (i == rv$current.pos) 
      #       div(id = ns(paste0("screen", i)),  rv$screens[[i]])
      #   else  
      #     shinyjs::hidden(div(id = ns(paste0("screen", i)),  rv$screens[[i]]))
      #   rv$screens
      # })
      
      # CreateScreens <- reactive({
      #   rv$screens <- lapply(1:length(rv.process_config$stepsNames), function(x){
      #     do.call(uiOutput, list(outputId=ns(paste0("screen", x))))})
      #   rv$screens
      # })
      
      ReinitScreens <- reactive({
        lapply(1:length(rv.process_config$stepsNames), 
               function(x){
                 shinyjs::enable(paste0('screen', x))
                 shinyjs::reset(paste0('screen', x))
               })
        rv.process_config$isDone <- c(TRUE, rep(FALSE, size()-1))
        rv$current.pos <- 1
      })
      
      
      output$show_dataIn <- renderPrint({names(dataIn())})
      output$show_rv_dataIn <- renderPrint({names(rv$dataIn)})
      output$show_rv_dataOut <- renderPrint({names(rv$dataOut)})
      #output$show_screens <- renderUI({tagList(rv$screens)})
      
      
      
      
      
      
      DisableAllPrevSteps <- reactive({
        pos <- max(grep(TRUE, rv.process_config$isDone))
        lapply(1:pos, 
               function(x){ shinyjs::disable(paste0('screen', x))})
        
      })
      
      DisableAllSteps <- reactive({
        lapply(1:length(rv.process_config$isDone), 
               function(x){ shinyjs::disable(paste0('screen', x))})
        
      })
      
      
      DisplayCurrentStep <- reactive({
        lapply(1:length(rv.process_config$stepsNames), 
               function(x){shinyjs::toggle(paste0('screen', x),
                                           condition = x==rv$current.pos )}) 
      })
      
      
      HideAllSteps <- reactive({
        lapply(1:length(rv.process_config$stepsNames), 
               function(x){shinyjs::toggle(paste0('screen', x),
                                           condition = F)}) 
      })
      
      condNextBtn <- reactive({
        
        # # Conditional enabling of the next button
        end_of_tl <- rv$current.pos == length(rv.process_config$stepsNames)
        mandatory_step <- isTRUE(rv.process_config$mandatory[rv$current.pos])
        validated <- isTRUE(rv.process_config$isDone[rv$current.pos])
        cond.next.btn <-  !mandatory_step || validated
        cond.next.btn
      })
      
      condPrevBtn <- reactive({
        start_of_tl <- rv$current.pos == 1
        cond.prev.btn <- !start_of_tl
        cond.prev.btn
      })
      
      size <- reactive({
        req(rv.process_config$stepsNames)
        length(rv.process_config$stepsNames)
      })
      #################################################################################
      
      
      
      rv <- reactiveValues(
        current.pos = 1,
        screens = NULL)
      
      
      actions <- reactiveValues(
          rst = TRUE,
          nxt = TRUE,
          prv = TRUE)

      
      # variables to communicate with the navigation module
      rv.process_config <- reactiveValues(
        type = 'process',
        name = 'Filtering',
        stepsNames = c("Description", "Step 1", "Step 2", "Step 3"),
        isDone =  c(TRUE, FALSE, FALSE, FALSE),
        mandatory =  c(FALSE, FALSE, TRUE,TRUE),
        screens = list(screenStep1 = uiOutput(ns('screen1')),
                       screenStep2 = uiOutput(ns('screen2')),
                       screenStep3 = uiOutput(ns('screen3')),
                       screenStep1 = uiOutput(ns('screen4')))
      )
   
      pos <- mod_timeline_server("timeline", 
                                 style = 2, 
                                 pages = rv.process_config, 
                                 actions = actions)
      

      #--------------------------------------------------------------
      observeEvent(req(dataIn()), { 
        print(' ------- MODULE A : Initialisation du module A ------- ')
        rv$dataIn <- dataIn()
      })
      
      
      #--------------------------------------------------------------
      observeEvent(req(rv.process_config), { 
        print(' ------- MODULE A : Initialisation de la configuration A ------- ')
        
        rv$isValidated <- rv.process_config$isDone[size()]
        
        #CreateScreens()
        #InitScreens()
        print(rv$screens)
        if (isTRUE(rv$isValidated))
          rv$current.pos <-  size()
        else 
          rv$current.pos <- 1

        actions$nxt <- condNextBtn()
        actions$prv <- condPrevBtn() 
      })
      
      
      #--------------------------------------------------------------
      observeEvent(req(c(pos$rstBtn()!=0, remoteReset()!=0)), {
        ReinitScreens()

        rv.process_config$isDone <- c(TRUE, rep(FALSE, size()-1))
        rv$current.pos <- 1
        lapply(actions, function(x){x <- T})

        rv$dataIn <- RemoveItemFromDataset(dataIn(), rv.process_config$name)
        rv$dataOut <- NULL
      })
      

      
      observeEvent(rv.process_config$isDone,  ignoreInit = T, {
        print(' ------- MODULE A : A new step is validated ------- ')
        
        DisableAllPrevSteps()
        rv$isValidated <- rv.process_config$isDone[size()]
        actions$nxt <- condNextBtn()
        actions$prv <- condPrevBtn()
      })
      
     
      observeEvent(rv$current.pos,  ignoreInit = T, {
        DisplayCurrentStep()
        actions$nxt <- condNextBtn() 
        actions$prv <- condPrevBtn() 
      })
      

      
      observeEvent(req(forcePosition() != 0), ignoreNULL=T, { rv$forcePosition <- forcePosition()})
      observeEvent(req(rv$forcePosition), { rv$current.pos <- size() })
      

      
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
              rv$dataIn <- AddItemToDataset(rv$dataIn, rv.process_config$name)
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

