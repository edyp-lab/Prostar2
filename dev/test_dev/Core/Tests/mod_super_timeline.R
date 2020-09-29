
mod_super_timeline_ui <- function(id){
  ns <- NS(id)
  tagList(
    useShinyjs(),
    mod_timeline_ui(ns("timeline")),
    uiOutput(ns('show_screens')),
    hr(),
    wellPanel(
      h3('Pipeline'),
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
mod_super_timeline_server <- function(id, dataIn=NULL){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
      rv <- reactiveValues()
      
      # variables to communicate with the navigation module
      rv.process_config <- reactiveValues(
        process.name = 'Pipeline protein',
        stepsNames = c("Description", "Filtering", "Normalization", "Imputation", "Summary"),
        isDone =  c(TRUE, rep(FALSE,4)),
        mandatory =  c(FALSE, rep(TRUE,4))
      )
      
      
      
      tl.update <- reactiveValues(
        current.pos = 1,
        actions = list(rst = TRUE,
                       nxt = TRUE,
                       prv = TRUE)
      )
      
      
      pos <- mod_timeline_server("timeline", 
                                 style = 2, 
                                 process_config = rv.process_config, 
                                 tl.update = tl.update)
      
      
      # Initialization of the process
      observeEvent(req(dataIn()), {
        print("--------------------------------------------------")
        print('MODULE SUPER_TIMELINE : Initialisation du module')
        rv$dataIn <- dataIn()
        
        rv$remoteReset <- rep(length(rv.process_config$stepsNames), FALSE)
        
        # Instantiation of the screens
        rv$screens <- lapply(1:length(rv.process_config$stepsNames), function(x){
          do.call(uiOutput, list(outputId=ns(paste0("screen", x))))})
        
        
        # initialisation of the screens
        for (i in 1:length(rv.process_config$stepsNames))
          rv$screens[[i]] <- if (i == tl.update$current.pos) 
            div(id = ns(paste0("screen", i)),  rv$screens[[i]])
        else  
          shinyjs::hidden(div(id = ns(paste0("screen", i)),  rv$screens[[i]]))
        
        togglePrevBtn()
        toggleNextBtn()
        
        print(paste0('MODULE SUPER_TL : New value for tl.update$current.pos : ', tl.update$current.pos))
      })
      
      
      output$show_dataIn <- renderPrint({dataIn()})
      output$show_rv_dataIn <- renderPrint({rv$dataIn})
      output$show_rv_dataOut <- renderPrint({rv$dataOut})
      output$show_screens <- renderUI({tagList(rv$screens)})
      
      
      
      navPage <- function(direction) {
        newval <- tl.update$current.pos + direction 
        newval <- max(1, newval)
        newval <- min(newval, length(rv.process_config$stepsNames))
        tl.update$current.pos <- newval
      }
      observeEvent(pos$prevBtn(), ignoreInit = TRUE, {navPage(-1)})
      observeEvent(pos$nextBtn(), ignoreInit = TRUE, {navPage(1)})
      
      
      #------------------------------------------------------------------------
      observeEvent(req(pos$rstBtn()!=0), {
       lapply(1:length(rv.process_config$stepsNames), 
               function(x){shinyjs::enable(paste0('screen', x))})
        
        lapply(1:length(rv.process_config$stepsNames), 
               function(x){ shinyjs::reset(paste0('screen', x))})
        
        # Set all steps to undone except the first one which is the description screen
        #print("MODULE TL_ENGINE : Set all steps to undone")
        rv.process_config$isDone <- c(TRUE, rep(FALSE, length(rv.process_config$stepsNames)-1))
        tl.update$current.pos <- 1
        
        UpdateDataIn()
        
      })
      
      
      UpdateDataIn <- reactive({
        rv$dataOut <- dataIn()
      })
      
      
      
      observeEvent(c(tl.update$current.pos, rv.process_config$isDone),  ignoreInit = T, {
        rv.process_config$mandatory
        
        if (rv.process_config$isDone[tl.update$current.pos])
          DisableAllPrevSteps()

        # Display the screen corresponding to the new position
        DisplayCurrentStep()
        
        toggleNextBtn()
        togglePrevBtn()
      })
      
      
      
      DisplayCurrentStep <- reactive({
        # Display the screen corresponding to the new position
        lapply(1:length(rv.process_config$stepsNames), 
                 function(x){shinyjs::toggle(paste0('screen', x),
                                             condition = x==tl.update$current.pos )}) 
      })
      
      
      DisableAllPrevSteps <- reactive({
       # pos <- max(grep(TRUE, rv.process_config$isDone))
       # lapply(1:pos, function(x){ shinyjs::disable(paste0('screen', x))})
      #  tl.update$actions$rst <- T
      })
      
      toggleNextBtn <- reactive({
        tl.update$current.pos
        
        # # Conditional enabling of the next button
        end_of_tl <- tl.update$current.pos == length(rv.process_config$stepsNames)
        mandatory_step <- isTRUE(rv.process_config$mandatory[tl.update$current.pos])
        validated <- isTRUE(rv.process_config$isDone[tl.update$current.pos])
        cond.next.btn <-  !mandatory_step || validated
        tl.update$actions$nxt <- cond.next.btn
      })
      
      togglePrevBtn <- reactive({
        start_of_tl <- tl.update$current.pos == 1
        cond.prev.btn <- !start_of_tl
        tl.update$actions$prv <-  cond.prev.btn
      })
    

      # observeEvent(rv$tmp_super$reset(), {
      #   rv$remoteReset <- rep(length( rv.process_config$stepsNames), TRUE)
      # })
        
      #####################################################################
      ## screens of the module
      ##
      ############### SCREEN 1 ######################################
      output$screen1 <- renderUI({
        tagList(
          tags$h3(paste0('Pipeline ', rv.process_config$name))
        )
      })
      
      
      ############### SCREEN 2 ######################################
      
      output$screen2 <- renderUI({
        
        tagList(
          div(id=ns('screen2'),
              tags$h3(rv.process_config$stepsName[2]),
              mod_wf_wf1_A_ui(ns('mod_A_nav'))
          )
        )
      })

      rv$tmpA <- mod_wf_wf1_A_server("mod_A_nav",
                                     dataIn = rv$dataIn,
                                     remoteReset = rv$remoteReset[2],
                                     forcePosition = NULL
                                     )
      
      observeEvent(rv$tmpA$dataOut(),  { 
      isolate({
        rv$dataOut <- rv$tmpA$dataOut()
      })
      })
      
      observeEvent(rv$tmpA$validated(),  { 
        #print('MODULE SUPER_TL : New value for rv$validated() :')
        #print(paste0("      rv$tmpA$validated() = ", rv$tmpA$validated()))
        isolate({
          rv.process_config$isDone[2] <- rv$tmpA$validated()
        })
      })
      
     # observeEvent(req(rv$tmpA$reseted()!=0), {
     #   print(paste0('MODULE SUPER_TL : New value for rv$tmpA$reseted() : ', rv$tmpA$reseted()))
       # ind <- grep(rv.process_config$stepsNames[4], names(rv$dataIn))
       # if (length(ind)>0){
       #   rv$dataIn <- dataIn()
       #   rv$dataOut <- rv$dataIn
       #   rv.process_config$isDone[ind:length(dataIn())] <- FALSE
       # }
        
     # })
      
      ############### SCREEN 3 ######################################
      output$screen3 <- renderUI({
        
        tagList(
          div(id=ns('screen3'),
              tags$h3(rv.process_config$stepsName[3]),
              mod_wf_wf1_B_ui(ns('mod_B_nav'))
          )
        )
      })
      
      rv$tmpB <- mod_wf_wf1_B_server("mod_B_nav",
                                     dataIn = rv$tmpA$dataOut(),
                                     remoteReset = rv$remoteReset[3],
                                     forcePosition = rv.process_config$isDone[length(rv.process_config$isDone)]
                                     )
      
      observeEvent(req(rv$tmpB$dataOut()),  { 
        isolate({ rv$dataOut <- rv$tmpB$dataOut()})
      })
      
      observeEvent(rv$tmpB$validated(),  { 
        #print('MODULE SUPER_TL : New value for rv$tmpB$validated() :')
        #print(paste0("      rv$tmpB$validated() = ", rv$tmpB$validated()))
        isolate({ rv.process_config$isDone[3] <- rv$tmpB$validated()})
      })
      
      observeEvent(req(rv$tmpB$reseted()!=0), {
        print(paste0('MODULE SUPER_TL : New value for rv$tmpB$reseted() : ', rv$tmpB$reseted()))
        #ind <- grep(rv.process_config$stepsNames[4], names(rv$dataIn))
        #if (length(ind)>0){
       #   rv$dataIn <- dataIn()
        #  rv$dataOut <- rv$dataIn
        #  rv.process_config$isDone[ind:length(dataIn())] <- FALSE
       # } 
        
      })
      
      
      ############### SCREEN 4 ######################################
      output$screen4 <- renderUI({
        
        tagList(
          div(id=ns('screen4'),
              tags$h3(rv.process_config$stepsName[4]),
              mod_wf_wf1_C_ui(ns('mod_C_nav'))
          )
        )
      })
      
      rv$tmpC <- mod_wf_wf1_C_server("mod_C_nav",
                                     dataIn = rv$tmpB$dataOut(),
                                     remoteReset = rv$remoteReset[4] ,
                                     forcePosition = rv.process_config$isDone[length(rv.process_config$isDone)]
                                     )
      
      
      observeEvent(req(rv$tmpC$dataOut()),  { 
        isolate({  rv$dataOut <- rv$tmpC$dataOut()})
      })
      
      observeEvent(rv$tmpC$validated(),  { 
        #print('MODULE SUPER_TL : New value for rv$tmpC$validated() :')
        #print(paste0("      rv$tmpC$validated() = ", rv$tmpC$validated()))
        isolate({ rv.process_config$isDone[4] <- rv$tmpC$validated()})
      })
      
      observeEvent(req(rv$tmpC$reseted()!=0), {
        print(paste0('MODULE SUPER_TL : New value for rv$tmpC$reseted() : ', rv$tmpC$reseted()))
 
       # ind <- grep(rv.process_config$stepsNames[4], names(rv$dataIn))
       # if (length(ind)>0){
       #   rv$dataIn <- dataIn()
       #   rv$dataOut <- rv$dataIn
       #   rv.process_config$isDone[ind:length(dataIn())] <- FALSE
       # } 
        
      })
      
      ############### SCREEN 5 ######################################
      output$screen5 <- renderUI({
        
        tagList(
          div(id='screen5',
              tags$h3(rv.process_config$stepsName[5])
          )
        )
      })
      
      ##########################################################
      
      reactive({rv$dataOut})
    }
  )
}

