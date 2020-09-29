

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
      
      rv <- reactiveValues(
        screens=NULL
      )

      
      # variables to communicate with the navigation module
      rv.process_config <- reactiveValues(
        process.name = 'Filtering',
        stepsNames = c("Description", "Step 1", "Step 2", "Step 3"),
        isDone =  c(TRUE, rep(FALSE, 3)),
        mandatory =  c(FALSE, rep(TRUE, 3))
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
        print('MODULE A : Initialisation du module A')
        print(paste0("rv.process_config$isDone = ", paste0(rv.process_config$isDone, collapse=' ')))
        rv$dataIn <- dataIn()
        rv$process.validated <- rv.process_config$isDone[length(rv.process_config$isDone)]
        #browser()
        
        print(paste0("     tl.update$current.pos = ", tl.update$current.pos))
        print(paste0("     rv$process.validated = ", rv$process.validated))
       # Instantiation of the screens
        rv$screens <- lapply(1:length(rv.process_config$stepsNames), function(x){
          do.call(uiOutput, list(outputId=ns(paste0("screen", x))))}) 
        
        # initialisation of the screens
        for (i in 1:length(rv.process_config$stepsNames))
          rv$screens[[i]] <- if (i == tl.update$current.pos) 
            div(id = ns(paste0("screen", i)),  rv$screens[[i]])
        else  
          shinyjs::hidden(div(id = ns(paste0("screen", i)),  rv$screens[[i]]))
        
        
        if (isTRUE(rv$process.validated)){
          pos <- max(grep(TRUE, rv.process_config$isDone))
          #lapply(1:pos, function(x){ shinyjs::disable(paste0('screen', x))})
          tl.update$current.pos <-  length(rv.process_config$isDone)}
        else {
          tl.update$current.pos <- 1
        }
        
        
        togglePrevBtn()
        toggleNextBtn()
        
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
      
      
      ###
      ###
      ### RESET FUNCTION
      ### The goal is to restart the timeline as if it is the first time
      ### The main action is to reload the dataset
      ### if the final validation button has not be clicked, then restore the last not null dataset
      ### among the set of datasets before current position i
      ### else reload the dataset among the set o 1 : (i-1)
      ###
      ###
     # observeEvent(req(c(pos$rstBtn()!=0, remoteReset()!=0)), {
     #   print('MODULE A : RESET du module A')
     #   lapply(1:length(rv.process_config$stepsNames), 
     #          function(x){shinyjs::enable(paste0('screen', x))})
        
     #   lapply(1:length(rv.process_config$stepsNames), 
     #          function(x){ shinyjs::reset(paste0('screen', x))})
        
     #   rv.process_config$isDone <- c(TRUE, rep(FALSE, length(rv.process_config$stepsNames)-1))
     #   tl.update$current.pos <- 1
     #   rv$process.validated <- F

     #   UpdateDataIn()

    #  })
      
      # In order to trigger the initialization of the module, one change 
      # the value of rv$dataOut in the case where it is necessary
     # UpdateDataIn <- reactive({
     #   ind <- grep(rv.process_config$process.name, names(rv$dataIn))
     #   if (length(ind) == 0)
     #     rv$dataIn <- dataIn()
     #   else
      #    rv$dataIn <- dataIn()[ , , -c(ind:length(dataIn()))]
      #  rv$dataOut <- rv$dataIn
      #})
      
      
      
      observeEvent(c(rv.process_config$isDone,tl.update$current.pos),  ignoreInit = T, {
        rv.process_config$mandatory
        print('MODULE A : observeEvent(rv.process_config$isDone)')
        print(paste0('     New value for rv.process_config$isDone : ', paste0(rv.process_config$isDone, collapse=' ')))
       
        lapply(1:length(rv.process_config$stepsNames), 
               function(x){shinyjs::toggle(paste0('screen', x),
                                           condition = x==tl.update$current.pos )}) 
        
        if (rv.process_config$isDone[tl.update$current.pos]){
          pos <- max(grep(TRUE, rv.process_config$isDone))
        lapply(1:pos, function(x){ shinyjs::disable(paste0('screen', x))})
        }
        
        
        
                toggleNextBtn()
        togglePrevBtn()
      })
      
     
      toggleNextBtn <- reactive({
        
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
               tags$h2('Step 1'),
               actionButton(ns('perform_screen2_btn'), 'Perform'),
               selectInput(ns('select1'), 'Select step 1', 
                           choices = 1:5, 
                           selected = 1,
                           width = '150px')
           )
         )
       })
       
       observeEvent(input$perform_screen2_btn, {
         # Put here the code for modifying the QF after this step
         shinyjs::disable('screen2')
         rv.process_config$isDone[2] <- TRUE
       })
       
       
       ############### SCREEN 3 ######################################
       output$screen3 <- renderUI({
         
         tagList(
           div(id=ns('screen3'),
               tags$h3('Step 2'),
               actionButton(ns('perform_screen3_btn'), 'Perform'),
               selectInput(ns('select2'), 'Select step 2',
                           choices = 1:5,
                           selected = 1,
                           width = '150px')
           )
         )
       })
       
       ## Logics to implement: here, we must take the last data not null
       # in previous datas. The objective is to take account
       # of skipped steps
       observeEvent(input$perform_screen3_btn, {
         shinyjs::disable('screen3')
         rv.process_config$isDone[3] <- TRUE
       })
       
       
       ############### SCREEN 4 ######################################
       output$screen4 <- renderUI({
         
         tagList(
           div(id=ns('screen4'),
               tags$h3('Step 4'),
               actionButton(ns('validate_btn'), 'Validate')
           )
         )
       })
         
          observeEvent(input$validate_btn, {
            isolate({
              shinyjs::disable('screen4')
              rv$dataIn <- addAssay(rv$dataIn, 
                                    rv$dataIn[[length(rv$dataIn)]], 
                                    name=rv.process_config$process.name)
              rv$dataOut <- rv$dataIn
              #rv$dataIn <- NULL
              rv.process_config$isDone[4] <- TRUE
            })
       })
       
       
          
          
       ##########################################################
        
  list(dataOut = reactive({rv$dataOut}),
       validated = reactive({rv$process.validated}),
       reseted = reactive({pos$rstBtn()})
  )
    }
  )
}

