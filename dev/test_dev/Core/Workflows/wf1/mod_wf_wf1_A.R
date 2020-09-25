

mod_wf_wf1_A_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_timeline_ui(ns("test_nav")),
    uiOutput(ns('show_UI')),
    hr(),
    wellPanel(
      h3('Module A'),
      p('rv$dataIn :'),
      verbatimTextOutput(ns('show_dataIn')),
      p('rv$dataOut'),
      verbatimTextOutput(ns('show_dataOut'))
    )
  )
}

#' @param dataIn xxx
#'
#' 
#' 
mod_wf_wf1_A_server <- function(id, dataIn=NULL, remoteReset=FALSE){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      rv <- reactiveValues()
      
      # variables to communicate with the navigation module
      rv.timeline <- reactiveValues(
        stepsNames = c("Description", "Step 1", "Step 2", "Step 3"),
        isDone =  c(TRUE, FALSE, FALSE, FALSE),
        mandatory =  c(FALSE, FALSE, TRUE, TRUE),
        current.pos = 1
      )
      
      ll.UI <- reactiveValues(screens = NULL)
      
      pos <- mod_timeline_server("test_nav", style = 2, pages = rv.timeline  )

      observeEvent(pos()$rstBtn, { print("clic on reset button")})
      
      observeEvent(pos()$nextBtn, { })
      
      observeEvent(pos()$prevBtn, { })
      
      observeEvent(pos()$current.pos, {
        print(paste0('current position = ', pos()$current.pos))
        rv.timeline$isDone[pos()$current.pos]
        
        # Display the screen corresponding to the new position
        lapply(1:length(rv.timeline$stepsNames), function(x){shinyjs::toggle(paste0('screen', x), 
                                                                             condition = x==pos()$current.pos)})
        
        # Conditional enabling of the next button
        #cond.next.btn <- isTRUE(rv.timeline$isDone[pos()$current.pos]) && (pos()$current.pos< length(rv.timeline$stepsNames)) || !isTRUE(rv.timeline$mandatory[pos()$current.pos])
        #shinyjs::toggleState(id = "nextBtn", condition = cond.next.btn) 
        
        #cond.prev.btn <- (pos()$current.pos > 1 && pos()$current.pos <= length(rv.timeline$stepsNames)) || (pos()$current.pos == length(rv.timeline$stepsNames) && !rv.timeline$isDone[pos()$current.pos])
        #shinyjs::toggleState(id = "prevBtn", condition = cond.prev.btn)
        
      })
      
      # Initialization fo the process
      session$userData$mod_A_obs_1 <-  observeEvent(dataIn(), { 
        print('Initialisation du module A')
        rv$dataIn <- dataIn()
       
        ll.UI$screens <- lapply(1:length(rv.timeline$stepsNames), function(x){
           do.call(uiOutput, list(outputId=ns(paste0("screen", x))))})
         
        # initialisation fo the screens
        ll.UI$screens[[1]] <- div(id = ns(paste0("screen", 1)),  ll.UI$screens[[1]])
        for (i in 2:length(rv.timeline$stepsNames)){
          ll.UI$screens[[i]] <- shinyjs::hidden(div(id = ns(paste0("screen", i)),  ll.UI$screens[[i]]))
        }
      })
      
      
      
      
      
      
      observeEvent(req(rv.timeline$isDone[pos()$current.pos]), {
        # Disable all previous screensbut
        lapply(1:length(rv.timeline$stepsNames), function(x){ shinyjs::disable(paste0('screen', x))})
      })
      

      
      
      output$show_UI <- renderUI({tagList(ll.UI$screens)})
      
      output$show_dataIn <- renderPrint({rv$dataIn})
      output$show_dataOut <- renderPrint({rv$dataOut})
      
      #observeEvent(rv.timeline$isDone, {
      #  print("new event on isDone")
      #  inames <- c("test_nav-prevBtn", "test_nav-nextBtn", "test_nav-rstBtn")
      #  toDisable <- names(input)[-which(names(input) %in% inames)]
        #browser()
        # Disable all input from test_nav but the action buttons
        
        #browser()
        # Disable all previous screens but the action buttons of the timeline
      #  if (rv.timeline$isDone[current$val])
      #    lapply(toDisable, function(x){ shinyjs::disable( x)})
          #lapply(1:current$val, function(x){ shinyjs::disable(paste0('screen', x))})
     # })
      
      
      
      
      
        ## The goal is t restart the timeline as if it is the first time
      # The main action is to reload the dataset
      # if the final validation button has not be clicked, then restore the last not null dataset
      # among the set of datasets before current position i
      # else reload the dataset among the set o 1 : (i-1)
      # observeEvent(req(c(rv.timeline$reset, remoteReset())),{
      #   
      #   # Re-enable all screens
      #   lapply(1:length(rv.timeline$stepsNames), function(x){shinyjs::enable(paste0('screen', x))})
      #   
      #   rv.timeline$current.pos <- 1
      #   
      #   # Reload previous dataset
      #   for (i in 1:length(rv.timeline$stepsNames))
      #       shinyjs::reset(paste0('screen', i))
      # 
      #   if (rv.timeline$isDone[length(rv.timeline$stepsNames)])
      #       rv$dataOut <- dataIn()[-length(dataIn())]
      #     else
      #       rv$dataIn <- dataIn()
      #     
      #     # Set all steps to undone except the first one which is the description screen
      #     rv.timeline$isDone <- c(TRUE, rep(FALSE, length(rv.timeline$stepsNames)-1))
      #     
      #     rv$dataOut <- NULL
      #     
      #     
      #   })


     
        
        
       #####################################################################
       ## screens of the module
       
       ############### SCREEN 1 ######################################
       output$screen1 <- renderUI({
         tagList(
           tags$h3(rv.timeline$name)
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
         
         rv.timeline$isDone[2] <- TRUE
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

         #rv$dataIn <- rv$dataIn[[length(rv$dataIn)]] + as.numeric(input$select2)
         rv.timeline$isDone[3] <- TRUE
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
              rv$dataIn <- addAssay(rv$dataIn, rv$dataIn[[length(rv$dataIn)]], name='Process_A')
              rv$dataOut <- rv$dataIn
              rv$dataIn <- NULL
              rv.timeline$isDone[4] <- TRUE
            })
       })
       
       
       ##########################################################
        
  reactive({rv$dataOut})
    }
  )
}

