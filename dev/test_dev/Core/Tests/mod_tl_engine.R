

mod_tl_engine_ui <- function(id){
  ns <- NS(id)
  tagList(
    useShinyjs(),
    mod_timeline_ui(ns("timeline")),
    uiOutput(ns('show_screens'))
  )
}

#' @param dataIn xxx
#'
#' 
#' 
mod_tl_engine_server <- function(id, process_config = NULL, hasReset=F, screens = NULL,remoteReset=FALSE){
  #stopifnot(!(is.null(dataIn()) && is.reactive(config) && is.reactive(screens)))
  
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns

      rv <- reactiveValues(
        hasReset = NULL
      )
      
      tl.update <- reactiveValues(
          current.pos = 1,
          actions = list(rst = TRUE,
                          nxt = TRUE,
                          prv = TRUE)
      )
      
      pos <- mod_timeline_server("timeline", 
                                 style = 2, 
                                 process_config = process_config, 
                                 tl.update = tl.update)
      
      
      navPage <- function(direction) {
        newval <- tl.update$current.pos + direction 
        newval <- max(1, newval)
        newval <- min(newval, length(process_config$stepsNames))
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
      observeEvent(req(c(pos$rstBtn()!=0, remoteReset()!=0)), {
        print(paste0("MODULE TL_ENGINE : ---> clic on reset button", pos$rstBtn()))
        #browser()
        # Re-enable all screens
        lapply(1:length(process_config$stepsNames), 
               function(x){shinyjs::enable(paste0('screen', x))})

        
        # Reload previous dataset
           lapply(1:length(process_config$stepsNames), 
                  function(x){ shinyjs::reset(paste0('screen', x))})

         #if (process_config$isDone[length(process_config$stepsNames)])
         #    rv$dataIn <- dataIn()[ , , -length(dataIn())]
         #  else
          #   rv$dataIn <- dataIn()

          # Set all steps to undone except the first one which is the description screen
        process_config$isDone <- c(TRUE, rep(FALSE, length(process_config$stepsNames)-1))
        tl.update$current.pos <- 1
        rv$hasReset <- TRUE
        })

      observeEvent(hasReset(), {
         print("MODULE TL_ENGINE : Received hasReset() = T => set rv$hasReset back to F")
         rv$hasReset <- F
       })
      
      # # Action on validation of the current step
       observeEvent(req(process_config$isDone[tl.update$current.pos]),  {
         
        # print("---> observeEvent(req(process_config$isDone[tl.update$current.pos])")
        # print("# Disable all previous screens but the current one")

         lapply(1:tl.update$current.pos, function(x){ shinyjs::disable(paste0('screen', x))})
         toggleNextBtn()

       })
      
  
      observeEvent(c(tl.update$current.pos,process_config$isDone),  ignoreInit = T, {
        process_config$mandatory
        
        print(paste0("-MODULE TL_ENGINE : --> observeEvent(tl.update$current.pos). New pos = ", tl.update$current.pos))
      
        #Case 1: the current step is validated -> disable all previous steps
        if (process_config$isDone[tl.update$current.pos])
          DisableAllPrevSteps()
        
        # Display the screen corresponding to the new position
        DisplayCurrentStep()
                                                                                
        toggleNextBtn()
        togglePrevBtn()
      })
      
      
      
      DisplayCurrentStep <- reactive({
        lapply(1:length(process_config$stepsNames), 
               function(x){shinyjs::toggle(paste0('screen', x),
                                           condition = x==tl.update$current.pos)}) 
      })
      
      
      DisableAllPrevSteps <- reactive({
        lapply(1:tl.update$current.pos, function(x){ shinyjs::disable(paste0('screen', x))})
      })
      
      toggleNextBtn <- reactive({
        tl.update$current.pos
        #browser()
        # # Conditional enabling of the next button
        end_of_tl <- tl.update$current.pos == length(process_config$stepsNames)
        mandatory_step <- isTRUE(process_config$mandatory[tl.update$current.pos])
        validated <- isTRUE(process_config$isDone[tl.update$current.pos])
        cond.next.btn <-  !mandatory_step || validated
        tl.update$actions$nxt <- cond.next.btn
      })
      
      togglePrevBtn <- reactive({
        start_of_tl <- tl.update$current.pos == 1
        cond.prev.btn <- !start_of_tl
        tl.update$actions$prv <-  cond.prev.btn
      })
      
      # Initialization fo the process
      observeEvent(process_config, { 
        print('MODULE TL_ENGINE : Initialisation du module engine')
        print(paste0('MODULE TL_ENGINE : Init pos = ', tl.update$current.pos))
        
        rv$screens <- screens
        rv$hasReset <- F
        
        # update the current.pos if the final step is validated
        if (process_config$isDone[length(process_config$stepsNames)])
          tl.update$current.pos <- length(process_config$stepsNames)
        
        # initialisation of the screens
        for (i in 1:length(process_config$stepsNames))
            rv$screens[[i]] <- if (i == tl.update$current.pos) 
                                  div(id = ns(paste0("screen", i)),  rv$screens[[i]])
                                else  
                                  shinyjs::hidden(div(id = ns(paste0("screen", i)),  rv$screens[[i]]))

        
        togglePrevBtn()
        toggleNextBtn()
      })
      
      output$show_screens <- renderUI({tagList(rv$screens)})
      
      #observeEvent(config$isDone, {
      #  print("new event on isDone")
      #  inames <- c("test_nav-prevBtn", "test_nav-nextBtn", "test_nav-rstBtn")
      #  toDisable <- names(input)[-which(names(input) %in% inames)]
      #browser()
      # Disable all input from test_nav but the action buttons
      
      #browser()
      # Disable all previous screens but the action buttons of the timeline
      #  if (config$isDone[current$val])
      #    lapply(toDisable, function(x){ shinyjs::disable( x)})
      #lapply(1:current$val, function(x){ shinyjs::disable(paste0('screen', x))})
      # })
      
      
      return(reactive({rv$hasReset}))
    }
  )
}

