
#' 
mod_tl_engine_server <- function(id, process_config = NULL, screens = NULL, remoteReset=FALSE, forcePosition = NULL){
  #stopifnot(!(is.reactive(screens) && !is.null(screens)))
  #stopifnot(!is.reactive(process_config) && !is.null(process_config))
  
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns

      
      
      # observeEvent(req(forcePosition()), {
      #   print(paste0("MODULE TL_ENGINE : New value for forcePosition : ", forcePosition()))
      #   tl.update$current.pos <- forcePosition()
      # })
      
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
        #print(paste0("MODULE TL_ENGINE : ---> clic on reset button = ", pos$rstBtn()))
        #print(paste0("MODULE TL_ENGINE : ---> new value for remoteReset() = ", remoteReset()))
        #browser()
        # Re-enable all screens
        #print("MODULE TL_ENGINE : Re-enable all screens")
        lapply(1:length(process_config$stepsNames), 
               function(x){shinyjs::enable(paste0('screen', x))})
        
        # Reset for all screens inputs of the caller
        #print("MODULE TL_ENGINE : Reset all screens inputs")
        lapply(1:length(process_config$stepsNames), 
                  function(x){ shinyjs::reset(paste0('screen', x))})

          # Set all steps to undone except the first one which is the description screen
        #print("MODULE TL_ENGINE : Set all steps to undone")
        process_config$isDone <- c(TRUE, rep(FALSE, length(process_config$stepsNames)-1))
        tl.update$current.pos <- 1
        })

      
      
      
  
      observeEvent(c(tl.update$current.pos, process_config$isDone),  ignoreInit = T, {
        process_config$mandatory
        
        #print(paste0("####### MODULE TL_ENGINE : --> observeEvent(tl.update$current.pos). New pos = ", tl.update$current.pos))
        #rint(paste0("####### MODULE TL_ENGINE : --> isDone = ", paste0(process_config$isDone, collapse=' ')))
        
        #Case 1: the current step is validated -> disable all previous steps
        if (process_config$isDone[tl.update$current.pos])
          {
          DisableAllPrevSteps()
        }
        
        # Display the screen corresponding to the new position
        DisplayCurrentStep()
                                                                                
        toggleNextBtn()
        togglePrevBtn()
      })
      
      
      
      DisplayCurrentStep <- reactive({
        # Display the screen corresponding to the new position
        print("####### MODULE TL_ENGINE : --> Display the screen corresponding to the new position")
        cond <- 
        lapply(1:length(process_config$stepsNames), 
               function(x){shinyjs::toggle(paste0('screen', x),
                                           condition = x==tl.update$current.pos )}) 
      })
      
      
      DisableAllPrevSteps <- reactive({
        #print("####### MODULE TL_ENGINE : --> the current step is validated -> disable all previous steps")
        #browser()
        #lapply(1:tl.update$current.pos, function(x){ shinyjs::disable(paste0('screen', x))})
        tl.update$actions$rst <- T
      })
      
      toggleNextBtn <- reactive({
        tl.update$current.pos

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
        
        # update the current.pos if the final step is validated
        #if (process_config$isDone[length(process_config$stepsNames)])
        #  tl.update$current.pos <- length(process_config$stepsNames)
        
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
      
      list(reset = reactive({pos$rstBtn()}),
           position = reactive({tl.update$current.pos}))
    }
  )
}

