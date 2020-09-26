

mod_tl_engine_ui <- function(id){
  ns <- NS(id)
  tagList(
    useShinyjs(),
    mod_timeline_ui(ns("timeline")),
    uiOutput(ns('show_screens')),
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
mod_tl_engine_server <- function(id, dataIn=NULL, process_config = NULL, screens = NULL, remoteReset=FALSE){
  #stopifnot(!(is.null(dataIn) && is.reactive(config) && is.reactive(screens)))
  
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      rv <- reactiveValues()
      
      tl.update <- reactiveValues(
          current.pos = 1,
          actions = list(rst = TRUE,
                          nxt = TRUE,
                          prv = TRUE)
      )
      
      pos <- mod_timeline_server("timeline", style = 2, process_config = process_config, tl.update = tl.update)
      
      observeEvent(pos$rstBtn(), { print("clic on reset button")})
      
      navPage <- function(direction) {
        newval <- tl.update$current.pos + direction 
        newval <- max(1, newval)
        newval <- min(newval, length(process_config$stepsNames))
        tl.update$current.pos <- newval
      }
      observeEvent(pos$prevBtn(), ignoreInit = TRUE, {navPage(-1)})
      observeEvent(pos$nextBtn(), ignoreInit = TRUE, {navPage(1)})
      
      observeEvent(req(process_config$isDone[length(process_config$stepsNames)]), {
        print('the last step has just been validated')
        print(tl.update$current.pos)
        })
  
      observeEvent(c(tl.update$current.pos, process_config$isDone[tl.update$current.pos]), {
        process_config$isDone[tl.update$current.pos]
        print(paste0('New position = ', tl.update$current.pos))

        # Display the screen corresponding to the new position
         lapply(1:length(process_config$stepsNames), function(x){shinyjs::toggle(paste0('screen', x), 
                                                                           condition = x==tl.update$current.pos)})
        # 
        # Conditional enabling of the next button
        end_of_tl <- tl.update$current.pos == length(process_config$stepsNames)
        mandatory_step <- isTRUE(process_config$mandatory[tl.update$current.pos])
        validated <- isTRUE(process_config$isDone[tl.update$current.pos])
        cond.next.btn <-  !mandatory_step || validated
        #cond.next.btn <- TRUE
        tl.update$actions$nxt <- cond.next.btn

        # Conditional enabling of the prev button
        start_of_tl <- tl.update$current.pos == 1
        cond.prev.btn <- !start_of_tl
        tl.update$actions$prv <-  cond.prev.btn
        
        
      })
      
      
      # Initialization fo the process
      observeEvent(dataIn, { 
        print('Initialisation du module engine')
        rv$dataIn <- dataIn
        rv$screens <- screens

        # initialisation fo the screens
        for (i in 1:length(process_config$stepsNames))
            rv$screens[[i]] <- if (i == 1) div(id = ns(paste0("screen", i)),  rv$screens[[i]])
                                else  shinyjs::hidden(div(id = ns(paste0("screen", i)),  rv$screens[[i]]))

        # update the current.pos f the final step has been validated
        if (process_config$isDone[4])
          tl.update$current.pos <- length(process_config$stepsNames)
      })
      
      output$show_screens <- renderUI({
        tagList(rv$screens)
        })
      
      # Action on validation of the current step
      observeEvent(req(process_config$isDone[tl.update$current.pos]), {
        # Disable all previous screens but
        lapply(1:length(process_config$stepsNames), function(x){ shinyjs::disable(paste0('screen', x))})
      })
      
      
      
      
      
      output$show_dataIn <- renderPrint({rv$dataIn})
      output$show_dataOut <- renderPrint({rv$dataOut})
      
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
      
      
      
      
      
      ## The goal is t restart the timeline as if it is the first time
      # The main action is to reload the dataset
      # if the final validation button has not be clicked, then restore the last not null dataset
      # among the set of datasets before current position i
      # else reload the dataset among the set o 1 : (i-1)
      # observeEvent(req(c(rv.config$reset, remoteReset())),{
      #   
      #   # Re-enable all screens
      #   lapply(1:length(config$stepsNames), function(x){shinyjs::enable(paste0('screen', x))})
      #   
      #   tl.update$current.pos <- 1
      #   
      #   # Reload previous dataset
      #   for (i in 1:length(config$stepsNames))
      #       shinyjs::reset(paste0('screen', i))
      # 
      #   if (config$isDone[length(config$stepsNames)])
      #       rv$dataOut <- dataIn[-length(dataIn)]
      #     else
      #       rv$dataIn <- dataIn
      #     
      #     # Set all steps to undone except the first one which is the description screen
      #     config$isDone <- c(TRUE, rep(FALSE, length(config$stepsNames)-1))
      #     
      #     rv$dataOut <- NULL
      #     
      #     
      #   })
      

      ##########################################################
      
      reactive({rv$dataOut})
    }
  )
}

