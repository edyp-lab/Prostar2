

mod_tl_engine_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_timeline_ui(ns("timeline")),
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
mod_tl_engine_server <- function(id, dataIn=NULL, config = NULL, remoteReset=FALSE){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      rv <- reactiveValues()
      
      rv.engine <- reactiveValues(
          current.pos = 1,
          actions = list(rst = TRUE,
                          nxt = TRUE,
                          prv = TRUE)
      )
      
      
      screens <- reactiveValues()
      
      pos <- mod_timeline_server("timeline", style = 2, config = config, actions = rv.engine)
      
      observeEvent(pos$rstBtn(), { print("clic on reset button")})
      
      navPage <- function(direction) {
        newval <- rv.engine$current.pos + direction 
        newval <- max(1, newval)
        newval <- min(newval, length(config$stepsNames))
        rv.engine$current.pos <- newval
      }
      observeEvent(pos$prevBtn(), ignoreInit = TRUE, {navPage(-1)})
      observeEvent(pos$nextBtn(), ignoreInit = TRUE, {navPage(1)})
      
      
      observeEvent(c(rv.engine$current.pos, config$isDone), {
        config$isDone[rv.engine$current.pos]
        print(paste0('New position = ', rv.engine$current.pos))
        
        # Display the screen corresponding to the new position
        lapply(1:length(config$stepsNames), function(x){shinyjs::toggle(paste0('screen', x), 
                                                                           condition = x==rv.engine$current.pos)})
        
        # Conditional enabling of the next button
        end_of_tl <- rv.engine$current.pos == length(config$stepsNames)
        mandatory_step <- isTRUE(config$mandatory[rv.engine$current.pos])
        validated <- isTRUE(config$isDone[rv.engine$current.pos])
        cond.next.btn <-  !mandatory_step || validated
        rv.engine$actions$nxt <- cond.next.btn 
        
        # Conditional enabling of the prev button
        start_of_tl <- rv.engine$current.pos == 1
        cond.prev.btn <- TRUE 
        #rv.engine$actions$prv <-  cond.prev.btn
      })
      
      
      
      
      
      
      
      
      
      # Initialization fo the process
      session$userData$mod_tl_engine_1 <-  observeEvent(dataIn(), { 
        print('Initialisation du module engine')
        rv$dataIn <- dataIn()
        
        screens$ui <- lapply(1:length(config$stepsNames), function(x){
          do.call(uiOutput, list(outputId=ns(paste0("screen", x))))})
        
        # initialisation fo the screens
        screens$ui[[1]] <- div(id = ns(paste0("screen", 1)),  screens$ui[[1]])
        for (i in 2:length(config$stepsNames)){
          screens$ui[[i]] <- shinyjs::hidden(div(id = ns(paste0("screen", i)),  screens$ui[[i]]))
        }
      })
      
      
      
      
      
      
      observeEvent(req(config$isDone[rv.engine$current.pos]), {
        # Disable all previous screens but
        lapply(1:length(config$stepsNames), function(x){ shinyjs::disable(paste0('screen', x))})
      })
      
      
      
      
      output$show_UI <- renderUI({tagList(screens$ui)})
      
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
      #   rv.engine$current.pos <- 1
      #   
      #   # Reload previous dataset
      #   for (i in 1:length(config$stepsNames))
      #       shinyjs::reset(paste0('screen', i))
      # 
      #   if (config$isDone[length(config$stepsNames)])
      #       rv$dataOut <- dataIn()[-length(dataIn())]
      #     else
      #       rv$dataIn <- dataIn()
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

