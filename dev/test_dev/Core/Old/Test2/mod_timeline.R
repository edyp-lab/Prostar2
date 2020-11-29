btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"

# Module UI

#' @title   mod_navigation_ui and mod_navigation_server
#' @description  A shiny Module. The sass source code for timeline was inspired by 
#'  : https://codepen.io/cjl750/pen/mXbMyo
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' @param status xxxxx
#' @param screens xxxxx
#' @param rstFunc xxxxx
#' @param iconType xxxxxx
#'
#' @rdname mod_navigation
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList
#' @importFrom shinyjs disabled inlineCSS
mod_timeline_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(),
    uiOutput(ns("load_css_style")),
    
    fluidRow(
      align= 'center',
      column(width=2,div(style=btn_style,
                         uiOutput(ns('showResetBtn')),
                         shinyjs::disabled(actionButton(ns("prevBtn"), "<<",
                                                        class = PrevNextBtnClass,
                                                        style='padding:4px; font-size:80%'))
      )),
      column(width=8,div( style = btn_style, uiOutput(ns("timelineStyle"))) ),
      column(width=2,div(style=btn_style,
                         actionButton(ns("nextBtn"), "next",
                                      class = PrevNextBtnClass,
                                      style='padding:4px; font-size:80%'))
      )
    ),
    uiOutput(ns('show_screens'))
  )
}

# Module Server

#' @rdname mod_navigation
#' 
#' @param id the id to connect the ui and server parts of the module
#' 
#' @param style An integer which codes for the style of timeline
#' 
#' @param config A list of xx elements to configure and update the timeline:
#'   * type: 
#'   * process.name:
#'   * position An integer which specify the position to which to go. This is
#'   used by the caller to force the 
#' 
#' @param  btns xxx
#' 
#' @export
#' 
#' @keywords internal
#' 
#' @import shiny shinyjs
#' 
#' @importFrom sass sass
#' 
mod_timeline_server <- function(id, style=2, config){
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    verbose = T
    source(file.path('.', 'code_general.R'), local=TRUE)$value
    
    
    output$showResetBtn <- renderUI({
      print(paste0('TL(',config$process.name, ') : output$showResetBtn <- renderUI'))
      actionButton(ns("rstBtn"), paste0("Reset ", config$type),
                   class = redBtnClass,
                   style='padding:4px; font-size:80%')
      })
    
    output$timelineStyle <- renderUI({ uiOutput(ns(paste0('timeline', style))) })
    
    #-------------------------------------------------------
    # Return the UI for a modal dialog with data selection input. If 'failed' is
    # TRUE, then display a message that the previous value was invalid.
    dataModal <- function() {
     

      if(config$type == "pipeline")
       txt <- 'This action will reset the entire pipeline and delete all the datasets.'
     else if (config$type == "process")
       txt <- paste0("This action will reset this process. The input dataset will be the output of the previous
                     validated process and all further datasets will be removed")
       
       modalDialog(
        span(txt),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("modal_ok"), "OK")
        )
      )
    }
    
    
    # Show modal when button reset is clicked
    observeEvent(input$rstBtn, {
      showModal(dataModal())
    })
    
    # When OK button is pressed, update the reactive value which will be sent
    # to the caller
    observeEvent(input$modal_ok, {
      current$reset_OK <- input$rstBtn
      removeModal()
    })
    
    #----------------------------------------------------------
    
    # Update current position with the value received by the caller module
    #observeEvent(position(),{current$val <- position()})
    
    
    current <- reactiveValues(
      val = 1,
      nbSteps = NULL,
      
      # Transit variable to manage the clics on the reset button
      reset_OK = FALSE
    )
    
    observeEvent(req(config),{
      if(verbose)
        print(paste0('TL(',config$process.name, ') : observeEvent(req(config)() '))
      
      current$nbSteps <- length(config$steps)
      InitScreens()
    })
    
    # Initialization of the screens by integrating them into a div specific
    # to this module (name prefixed with the ns() function
    # Those div englobs the div of the caller where screens are defined
    InitScreens <- reactive({
      if(verbose)
        print(paste0('TL(',config$process.name, ') : call to InitScreens() '))
      
      config$screens <- lapply(1:current$nbSteps,
                               function(x){
                                 config$screens[[x]] <- if (x == 1) 
                                   div(id = ns(paste0("div_screen", x)),  config$screens[[x]])
                                 else 
                                   shinyjs::hidden(div(id = ns(paste0("div_screen", x)),  config$screens[[x]]))
                               })
    })
    
    
    
    navPage <- function(direction) {
      newval <- current$val + direction 
      newval <- max(1, newval)
      newval <- min(newval, current$nbSteps)
      current$val <- newval
    }
    observeEvent(input$prevBtn, ignoreInit = TRUE, {navPage(-1)})
    observeEvent(input$nextBtn, ignoreInit = TRUE, {navPage(1)})
    
    
    output$show_screens <- renderUI({tagList(config$screens)})
    
    # Builds the condition to enable/disable the next button
    NextBtn_logics <- reactive({
      end_of_tl <- current$val == current$nbSteps
      mandatory_step <- isTRUE(config$steps[[current$val]])
      validated <- config$status[[current$val]] == VALIDATED
      skipped <- config$status[[current$val]] == SKIPPED
      entireProcessSkipped <- config$status[[current$nbSteps]] == SKIPPED
      !end_of_tl && !entireProcessSkipped && (!mandatory_step || (mandatory_step && (validated || skipped)))
    })
    
    # Builds the condition to enable/disable the 'Previous' button
    PrevBtn_logics <- reactive({
      start_of_tl <- current$val == 1
      entireProcessSkipped <- config$status[[current$nbSteps]] == SKIPPED
      
      !start_of_tl && !entireProcessSkipped
    })
    
    # Catch a new position or a change in the status list
    observeEvent(c(current$val, config$status), {
      if(verbose){
        print(paste0('TL(',config$process.name, ') : observeEvent(c(current$val, config$status : '))
        print(paste0('TL(',config$process.name, ') : status = ', paste0(config$status, collapse=' ')))
      }
      
      shinyjs::toggleState('prevBtn', cond = PrevBtn_logics())
      shinyjs::toggleState('nextBtn', cond = NextBtn_logics())
      
      if (config$type == 'process'){
        Update_Cursor_position()
        Analyse_status_Process()
      } else if (config$type == 'pipeline')
        Analyse_status_Pipeline()
      
      # Display current page
      if (config$type == 'pipeline')
        # One display all processes, even the validated ones
        lapply(1:current$nbSteps, function(x){
        shinyjs::toggle(paste0('div_screen', x), condition = x==current$val)})
      else if (config$type == 'process')
        # One only displays the steps that are not skipped
        lapply(1:current$nbSteps, function(x){
          shinyjs::toggle(paste0('div_screen', x), condition = x==current$val && config$status[[current$val]] != SKIPPED)})
      
      
    })


    Update_Cursor_position <- reactive({
      if(verbose)
        print(paste0('TL(',config$process.name, ') : Update_Cursor_position() :'))
      n <- current$nbSteps
      initial_status <- setNames(lapply(1:current$nbSteps, 
                                        function(x){ if (x == 1) VALIDATED else UNDONE}), 
                                 names(config$steps))
      is.equal(config$status, initial_status)
      
      if ((config$status[[n]] == VALIDATED))
        current$val <- current$nbSteps
      else if (config$status[[n]] == SKIPPED)
        current$val <- 1
      else if (is.equal(config$status, initial_status))
        current$val <- 1
    })
    
    is.equal <- function(ll1, ll2){
      A <- names(ll1)==names(ll2)
      B <- length(ll1)==length(ll2)
      C <- sum(unlist(ll1)==unlist(ll2))==length(ll1)
      
      A && B && C
    }
   
    
    # This function catches any event on config$status and analyze it
    # to decide whether to disable/enable UI parts
    Analyse_status_Process <- reactive({
      
      #browser()
      SetSkippedStatus()
      initial_status <- setNames(lapply(1:current$nbSteps, 
                                        function(x){ if (x == 1) VALIDATED else UNDONE}), 
                                 names(config$steps))
      is.equal(config$status, initial_status)
      
      if (is.equal(config$status,initial_status)){
        # This is the case at the initialization of a process or after a reset
        if(verbose)
          print(paste0('TL(',config$process.name, ') : Analyse_status() : Init -> Enable all steps'))
          
        # Enable all steps
        toggleState_Steps(cond = TRUE, i = current$nbSteps)
      } else if (config$status[[length(current$nbSteps)]] == SKIPPED){
        # The entire process is skipped
        if(verbose)
          print(paste0('TL(',config$process.name, ') : Analyse_status() : The entire process is skipped'))
        # Disable all steps
        toggleState_Steps(cond = FALSE, i = current$nbSteps)
      } else {
        # Disable all previous steps from each VALIDATED step
        if(verbose)
          print(paste0('TL(',config$process.name, ') : Analyse_status() : Disable all previous steps from each VALIDATED step'))
        ind.max <- max(grep(VALIDATED, unlist(config$status)))
        toggleState_Steps(cond = FALSE, i = ind.max)
      }
    })
      

      
      Analyse_status_Pipeline <- reactive({
        SetSkippedStatus()
    })
    
    
    
    SetSkippedStatus <- reactive({
      if(verbose)
        print(paste0('TL(',config$process.name, ') : SetSkippedStatus()'))
     # browser()
      if (!is.equal(config$status, setNames(lapply(1:nbSteps(),
                                                  function(x){ SKIPPED}),
                                           names(config$steps))))
      config$status[which(config$status==UNDONE)[which(which(config$status == UNDONE ) < GetMaxValidated(config$status, current$nbSteps))]] <- SKIPPED
      else
        print(paste0('TL(',config$process.name, ') : Process entire skipped !!!!!'))
    })
    

    toggleState_Steps <- function(cond, i){
      if(verbose)
        print(paste0('TL(',config$process.name, ') : toggleState_Steps() : cond = ', cond, ', i = ', i))
      
      lapply(1:i, function(x){
        shinyjs::toggleState(paste0('div_screen', x), condition = cond)})
    }

    ##
    ## Functions defining timeline and styles
    ##
    output$load_css_style <- renderUI({
      if(verbose)
        print(paste0('TL(', config$process.name, ') : load_css_style'))
      
      req(current$nbSteps)
      req(style != 3)
      
      file <- paste0('./Timelines/timeline',style, '.sass')
      #code <- code_sass_timeline[[paste0('style',style)]],"\n")
      code <- strsplit(readLines(file),"\n")
      firstLine <- code[[1]][1]
      prefix <- substr(firstLine, 1, unlist(gregexpr(pattern =':',firstLine)))
      suffix <- substr(firstLine, unlist(gregexpr(pattern =';',firstLine)), nchar(firstLine))
      
      code[[1]][1] <- paste0(prefix, current$nbSteps, suffix, collapse='')
      
      shinyjs::inlineCSS( sass::sass(paste(unlist(code), collapse = '')))
   
    })
    
    
    
    #### -----
    ### Definition of timelines style
    output$timeline1 <- renderUI({
      config
      status <- rep('', current$nbSteps)
      status[current$val] <- ' active'
      steps <- names(config$steps)
      txt <- "<div class='flex-parent'> <div class='input-flex-container'>"
      for (i in 1:current$nbSteps){
        txt <- paste0(txt, "<div class='input",status[i], "'><span name='", steps[i],"'></span>  </div>")
      }
      txt <- paste0(txt,"</div></div>")
      HTML(txt)
    })
    
    output$timeline2 <- renderUI({
      if(verbose)
        print(paste0('TL(', config$process.name, ') : timeline2. status = ', paste0(config$status, collapse=' ')))
      
      config
      status <- rep('', current$nbSteps)
  
      if( !is.null(config$steps))
        status[which(unlist(config$steps))] <- 'mandatory'
      
      #status <- rep('',length(config$stepsNames))
      status[which(unlist(config$status) == VALIDATED)] <- 'complete'
      
      #Compute the skipped steps
      status[which(config$status==SKIPPED)] <- 'skipped'
      
      #browser()
      active  <- rep('', current$nbSteps)
      active[current$val] <- 'active'
      
      steps <- names(config$steps)
      txt <- "<ul class='timeline' id='timeline'>"
      for (i in 1:current$nbSteps){
        txt <- paste0(txt, "<li class='li ",status[i]," ",active[i],"'><div class='timestamp'></div><div class='status'><h4>", steps[i],"</h4></div></li>")
      }
      txt <- paste0(txt,"</ul>")
      
      HTML(txt)
    })
    
    
    output$timeline3 <- renderUI({
      config
      
      color <- rep("lightgrey", current$nbSteps)
      colorForCursor <- rep("white", current$nbSteps)
      
      for (i in 1:length(config$stepsNames)){
        status <- config$status[[i]]
        col <- ifelse(!is.null(config$steps) && config$steps[[i]], "red", orangeProstar)
        ifelse(status==VALIDATED, color[i] <- "green", color[i] <- col)
      }
      
      colorForCursor[current$val] <- "black"
      
      steps <- config$steps
      colorCurrentPos <- colorForCursor
      paste0("     ", steps, "     ")
      rows.color <- rows.text <-  rows.cursor <- list()
      rows.text <- list()
      for( i in 1:length( color ) ) {
        rows.color[[i]] <-lapply( color[i], function( x ) tags$th(  style=paste0("background-color:", x,"; height: 20px;" ) ))
        rows.cursor[[i]] <-lapply( colorCurrentPos[i], function( x ) tags$th(  style=paste0("background-color:", x,"; height: 5px;" ) ))
        rows.text[[i]] <- lapply( steps[i], function( x ) tags$td( x ) ) 
      }
      
      html.table <-  tags$table(style = "width: 100%; text-align: center;border: 1;border-collapse: separate;border-spacing: 10px;padding-top: 0px;",
                                tags$tr( rows.color ),
                                tags$tr( rows.cursor ),
                                tags$tr( rows.text )
      )
      
      html.table
    })
    

    
    list(rstBtn = reactive(current$reset_OK),
         prvBtn = reactive(input$prevBtn),
         nxtBtn = reactive(input$nextBtn),
         pos = reactive(current$val)
    )
    
  })
}
