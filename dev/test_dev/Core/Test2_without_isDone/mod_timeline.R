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
    div(id = 'GlobalTL',
    fluidRow(
      align= 'center',
      column(width=2,div(style=btn_style,
                         uiOutput(ns('showPrevBtn')),
                         uiOutput(ns('showResetBtn'))
      )),
      column(width=8,div( style = btn_style, uiOutput(ns("timelineStyle"))) ),
      column(width=2,div(style=btn_style,
                         uiOutput(ns('showNextBtn')),
                         uiOutput(ns('showSaveExitBtn'))
                         )
      )
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
mod_timeline_server <- function(id, style=2, config, onlyReset=NULL, showSaveBtn = FALSE, wake = NULL){
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    verbose = T
    source(file.path('.', 'code_general.R'), local=TRUE)$value
    
    # output$showSaveExitBtn <- renderUI({
    #   req(showSaveBtn)
    #   print(paste0('TL(',config$process.name, ') : output$showSaveExitBtn <- renderUI'))
    #   actionButton(ns("saveExitBtn"), paste0("Save & Exit ", config$type),
    #                class = btn_success_color,
    #                style='padding:4px; font-size:80%')
    # })
    
    output$showResetBtn <- renderUI({
      print(paste0('TL(',config$process.name, ') : output$showResetBtn <- renderUI'))
      actionButton(ns("rstBtn"), paste0("Reset ", config$type),
                   class = redBtnClass,
                   style='padding:4px; font-size:80%')
      })
    
    output$showPrevBtn <- renderUI({
      req(!isTRUE(onlyReset))
      shinyjs::disabled(actionButton(ns("prevBtn"), "<<",
                                     class = PrevNextBtnClass,
                                     style='padding:4px; font-size:80%'))
    })
    
    output$showNextBtn <- renderUI({
      req(!isTRUE(onlyReset))
      shinyjs::disabled(actionButton(ns("nextBtn"), "next",
                   class = PrevNextBtnClass,
                   style='padding:4px; font-size:80%'))
    })
    
    output$timelineStyle <- renderUI({ 
      req(!isTRUE(onlyReset))
      uiOutput(ns(paste0('timeline', style))) })
    
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
      DEFAULT_SKIPPED_POSITION = 1,
      DEFAULT_VALIDATED_POSITION = 1,
      DEFAULT_UNDONE_POSITION = 1,
      # Transit variable to manage the clics on the reset button
      reset_OK = FALSE
    )
    
    
    observeEvent(req(wake()), {
      if(verbose)
        print(paste0('TL(',config$process.name, ') : observeEvent(current$wake() '))
     
      Update_Cursor_position()
      
    })
    
    
    observeEvent(req(config), ignoreInit=F,{
      if(verbose)
        print(paste0('TL(',config$process.name, ') : observeEvent(req(config)() '))
     InitScreens()
    })
    

    Init_Default_Positions <- reactive({
      current$DEFAULT_VALIDATED_POSITION <- current$nbSteps
      current$DEFAULT_SKIPPED_POSITION <- current$nbSteps
      current$DEFAULT_UNDONE_POSITION <- 1
    })
    
    
    # Initialization of the screens by integrating them into a div specific
    # to this module (name prefixed with the ns() function
    # Those div englobs the div of the caller where screens are defined
    InitScreens <- reactive({
      if(verbose)
        print(paste0('TL(',config$process.name, ') : call to InitScreens() '))
      
      current$nbSteps <- length(config$steps)
      Init_Default_Positions() 
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
      if(newval == 0)
        browser()
        
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
    
    
    Update_Buttons <- reactive({
      
      shinyjs::toggleState('prevBtn', cond = PrevBtn_logics())
      shinyjs::toggleState('nextBtn', cond = NextBtn_logics())
      
    })
    
    # Catch a new position or a change in the status list
    observeEvent(req(c(current$val, config$status)), {
      if(verbose){
        print(paste0('TL(',config$process.name, ') : observeEvent(req(c(current$val, config$status)) : '))
        print(paste0('TL(',config$process.name, ') : status = ', paste0(config$status, collapse=' ')))
        print(paste0('TL(',config$process.name, ') : PrevBtn_logics() = ', PrevBtn_logics(), ', NextBtn_logics() = ', NextBtn_logics()))
      }
     # browser()
      
      if (config$type == 'process'){
       # Update_Cursor_position()
        Analyse_status_Process()
        
        # One only displays the steps that are not skipped
        lapply(1:current$nbSteps, function(x){
          shinyjs::toggle(paste0('div_screen', x), condition = x==current$val && config$status[[current$val]] != SKIPPED)})
        
      } else if (config$type == 'pipeline'){
        #Analyse_status_Pipeline()
        
        # Display current page
        # One display all processes, even the validated ones
        lapply(1:current$nbSteps, function(x){
          shinyjs::toggle(paste0('div_screen', x), condition = x==current$val)})
      }

      Update_Buttons()
      
    })


    Update_Cursor_position <- function(){
      req(current$nbSteps)
      
      if(verbose)
        print(paste0('TL(',config$process.name, ') : Update_Cursor_position() :'))

      #browser()
      
      if ((config$status[[current$nbSteps]] == VALIDATED))
        current$val <- current$DEFAULT_VALIDATED_POSITION
      else if (config$status[[current$nbSteps]] == SKIPPED)
        current$val <- current$DEFAULT_SKIPPED_POSITION
      else if (config$status[[current$nbSteps]] == UNDONE)
        current$val <- current$DEFAULT_UNDONE_POSITION
      if(current$val==0)
        browser()
    }
   
    
    # This function catches any event on config$status and analyze it
    # to decide whether to disable/enable UI parts
    Analyse_status_Process <- reactive({
      
      if ((length(config$status)==1) || (length(config$status)>=2 && sum(unlist(config$status)[2:current$nbSteps])== 0 )){
        # This is the case at the initialization of a process or after a reset
        if(verbose)
          print(paste0('TL(',config$process.name, ') : Analyse_status() : Init -> Enable all steps'))
          
        # Enable all steps and buttons
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
      req(!isTRUE(onlyReset))
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
      status[which(unlist(config$status) == SKIPPED)] <- 'skipped'
      
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
         pos = reactive({current$val})
    )
    
  })
}
