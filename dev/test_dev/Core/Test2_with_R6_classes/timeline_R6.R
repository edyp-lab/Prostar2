#Timeline_R6.R
Timeline = R6Class(
  "Timeline",
  private = list(
    # attributes
    verbose = T,
    style = 2,
    DEFAULT_SKIPPED_POSITION = 1,
    DEFAULT_VALIDATED_POSITION = 1,
    DEFAULT_UNDONE_POSITION = 1,
    reset_OK = 0),
  
  public = list(
    # attributes
    id = NULL,
    
    rv = reactiveValues(
      rst_btn = NULL,
      position = 0,
      current.pos = 1
    ),
    # initializer
    initialize = function(id, style=2 ){
      self$id = id
      private$style = style
    },
    
    # UI
    ui = function(){
      
      # the ns function here will prepend a prefix to all the ids in the app.
      ns = NS(self$id)
      
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
    },
    
    # server
    server = function(input, output, session, config, wake){
      # the ns function here will prepend a prefix to all the ids in the app.
      ns = NS(self$id)
      
      
      output$showResetBtn <- renderUI({
        print(paste0('TL(',config$process.name, ') : output$showResetBtn <- renderUI'))
        actionButton(ns("rstBtn"), paste0("Reset ", config$type),
                     style='padding:4px; font-size:80%')
      })
      
      output$showPrevBtn <- renderUI({
        shinyjs::disabled(actionButton(ns("prevBtn"), "<<",
                                       style='padding:4px; font-size:80%'))
      })
      
      output$showNextBtn <- renderUI({
         shinyjs::disabled(actionButton(ns("nextBtn"), "next",
                                       style='padding:4px; font-size:80%'))
      })
      
      output$timelineStyle <- renderUI({ 
        uiOutput(ns(paste0('timeline', private$style))) })
      
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
        private$reset_OK <- input$rstBtn
        removeModal()
      })
      
      #----------------------------------------------------------
      
      
      
      
      Length <- reactive({
        req(config$status)
        base::length(config$status)
      })
      
      navPage <- function(direction) {
        newval <- self$rv$current.pos + direction 
        newval <- max(1, newval)
        newval <- min(newval, private$length)
        if(newval == 0)
          browser()
        
        self$rv$current.pos <- newval
      }
      
      
      observeEvent(req(wake), {
        if(private$verbose)
          print(paste0('TL(',config$process.name, ') : observeEvent(current$wake() '))
        
        Update_Cursor_position()
      })
      
      
      observeEvent(req(config), ignoreInit=F,{
        if(private$verbose)
          print(paste0('TL(',config$process.name, ') : observeEvent(req(config)() '))
        if (length(config$screens)==0)return(NULL)
        InitScreens()
      })
      
      
      Init_Default_Positions <- reactive({
        current$DEFAULT_VALIDATED_POSITION <- Length()
        current$DEFAULT_SKIPPED_POSITION <- Length()
        current$DEFAULT_UNDONE_POSITION <- 1
      })
      
      
      # Initialization of the screens by integrating them into a div specific
      # to this module (name prefixed with the ns() function
      # Those div englobs the div of the caller where screens are defined
      InitScreens <- reactive({
        if(private$verbose)
          print(paste0('TL(',config$process.name, ') : call to InitScreens() '))
        req(config$screens)
        private$length <- length(config$steps)
        Init_Default_Positions() 
        config$screens <- lapply(1:private$length,
                                 function(x){
                                   config$screens[[x]] <- if (x == 1) 
                                     div(id = ns(paste0("div_screen", x)),  config$screens[[x]])
                                   else 
                                     shinyjs::hidden(div(id = ns(paste0("div_screen", x)),  config$screens[[x]]))
                                 })
      })
      
      
      
     
      
      observeEvent(input$prevBtn, ignoreInit = TRUE, {navPage(-1)})
      observeEvent(input$nextBtn, ignoreInit = TRUE, {navPage(1)})
      
      
      output$show_screens <- renderUI({tagList(config$screens)})
      
      # Builds the condition to enable/disable the next button
      NextBtn_logics <- reactive({
        end_of_tl <- self$rv$current.pos == private$length()
        mandatory_step <- isTRUE(config$steps[[self$rv$current.pos]])
        validated <- config$status[[self$rv$current.pos]] == VALIDATED
        skipped <- config$status[[self$rv$current.pos]] == SKIPPED
        entireProcessSkipped <- config$status[[private$length]] == SKIPPED
        !end_of_tl && !entireProcessSkipped && (!mandatory_step || (mandatory_step && (validated || skipped)))
      })
      
      # Builds the condition to enable/disable the 'Previous' button
      PrevBtn_logics <- reactive({
        start_of_tl <- self$rv$current.pos == 1
        entireProcessSkipped <- config$status[[private$length]] == SKIPPED
        
        !start_of_tl && !entireProcessSkipped
      })
      
      
      Update_Buttons <- reactive({
        
        shinyjs::toggleState('prevBtn', cond = PrevBtn_logics())
        shinyjs::toggleState('nextBtn', cond = NextBtn_logics())
        
      })
      
      # Catch a new position or a change in the status list
      observeEvent(req(c(self$rv$current.pos, config$status)), {
        req(private$length)
        if(private$verbose){
          print(paste0('TL(',config$process.name, ') : observeEvent(req(c(self$rv$current.pos, config$status)) : '))
          print(paste0('TL(',config$process.name, ') : status = ', paste0(config$status, collapse=' ')))
          print(paste0('TL(',config$process.name, ') : PrevBtn_logics() = ', PrevBtn_logics(), ', NextBtn_logics() = ', NextBtn_logics()))
        }
        # browser()
        
        if (config$type == 'process'){
          # Update_Cursor_position()
          Analyse_status_Process()
          
          # One only displays the steps that are not skipped
          lapply(1:private$length, function(x){
            shinyjs::toggle(paste0('div_screen', x), condition = x==self$rv$current.pos && config$status[[self$rv$current.pos]] != SKIPPED)})
          
        } else if (config$type == 'pipeline'){
          #Analyse_status_Pipeline()
          
          # Display current page
          # One display all processes, even the validated ones
          lapply(1:private$length, function(x){
            shinyjs::toggle(paste0('div_screen', x), condition = x==self$rv$current.pos)})
        }
        
        Update_Buttons()
        
      })
      
      
      Update_Cursor_position <- function(){
        req(private$length)
        
        if(private$verbose)
          print(paste0('TL(',config$process.name, ') : Update_Cursor_position() :'))
        
        #browser()
        
        if ((config$status[[private$length]] == VALIDATED))
          self$rv$current.pos <- current$DEFAULT_VALIDATED_POSITION
        else if (config$status[[private$length]] == SKIPPED)
          self$rv$current.pos <- current$DEFAULT_SKIPPED_POSITION
        else if (config$status[[private$length]] == UNDONE)
          self$rv$current.pos <- current$DEFAULT_UNDONE_POSITION
        if(self$rv$current.pos==0)
          browser()
      }
      
      
      # This function catches any event on config$status and analyze it
      # to decide whether to disable/enable UI parts
      Analyse_status_Process <- reactive({
        
        if ((length(config$status)==1) || (length(config$status)>=2 && sum(unlist(config$status)[2:private$length])== 0 )){
          # This is the case at the initialization of a process or after a reset
          if(private$verbose)
            print(paste0('TL(',config$process.name, ') : Analyse_status() : Init -> Enable all steps'))
          
          # Enable all steps and buttons
          toggleState_Steps(cond = TRUE, i = private$length)
        } else if (config$status[[length(private$length)]] == SKIPPED){
          # The entire process is skipped
          if(private$verbose)
            print(paste0('TL(',config$process.name, ') : Analyse_status() : The entire process is skipped'))
          # Disable all steps
          toggleState_Steps(cond = FALSE, i = private$length)
        } else {
          # Disable all previous steps from each VALIDATED step
          if(private$verbose)
            print(paste0('TL(',config$process.name, ') : Analyse_status() : Disable all previous steps from each VALIDATED step'))
          ind.max <- max(grep(VALIDATED, unlist(config$status)))
          toggleState_Steps(cond = FALSE, i = ind.max)
        }
      })
      
      
      
      
      
      
      toggleState_Steps <- function(cond, i){
        if(private$verbose)
          print(paste0('TL(',config$process.name, ') : toggleState_Steps() : cond = ', cond, ', i = ', i))
        
        lapply(1:i, function(x){
          shinyjs::toggleState(paste0('div_screen', x), condition = cond)})
      }
      
      ##
      ## Functions defining timeline and styles
      ##
      output$load_css_style <- renderUI({
        if(private$verbose)
          print(paste0('TL(', config$process.name, ') : load_css_style'))
        
        req(private$length)
        req(private$style != 3)
        
        file <- paste0('./Timelines/timeline',private$style, '.sass')
        #code <- code_sass_timeline[[paste0('private$style',private$style)]],"\n")
        code <- strsplit(readLines(file),"\n")
        firstLine <- code[[1]][1]
        prefix <- substr(firstLine, 1, unlist(gregexpr(pattern =':',firstLine)))
        suffix <- substr(firstLine, unlist(gregexpr(pattern =';',firstLine)), nchar(firstLine))
        
        code[[1]][1] <- paste0(prefix, private$length, suffix, collapse='')
        
        shinyjs::inlineCSS( sass::sass(paste(unlist(code), collapse = '')))
        
      })
      
      
      
      #### -----
      ### Definition of timelines style
      output$timeline2 <- renderUI({
        req(private$length)
        if(private$verbose)
          print(paste0('TL(', config$process.name, ') : timeline2. status = ', paste0(config$status, collapse=' ')))
        
        config
        status <- rep('', private$length)
        
        if( !is.null(config$steps))
          status[which(unlist(config$steps))] <- 'mandatory'
        
        #status <- rep('',length(config$stepsNames))
        status[which(unlist(config$status) == VALIDATED)] <- 'complete'
        
        #Compute the skipped steps
        status[which(unlist(config$status) == SKIPPED)] <- 'skipped'
        
        #browser()
        active  <- rep('', private$length)
        active[self$rv$current.pos] <- 'active'
        
        steps <- names(config$steps)
        txt <- "<ul class='timeline' id='timeline'>"
        for (i in 1:private$length){
          txt <- paste0(txt, "<li class='li ",status[i]," ",active[i],"'><div class='timestamp'></div><div class='status'><h4>", steps[i],"</h4></div></li>")
        }
        txt <- paste0(txt,"</ul>")
        
        HTML(txt)
      })
    },
    
    GetCurrentPosition = function(){invisible(self$rv$current.pos)},
    
    GetResetAction = function(){invisible(private$reset_OK)},
    
    
    # call
    call = function(input, ouput, session, config, wake = NULL){
      callModule(self$server, self$id, config, wake = NULL)
    }
  )
)