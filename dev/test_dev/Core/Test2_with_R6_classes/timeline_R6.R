#Timeline_R6.R
Timeline = R6Class(
  "Timeline",
  private = list(
    # attributes
    verbose = T,
    style = 2,
    length = NULL,
    modal_txt = NULL,
    DEFAULT_SKIPPED_POSITION = 1,
    DEFAULT_VALIDATED_POSITION = 1,
    DEFAULT_UNDONE_POSITION = 1,
    reset_OK = 0,
    
    
    CheckConfig = function(conf){
      passed <- T
      msg <- ""
      if (!is.list(conf)){
        passed <- F
        msg <- c(msg, "'config' is not a list")
      }
      if (length(config)!=3){
        passed <- F
        msg <- c(msg, "The length of 'config' is not equal to 4")
      }
      names.conf <- c("process.name", "type", "steps")
      if (!all(sapply(names.conf, function(x){x %in% names(config)}))){
        passed <- F
        msg <- c(msg, "The names of elements in 'config' must be the following: 'process.name', 'type', 'steps'")
      }
      if (!is.list(conf$steps)){
        passed <- F
        msg <- c(msg, "The 'steps' slot is not a list")
      }
      
      list(passed=passed,
           msg = msg)
    },
    
    
    toggleState_Steps = function(cond, i){
      if(private$verbose)
        print(paste0('TL(',config$process.name, ') : toggleState_Steps() : cond = ', cond, ', i = ', i))
      
      lapply(1:i, function(x){
        shinyjs::toggleState(paste0('div_screen', x), condition = cond)})
    },
    
    Analyse_status_Process = function(){}
    
    
    
    # # This function catches any event on config$status and analyze it
    # # to decide whether to disable/enable UI parts
    # Analyse_status_Process = function(){
    #   if(private$verbose)
    #     print(paste0('TL(',config$process.name, ') : Analyse_status_Process() :'))
    #   
    #   if ((length(config$status)==1) || (length(config$status)>=2 && sum(unlist(config$status)[2:private$length])== 0 )){
    #     # This is the case at the initialization of a process or after a reset
    #     if(private$verbose)
    #       print(paste0('TL(',config$process.name, ') : Analyse_status() : Init -> Enable all steps'))
    #     
    #     # Enable all steps and buttons
    #     private$toggleState_Steps(cond = TRUE, i = private$length)
    #   } else if (config$status[[length(private$length)]] == SKIPPED){
    #     # The entire process is skipped
    #     if(private$verbose)
    #       print(paste0('TL(',config$process.name, ') : Analyse_status() : The entire process is skipped'))
    #     # Disable all steps
    #     private$toggleState_Steps(cond = FALSE, i = private$length)
    #   } else {
    #     # Disable all previous steps from each VALIDATED step
    #     if(private$verbose)
    #       print(paste0('TL(',config$process.name, ') : Analyse_status() : Disable all previous steps from each VALIDATED step'))
    #     ind.max <- max(grep(VALIDATED, unlist(config$status)))
    #     private$toggleState_Steps(cond = FALSE, i = ind.max)
    #   }
    # }
    
    
    ),
  
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
        modalDialog(
          span(private$modal_txt),
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
        
        if (!private$CheckConfig(config)$passed)
          stop(paste0("Errors in 'config'", paste0(private$CheckConfig(config)$msg, collapse=' ')))
        
        InitScreens()
      })
      
      
      Init_Default_Positions <- reactive({
        private$DEFAULT_VALIDATED_POSITION <- private$length
        private$DEFAULT_SKIPPED_POSITION <- private$length
        private$DEFAULT_UNDONE_POSITION <- 1
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
      
      
      Update_Buttons <- reactive({
        # Compute status for the Next button
        end_of_tl <- self$rv$current.pos == private$length
        mandatory_step <- isTRUE(config$steps[[self$rv$current.pos]])
        validated <- config$status[[self$rv$current.pos]] == VALIDATED
        skipped <- config$status[[self$rv$current.pos]] == SKIPPED
        entireProcessSkipped <- config$status[[private$length]] == SKIPPED
        NextBtn_logics <- !end_of_tl && !entireProcessSkipped && (!mandatory_step || (mandatory_step && (validated || skipped)))
        
        # Compute status for the Previous button
        start_of_tl <- self$rv$current.pos == 1
        entireProcessSkipped <- config$status[[private$length]] == SKIPPED
        PrevBtn_logics <- !start_of_tl && !entireProcessSkipped
        
        shinyjs::toggleState('prevBtn', cond = PrevBtn_logics)
        shinyjs::toggleState('nextBtn', cond = NextBtn_logics)
      })
      
      # Catch a new position or a change in the status list
      observeEvent(req(c(self$rv$current.pos, config$status)), {
        req(private$length)
        if(private$verbose){
          print(paste0('TL(',config$process.name, ') : observeEvent(req(c(self$rv$current.pos, config$status)) : '))
          print(paste0('TL(',config$process.name, ') : status = ', paste0(config$status, collapse=' ')))
          }
        # browser()
        
        private$Analyse_status()
        
        Update_Buttons()
      })
      
      
      Update_Cursor_position <- function(){
        req(private$length)
        
        if(private$verbose)
          print(paste0('TL(',config$process.name, ') : Update_Cursor_position() :'))
        
        if ((config$status[[private$length]] == VALIDATED))
          self$rv$current.pos <- private$DEFAULT_VALIDATED_POSITION
        else if (config$status[[private$length]] == SKIPPED)
          self$rv$current.pos <- private$DEFAULT_SKIPPED_POSITION
        else if (config$status[[private$length]] == UNDONE)
          self$rv$current.pos <- private$DEFAULT_UNDONE_POSITION
        if(self$rv$current.pos==0)
          browser()
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
    
    SetModalTxt = function(txt){private$modal_txt <- txt},
    
    # call
    call = function(input, ouput, session, config=NULL, wake = NULL){
      if(missing(config))
        stop("'config' is required")
      
      
      callModule(self$server, self$id, config, wake = NULL)
    }
  )
)