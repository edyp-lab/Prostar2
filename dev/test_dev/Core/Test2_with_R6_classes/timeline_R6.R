#Timeline_R6.R
Timeline = R6Class(
  "Timeline",
  private = list(
    # attributes
    verbose = T,
    val = 1,
    nbSteps = NULL,
    DEFAULT_SKIPPED_POSITION = 1,
    DEFAULT_VALIDATED_POSITION = 1,
    DEFAULT_UNDONE_POSITION = 1
    ),
  
  public = list(
    # attributes
    id = NULL,
    
    # initializer
    initialize = function(id){
      self$id = id
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
    server = function(input, output, session, style=2, config, wake = NULL){
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
      
      
      observeEvent(req(wake()), {
        if(verbose)
          print(paste0('TL(',config$process.name, ') : observeEvent(current$wake() '))
        
        Update_Cursor_position()
        
      })
      
      
      observeEvent(req(config), ignoreInit=F,{
        if(verbose)
          print(paste0('TL(',config$process.name, ') : observeEvent(req(config)() '))
        if (length(config$screens)==0)return(NULL)
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
        req(config$screens)
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
        req(current$nbSteps)
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
      output$timeline2 <- renderUI({
        req(current$nbSteps)
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
      
      
      list(rstBtn = reactive(current$reset_OK),
           prvBtn = reactive(input$prevBtn),
           nxtBtn = reactive(input$nextBtn),
           saveBtn = reactive({input$saveExitBtn}),
           pos = reactive({current$val})
      )
      
    },
    
    # call
    call = function(input, ouput, session, style=2, config, wake = NULL){
      callModule(self$server, self$id, style=2, config, wake = NULL)
    }
  )
)