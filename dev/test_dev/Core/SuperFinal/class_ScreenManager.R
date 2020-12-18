# Use of the tip in this page to unshare reactiveValues between different instances
# of the same class
# https://community.rstudio.com/t/r6-class-reactivevalues-property-and-instantiation/31025/2


redBtnClass <- "btn-danger"
PrevNextBtnClass <- "btn-info"
btn_success_color <- "btn-success"
optionsBtnClass <- "info"

btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"


#' @title Add together two numbers
#' 
#' @description 
#' 
#' @details 
#' 
#' 
#'
#' @param x A number
#' @param y A number
#' @return The sum of \code{x} and \code{y}
#' 
#' @docType class
#' 
#' @import R6
#' 
#' @export
#' 
#' @examples
#' add(1, 1)
ScreenManager <- R6Class(
  "ScreenManager",
  private = list(
    NavPage = function(direction) {
      newval <- self$rv$current.pos + direction 
      newval <- max(1, newval)
      newval <- min(newval, self$length)
      if(newval == 0)
        browser()
      
      self$rv$current.pos <- newval
      cat(paste0('new position = ', self$rv$current.pos, '\n'))
    },
    
    #' @title Default reset actions
    #' 
    #' @description `BasicReset()` execute some functions common to all
    #' child classes.
    #'
    #' @return nothing.
    #' 
    BasicReset = function(){
      if(verbose) cat(paste0(class(self)[1], '::', 'BasicReset() from - ', self$id, '\n\n'))
      private$ResetScreens()
      self$rv$dataIn <- NULL
      self$rv$current.pos <- 1
      private$Initialize_Status_Process()
      private$Send_Result_to_Caller()
    },
    
    #' @title Reset screens
    #' 
    #' @description Set the input widgets of all screens to their default value.
    #'
    #' @return nothing.
    #' 
    ResetScreens = function(){
      if(verbose) cat(paste0(class(self)[1], '::ResetScreens() from - ', self$id, '\n\n'))
      
      lapply(1:self$length, function(x){
        shinyjs::reset(self$config$steps[x])
      })
    },
    
    #' @title Check process/pipeline configuration data.
    #' 
    #' @description `CheckConfig()` checks if the .config property of the children classes is correct.
    #'
    #' @param conf A list of three items (named 'name', 'steps', 'mandatory') which are a private property
    #' of children classes
    #' 
    #' @return A list of two items:
    #' * passed: a boolean indicating whether the config is correct or not,
    #' * msg: a message accompanying the result.
    #' 
    CheckConfig = function(conf){
      if(verbose) cat(paste0(class(self)[1], '::CheckConfig() from - ', self$id, '\n\n'))
      passed <- T
      msg <- ""
      if (!is.list(conf)){
        passed <- F
        msg <- c(msg, "'config' is not a list")
      }
      if (length(conf)!=3){
        passed <- F
        msg <- c(msg, "The length of 'config' is not equal to 4")
      }
      names.conf <- c("name", "steps", "mandatory")
      if (!all(sapply(names.conf, function(x){x %in% names(conf)}))){
        passed <- F
        msg <- c(msg, "The names of elements in 'config' must be the following: 'name', 'steps', 'mandatory'")
      }
      if (length(conf$steps) != length(conf$mandatory)){
        passed <- F
        msg <- c(msg, "The length of 'steps' and 'mandatory' must be equal.")
      }
      
      passed <- T
      list(passed=passed,
           msg = msg)
    },
    
    #' Converts process/pipeline status code to a string.
    #'
    #' @description `GetStringStatus` converts the different values for the status to a string.
    #' 
    #' @param name A number
    #' 
    #' @return A string
    #' 
    GetStringStatus = function(name){
      if (name==global$VALIDATED) "Validated"
      else if (name==global$UNDONE) "Undone"
      else if (name==global$SKIPPED) 'Skipped'
    },
    
    #' Converts the current time to a UNIX format.
    #'
    #' @return A integer
    #' 
    Timestamp = function(){ 
      if(verbose) cat(paste0(class(self)[1], '::Timestamp() from - ', self$id, '\n\n'))
      as.numeric(Sys.time())
    },
    
    #' @title Send the current dataset to the program which has created this instance of the class.
    #' 
    #' @description `Send_Result_to_Caller` updates the return value of the class with the value of the current dataset.
    #' As this return value is stored in a reactive variable, the caller automatically receives the new dataset.
    #'
    #' @return nothing
    #' 
    Send_Result_to_Caller = function(){
      if(verbose) cat(paste0(class(self)[1], '::Send_Result_to_Caller() from - ', self$id, '\n\n'))
      #self$dataOut$value <- self$rv$dataIn
      self$dataOut$trigger <- private$Timestamp()
      self$dataOut$value <- self$rv$dataIn
    },
    
    #' @title Initialize the inner store dataset with the temporary dataset send to the instance.
    #' 
    #' @description `InitializeDataIn` instantiates the variable which is the working variable of 
    #' the current dataset which is to be processed
    #'
    #' @return Nothing.
    #' 
    InitializeDataIn = function(){ 
      if(verbose) cat(paste0(class(self)[1], '::', 'InitializeDataIn() from - ', self$id, '\n\n'))
      self$rv$dataIn <- self$rv$temp.dataIn
    },
    
    
    #' @title Enable/disable screens of the class.
    #' 
    #' @description Update_State_Screens()` checks which screens must be enabled or disabled w.r.t the state 
    #' of the status vector.
    #'
    #' @return Nothing.
    #' 
    Update_State_Screens = function(){
      if(verbose) cat(paste0(class(self)[1], '::', 'Update_State_Screens() from - ', self$id, '\n\n'))
      
      ind.max <- private$GetMaxValidated_AllSteps()
      
      if (ind.max > 0) # No step validated: init or reset of timeline 
        self$ToggleState_Screens(cond = FALSE, range = 1:ind.max)
      
      
      if (ind.max < self$length){
        # Enable all steps after the current one but the ones
        # after the first mandatory not validated
        firstM <- private$GetFirstMandatoryNotValidated((ind.max+1):self$length)
        if (is.null(firstM)){
          self$ToggleState_Screens(cond = TRUE, range = (1 + ind.max):(self$length))
        } else {
          self$ToggleState_Screens(cond = TRUE, range = (1 + ind.max):(ind.max + firstM))
          if (ind.max + firstM < self$length)
            self$ToggleState_Screens(cond = FALSE, range = (ind.max + firstM + 1):self$length)
        }
      }
    },
    
    #' Add together two numbers
    #'
    #' @param x A number
    #' @param y A number
    #' @return The sum of \code{x} and \code{y}
    #' @examples
    #' add(1, 1)
    GetMaxValidated_AllSteps = function(){
      if(verbose) cat(paste0(class(self)[1], '::', 'GetMaxValidated_AllSteps() from - ', self$id, '\n\n'))
      val <- 0
      ind <- grep(global$VALIDATED, self$rv$status)
      if (length(ind) > 0) 
        val <-max(ind)
      val
    },
    
    #' Add together two numbers
    #'
    #' @param x A number
    #' @param y A number
    #' @return The sum of \code{x} and \code{y}
    #' @examples
    #' add(1, 1)
    ToggleState_ResetBtn = function(cond){
      if(verbose) cat(paste0(class(self)[1], '::', 'ToggleState_ResetBtn(', cond, ')) from - ', self$id, '\n\n'))
      
      shinyjs::toggleState(self$ns('rstBtn'), condition = cond)
    },
    
    #' Add together two numbers
    #'
    #' @param x A number
    #' @param y A number
    #' @return The sum of \code{x} and \code{y}
    #' @examples
    #' add(1, 1)
    GetFirstMandatoryNotValidated = function(range){
      if(verbose) cat(paste0(class(self)[1], '::', 'GetFirstMandatoryNotValidated() from - ', self$id, '\n\n'))
      first <- NULL
      first <- unlist((lapply(range, 
                              function(x){self$config$mandatory[x] && !self$rv$status[x]})))
      if (sum(first) > 0)
        min(which(first == TRUE))
      else
        NULL
    },
    
    #' Return the UI for a modal dialog with data selection input. If 'failed' is
    #' TRUE, then display a message that the previous value was invalid.
    #' Add together two numbers
    #'
    #' @param x A number
    #' @param y A number
    #' @return The sum of \code{x} and \code{y}
    #' @examples
    #' add(1, 1)
    dataModal = function() {
      
      tags$div(id="modal1", 
               modalDialog(
                 span(private$modal_txt),
                 footer = tagList(
                   actionButton(self$ns("close"), "Cancel", class='btn-info'),
                   actionButton(self$ns("modal_ok"), "OK")
                 )
               )
      )
    },
    
    #' Add together two numbers
    #'
    #' @param x A number
    #' @param y A number
    #' @return The sum of \code{x} and \code{y}
    #' @examples
    #' add(1, 1)
    Initialize_Status_Process = function(){
      if(verbose) cat(paste0(class(self)[1], '::', 'Initialize_Status_Process() from - ', self$id, '\n\n'))
      self$rv$status <- setNames(rep(global$UNDONE, self$length),self$config$steps)
    },
    
    
    #' Add together two numbers
    #'
    #' @param x A number
    #' @param y A number
    #' @return The sum of \code{x} and \code{y}
    #' @examples
    #' add(1, 1)
    Additional_Initialize_Class = function(){},
    
    #' Add together two numbers
    #'
    #' @param x A number
    #' @param y A number
    #' @return The sum of \code{x} and \code{y}
    #' @examples
    #' add(1, 1)
    GetScreens_server = function(input, output){},
    
    #' Add together two numbers
    #'
    #' @param x A number
    #' @param y A number
    #' @return The sum of \code{x} and \code{y}
    #' @examples
    #' add(1, 1)
    ActionOn_New_DataIn = function(){},
    
    #' Add together two numbers
    #'
    #' @param x A number
    #' @param y A number
    #' @return The sum of \code{x} and \code{y}
    #' @examples
    #' add(1, 1)
    Additional_Server_Funcs = function(){},
    
    #' Add together two numbers
    #'
    #' @param x A number
    #' @param y A number
    #' @return The sum of \code{x} and \code{y}
    #' @examples
    #' add(1, 1)
    ValidateCurrentPos = function(){},
    
    #' Add together two numbers
    #'
    #' @param x A number
    #' @param y A number
    #' @return The sum of \code{x} and \code{y}
    #' @examples
    #' add(1, 1)
    EncapsulateScreens = function(){},
    
    #' Add together two numbers
    #'
    #' @param x A number
    #' @param y A number
    #' @return The sum of \code{x} and \code{y}
    #' @examples
    #' add(1, 1)
    ActionOn_NewPosition = function(){}
    
    
  ),
  public = list(
    
    #' @field id The internal id of the instance.
    id = NULL,
    
    #' @field ns The function NS().
    ns = NULL,
    
    #' @field style A integer coding for the style of timeline.
    style = NULL,
    
    #' @field
    currentStepName = NULL,
    
    #' @field child.process is a list in which each item is an instance of pipeline/process class.
    child.process = NULL,
    
    #' @field length The number of steps in the process/pipeline.
    length = NULL,
    
    #' @field original.length xxxx
    original.length = NULL,
    
    #' @field config xxx
    config = NULL,
    
    #' @field screens xxx
    screens = NULL,
    
    #' @field modal_txt xxx
    modal_txt = NULL,
    
    #' @field timeline xxx
    timeline  = NULL,
    
    #' @field default_pos
    default_pos = list(VALIDATED = 1,
                       SKIPPED = 1,
                       UNDONE = 1),
    
    #' @field dataOut xxx
    dataOut = "<reactiveValues>",
    
    #' @field rv xxx
    rv = "<reactiveValues>",
    
    
    #' @description
    #' xxxx
    #' 
    #' @param id xxx
    #' 
    #' @return xxxx
    #'
    initialize = function(id) {
      if(verbose) cat(paste0(class(self)[1], '::initialize() from - ', self$id, '\n\n'))
      self$id <- id
      self$ns <- NS(id)
      self$dataOut = reactiveValues(
        trigger = 0,
        value = NULL
      )
      
      self$default_pos$VALIDATED <- self$length
      self$default_pos$SKIPPED <- 1
      self$default_pos$UNDONE <- 1
      
      
      
      check <- private$CheckConfig(private$.config)
      if (!check$passed)
        stop(paste0("Errors in 'config'", paste0(check$msg, collapse=' ')))
      else
        self$config <- private$.config
      
      
      self$length <- length(self$config$mandatory)
      self$config$type = class(self)[2]
      self$config$mandatory <- setNames(self$config$mandatory, self$config$steps)
      self$rv = reactiveValues(
        dataIn = NULL,
        temp.dataIn = NULL,
        current.pos = 1,
        status = setNames(rep(global$UNDONE, self$length), self$config$steps),
        tl.tags.enabled = setNames(rep(FALSE, self$length), self$config$steps),
        local.reset = NULL,
        isAllSkipped = FALSE,
        isAllUndone = TRUE,
        isReseted = NULL,
        isSkipped = NULL
        )
      
      
      # Tip seen in: 
      # https://community.rstudio.com/t/reactive-within-r6class-throws-dependents-not-found-error/4973/2
      self$currentStepName <- reactive({self$config$steps[self$rv$current.pos]})
   
      self$rv$status <- setNames(rep(global$UNDONE, self$length), self$config$steps)
      
      self$timeline <- TimelineDraw$new(self$ns('TL_draw'), mandatory = self$config$mandatory)
      
      private$Additional_Initialize_Class()
      self$screens <- self$GetScreens_ui()
    },
    
   
    
    #' @description
    #' xxxx
    #' 
    #' @return xxxx
    #'
    Get_Result = function(){self$dataOut$value},
    
     
    #' @description
    #' 
    #' @param i xxx
    #' 
    #' @return xxxx
    #'
    Change_Current_Pos = function(i){ self$rv$current.pos <- i},
    
    #' @description
    #' 
    #' @param cond xxxx
    #' 
    #' @param range xxx
    #' 
    #' @return xxxx
    #'
    ToggleState_Screens = function(cond, range){},
    
   

    #' @description
    #' xxxx
    #' 
    #' @return xxxx
    #'
   ui = function(){
      if (verbose) cat(paste0(class(self)[1], '::', 'Main_UI() from - ', self$id, '\n\n'))
      #browser()
      tagList(
        shinyjs::useShinyjs(),
        # tags$head(tags$style("#modal1 .modal-body {padding: 10px}
        #                #modal1 .modal-content  {-webkit-border-radius: 6px !important;-moz-border-radius: 6px !important;border-radius: 6px !important;}
        #                #modal1 .modal-dialog { width: 50%; display: inline-block; text-align: left; vertical-align: top;}
        #                #modal1 .modal-header {background-color: #339FFF; border-top-left-radius: 6px; border-top-right-radius: 6px}
        #                #modal1 .modal { text-align: right; padding-right:10px; padding-top: 24px;}
        #                #moda1 .close { font-size: 16px}")),
        #div(id = self$ns('GlobalTL'),
        fluidRow(
          align= 'center',
          column(width=2, div(id = self$ns('TL_LeftSide'),
                              style = self$btn_style,
                              shinyjs::disabled(actionButton(self$ns("prevBtn"), "<<",
                                                             class = PrevNextBtnClass,
                                                             style='padding:4px; font-size:80%')),
                              shinyjs::disabled(actionButton(self$ns("rstBtn"), paste0("Reset ", self$config$type),
                                                             class = redBtnClass,
                                                             style='padding:4px; font-size:80%'))
          )
          ),
          column(width=8, div(id = self$ns('TL_Center'),
                              style = self$btn_style,
                              self$timeline$ui())),
          column(width=2, div(id = self$ns('TL_RightSide'),
                              style = self$btn_style,
                              actionButton(self$ns("nextBtn"),
                                           ">>",
                                           class = PrevNextBtnClass,
                                           style='padding:4px; font-size:80%')
          )
          )
        ),
        
        div(id = self$ns('Screens'),
            uiOutput(self$ns('SkippedInfoPanel')),
            private$EncapsulateScreens()
        ),
        fluidRow(
          column(width=2,
                 tags$b(h4(style = 'color: blue;', paste0("Global input of ", self$config$type))),
                 uiOutput(self$ns('show_dataIn'))),
          column(width=2,
                 tags$b(h4(style = 'color: blue;', paste0("Temp input of ", self$config$type))),
                 uiOutput(self$ns('show_rv_dataIn'))),
          column(width=2,
                 tags$b(h4(style = 'color: blue;', paste0("Output of ", self$config$type))),
                 uiOutput(self$ns('show_rv_dataOut'))),
          column(width=4,
                 tags$b(h4(style = 'color: blue;', "status")),
                 uiOutput(self$ns('show_status')))
        )
        
      )
      #)
    },
    
    
    ###############################################################
    ###                          SERVER                         ###
    ###############################################################
   #' @description
   #' 
   #' @param dataIn xxx
   #' 
   #' @return xxxx
   #'
   server = function(dataIn = reactive({NULL})) {
      if (verbose) cat(paste0(class(self)[1], '::server(dataIn) from - ', self$id, '\n\n'))

      self$timeline$server(status = reactive({self$rv$status}),
                           position = reactive({self$rv$current.pos}),
                           enabled = reactive({self$rv$tl.tags.enabled})
      )
      
      #
      # Catch a new dataset sent by the caller
      #
      observeEvent(dataIn(), ignoreNULL = F, ignoreInit = T,{
        if (verbose) cat(paste0(class(self)[1], '::observeEvent(dataIn()) from --- ', self$id, '\n\n'))
       # browser()
        self$Change_Current_Pos(1)
        self$rv$temp.dataIn <- dataIn()
        private$ActionOn_New_DataIn() # Used by class pipeline
        
        
        if(is.null(dataIn())){
          self$ToggleState_Screens(FALSE, 1:self$length)
         # self$ToggleState_ResetBtn(FALSE)
          self$original.length <- 0
        } else { # A new dataset has been loaded
          private$ToggleState_ResetBtn(TRUE) #Enable the reset button
          self$original.length <- length(dataIn())
          
          private$Update_State_Screens()
        }
      })
      
      # Catch new status event
      
      observeEvent(self$rv$status, ignoreInit = T, {
        if (verbose) cat(paste0(class(self)[1], '::observe((self$rv$status) from - ', self$id, '\n\n'))
        private$Discover_Skipped_Steps()
        private$Update_State_Screens()

      })
      
      
      observeEvent(self$rv$current.pos, ignoreInit = T,{
        if (verbose) cat(paste0(class(self)[1], '::observe(self$rv$current.pos) from - ', self$id, '\n\n'))
        
        shinyjs::toggleState(id = self$ns("prevBtn"), condition = self$rv$current.pos > 1)
        shinyjs::toggleState(id = self$ns("nextBtn"), condition = self$rv$current.pos < self$length)
        shinyjs::hide(selector = paste0(".page_", self$id))
        shinyjs::show(self$ns(self$config$steps[self$rv$current.pos]))
        
        private$ActionOn_NewPosition()
        
      })
      
     private$Additional_Server_Funcs()
      
      ###############################################################
      ###                    MODULE SERVER                        ###
      ###############################################################
      moduleServer(self$id, function(input, output, session) {
        if (verbose) cat(paste0(class(self)[1], '::moduleServer(input, output, session) from - ', self$id, '\n\n'))
        
        #Used to get the observeEvent functions
        private$GetScreens_server(input, output)
        
        observeEvent(input$rstBtn, {
          if (verbose) cat(paste0(class(self)[1], '::observeEvent(input$rstBtn) from - ', self$id, '\n\n'))
          showModal(private$dataModal())
        })
        
        observeEvent(input$close, {removeModal() })
        
        observeEvent(req(input$modal_ok > 0), ignoreInit=F, {
          if (verbose) cat(paste0(class(self)[1], '::observeEvent(req(c(input$modal_ok))) from - ', self$id, '\n\n'))
          self$rv$local.reset <- input$rstBtn
          self$Set_All_Reset()
          removeModal()
        })
        
        output$SkippedInfoPanel <- renderUI({
          if (verbose) cat(paste0(class(self)[1], '::output$SkippedInfoPanel from - ', self$id, '\n\n'))
          #browser()
          
          current_step_skipped <- self$rv$status[self$rv$current.pos] == global$SKIPPED
          entire_process_skipped <- isTRUE(sum(self$rv$status) == global$SKIPPED * self$length)
          req(current_step_skipped)
          
          
          if (entire_process_skipped){
            # This case appears when the process has been skipped from the
            # pipleine. Thus, it is not necessary to show the info box because
            # it is shown below the timeline of the pipeline
          } else {
            txt <- paste0("This ", self$config$type, " is skipped so it has been disabled.")
            wellPanel(
            style = "background-color: #7CC9F0; opacity: 0.72; padding: 0px; align: center; vertical-align: center;",
            height = 100,
            width=300,
            align="center",
            p(style = "color: black;", paste0('Info: ',txt))
          )
          }
        })
        
        observeEvent(input$prevBtn, ignoreInit = TRUE, {private$NavPage(-1)})
        observeEvent(input$nextBtn, ignoreInit = TRUE, {private$NavPage(1)})
        
        
        ###########---------------------------#################
        output$show_dataIn <- renderUI({
          if (verbose) cat(paste0(class(self)[1], '::output$show_dataIn from - ', self$id, '\n\n'))
          req(dataIn())
          tagList(
            # h4('show dataIn()'),
            lapply(names(dataIn()), function(x){tags$p(x)})
          )
        })
        
        output$show_rv_dataIn <- renderUI({
          if (verbose) cat(paste0(class(self)[1], '::output$show_rv_dataIn from - ', self$id, '\n\n'))
          req(self$rv$dataIn)
          tagList(
            # h4('show dataIn()'),
            lapply(names(self$rv$dataIn), function(x){tags$p(x)})
          )
        })
        
        output$show_rv_dataOut <- renderUI({
          if (verbose) cat(paste0(class(self)[1], '::output$show_rv_dataOut from - ', self$id, '\n\n'))
          tagList(
            #h4('show self$dataOut$value'),
            lapply(names(self$dataOut$value), function(x){tags$p(x)})
          )
        })
        
        
        output$show_status <- renderUI({
          tagList(lapply(1:self$length, 
                         function(x){
                           color <- if(self$rv$tl.tags.enabled[x]) 'black' else 'lightgrey'
                           if (x == self$rv$current.pos)
                             tags$p(style = paste0('color: ', color, ';'),
                                    tags$b(paste0('---> ', self$config$steps[x], ' - ', private$GetStringStatus(self$rv$status[[x]])), ' <---'))
                           else 
                             tags$p(style = paste0('color: ', color, ';'),
                                    paste0(self$config$steps[x], ' - ', private$GetStringStatus(self$rv$status[[x]])))
                         }))
        })
        
        reactive({self$dataOut})
      })
    }
)
)
