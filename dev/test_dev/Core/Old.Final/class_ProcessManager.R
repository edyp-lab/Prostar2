# Use of the tip in this page to unshare reactiveValues between different instances
# of the same class
# https://community.rstudio.com/t/r6-class-reactivevalues-property-and-instantiation/31025/2


redBtnClass <- "btn-danger"
PrevNextBtnClass <- "btn-info"
btn_success_color <- "btn-success"
optionsBtnClass <- "info"

btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"




ProcessManager <- R6Class(
  "ProcessManager",
  private = list(),
  public = list(
    
    # Declaration of variables
    input = NULL,
    id = NULL,
    ns = NULL,
    style = NULL,
    child.process = NULL,
    length = NULL,
    config = NULL,
    screens = NULL,
    modal_txt = NULL,
    timelineDraw  = NULL,
    default_pos = list(VALIDATED = 1,
                       SKIPPED = 1,
                       UNDONE = 1),
    
    dataOut = "<reactiveValues>",
    rv = "<reactiveValues>",
    
    
    # Initialize class
    initialize = function(id) {
      cat(paste0(class(self)[1], '::initialize() from - ', self$id, '\n'))
      self$id <- id
      self$ns <- NS(id)
      
      self$dataOut = reactiveValues(
        trigger = 0
      )
      
      self$default_pos$VALIDATED <- self$length
      self$default_pos$SKIPPED <- 1
      self$default_pos$UNDONE <- 1
      
      
      
      check <- self$CheckConfig(private$.config)
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
        reset_OK = NULL,
        isAllSkipped = FALSE,
        isAllUndone = TRUE,
        isReseted = NULL,
        isSkipped = NULL)
      
      
      self$rv$status <- setNames(rep(global$UNDONE, self$length), self$config$steps)
      
      self$screens <- self$GetScreensDefinition()
      
      self$Additional_Initialize_Class()
      
      self$timelineDraw <- TimelineDraw$new(self$ns('TL_draw'), 
                                            mandatory = self$config$mandatory)
      
     },
    
    Additional_Initialize_Class = function(){},
    
    CheckConfig = function(conf){
      cat(paste0(class(self)[1], '::CheckConfig() from - ', self$id, '\n'))
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
    
    #GetScreensDefinition = function(){},
    
    Timestamp = function(){ 
      cat(paste0(class(self)[1], '::Timestamp() from - ', self$id, '\n'))
      as.numeric(Sys.time())
    },
    
    Send_Result_to_Caller = function(){
      cat(paste0(class(self)[1], '::Send_Result_to_Caller() from - ', self$id, '\n'))
      #self$dataOut$value <- self$rv$dataIn
      self$dataOut$trigger <- self$Timestamp()
      #self$dataOut$name <- self$name
    },
    
    Get_Result = function(){
      self$rv$dataIn
    },
    
    InitializeDataIn = function(){ 
      cat(paste0(class(self)[1], '::', 'InitializeDataIn() from - ', self$id, '\n'))
      if (verbose==T) browser()
      self$rv$dataIn <- self$rv$temp.dataIn
    },
    
    GetMaxValidated_AllSteps = function(){
      cat(paste0(class(self)[1], '::', 'GetMaxValidated_AllSteps() from - ', self$id, '\n'))
      val <- 0
      ind <- which(self$rv$status == global$VALIDATED)
      if (length(ind) > 0)
        val <- max(ind)
      val
    },
    
   
    Initialize_Status_Process = function(){
      cat(paste0(class(self)[1], '::', 'Initialize_Status_Process() from - ', self$id, '\n'))
      self$rv$status <- setNames(rep(global$UNDONE, self$length),self$config$steps)
    },
    
    ActionOn_Reset = function(){
      cat(paste0(class(self)[1], '::', 'ActionsOnReset() from - ', self$id, '\n'))
      #browser()
      
      self$ResetScreens()
      self$rv$dataIn <- NULL
      self$Initialize_Status_Process()
      self$Send_Result_to_Caller()
      #self$InitializeDataIn()
    },
    
    # This function cannot be implemented in the timeline module because 
    # the id of the screens to reset are not known elsewhere.
    # Trying to reset the global 'div_screens' in the timeline module
    # does not work
    ResetScreens = function(){
      cat(paste0(class(self)[1], '::ResetScreens() from - ', self$id, '\n'))
      
      lapply(1:self$length, function(x){
        shinyjs::reset(self$config$steps[x])
      })
    },
    

    SetSkipped = function(skip){self$rv$isSkipped <- skip},
    SetReseted = function(reset){self$rv$isReseted <- reset},
    
    ValidateCurrentPos = function(){
      cat(paste0(class(self)[1], '::', 'ValidateCurrentPos() from - ', self$id, '\n'))
      #if(verbose=='skip')
       # browser()
      self$rv$status[self$rv$current.pos] <- global$VALIDATED
      self$Discover_Skipped_Status()
      #browser()
      # Either the process has been validated, one can prepare data to be sent to caller
      # Or the module has been reseted
      if (self$rv$current.pos == self$length)
        self$Send_Result_to_Caller()
    },
    
    Main_UI = function(){
      cat(paste0(class(self)[1], '::', 'Main_UI() from - ', self$id, '\n'))
      tagList(
        shinyjs::useShinyjs(),
        # tags$head(tags$style("#modal1 .modal-body {padding: 10px}
        #                #modal1 .modal-content  {-webkit-border-radius: 6px !important;-moz-border-radius: 6px !important;border-radius: 6px !important;}
        #                #modal1 .modal-dialog { width: 50%; display: inline-block; text-align: left; vertical-align: top;}
        #                #modal1 .modal-header {background-color: #339FFF; border-top-left-radius: 6px; border-top-right-radius: 6px}
        #                #modal1 .modal { text-align: right; padding-right:10px; padding-top: 24px;}
        #                #moda1 .close { font-size: 16px}")),
        div(id = self$ns('GlobalTL'),
            fluidRow(
              align= 'center',
              column(width=2, div(style = self$btn_style,
                                  shinyjs::disabled(actionButton(self$ns("prevBtn"), "<<",
                                                                 class = PrevNextBtnClass,
                                                                 style='padding:4px; font-size:80%')),
                                  actionButton(self$ns("rstBtn"), paste0("Reset entire ", self$type),
                                               class = redBtnClass,
                                               style='padding:4px; font-size:80%'))
              ),
              column(width=8, div( style = self$btn_style,
                                   self$timelineDraw$ui())),
              column(width=2, div(style = self$btn_style,
                                  actionButton(self$ns("nextBtn"),
                                               ">>",
                                               class = PrevNextBtnClass,
                                               style='padding:4px; font-size:80%')
              )
              )
            ),
            
            uiOutput(self$ns('SkippedInfoPanel')),
            self$EncapsulateScreens()
            
        )
      )
    },
    
    
    
    
    
    
    ToggleState_Screens = function(cond, range, prefix){
      cat(paste0(class(self)[1], '::ToggleState_Steps() from - ', self$id, '\n'))
      #if (verbose==T)  
      #browser()
      lapply(range, function(x){
        shinyjs::toggleState(paste0(prefix,self$config$steps[x]),
                             condition = cond)
      })
    },
    
    Update_Cursor_Position = function(){
      cat(paste0(class(self)[1], '::Update_Cursor_position() from - ', self$id, '\n'))
      if (verbose==T) browser()
      req(self$rv$status)
      if (self$rv$status[self$length] == global$VALIDATED)
        self$rv$current.pos <- self$default_pos$VALIDATED
    },
    
    GetFirstMandatoryNotValidated = function(){
      first <- NULL
      first <- unlist((lapply(1:self$length, 
                              function(x){self$config$mandatory[x] && !self$rv$status[x]})))
      if (sum(first) > 0)
        min(which(first == TRUE))
      else
        NULL
    },
    
    GetMaxValidated_AllSteps = function(){
      cat(paste0(class(self)[1], '::', 'GetMaxValidated_AllSteps() from - ', self$id, '\n'))
      val <- NULL
      ind <- grep(global$VALIDATED, self$rv$status)
      val <- if (length(ind) > 0) max(ind) else NULL
      val
    },
    
    SetModalTxt = function(txt){self$modal_txt <- txt},
    
    Change_Current_Pos = function(i){ self$rv$current.pos <- 1},
    #-------------------------------------------------------
    # Return the UI for a modal dialog with data selection input. If 'failed' is
    # TRUE, then display a message that the previous value was invalid.
    dataModal = function() {
      
      tags$div(id="modal1", 
               modalDialog(
                 span(self$modal_txt),
                 footer = tagList(
                   actionButton(self$ns("close"), "Cancel", class='btn-info'),
                   actionButton(self$ns("modal_ok"), "OK")
                 )
               )
      )
    },
    
    NavPage = function(direction) {
      newval <- self$rv$current.pos + direction 
      newval <- max(1, newval)
      newval <- min(newval, self$length)
      if(newval == 0)
        browser()
      
      self$rv$current.pos <- newval
      cat(paste0('new position = ', self$rv$current.pos, '\n'))
    },
    
    
    
    ###############################################################
    ###                          SERVER                         ###
    ###############################################################
    server = function(dataIn = reactive({NULL})) {
      cat(paste0(class(self)[1], '::server(dataIn) from - ', self$id, '\n'))
      
     observe({
       cat(paste0(class(self)[1], '::observe() from - ', self$id, '\n'))
       #browser()
       isolate({
         self$Additional_Server_Funcs()
       
       # This function get the UI definition of the screens for the steps
       # It differs between process class (which screens are given in details in their
       # corresponding source code file) and pipeline class in which the UI is
       # the ui() function from the entire process class
       
      #self$screens <- self$GetScreensDefinition()
       })

     })
      
      observeEvent(dataIn(), ignoreNULL = F, {
        cat(paste0(class(self)[1], '::observeEvent(dataIn()) from --- ', self$id, '\n'))
        #self$rv$current.pos <- 1
        self$timeline$Change_Current_Pos(1)
        self$rv$temp.dataIn <- dataIn()
        self$ActionOn_New_DataIn()
        })
      
     
      
      
      observeEvent(req(self$rv$status[self$length] == global$VALIDATED), {
        self$rv$current.pos <- self$length
      })
      
      observeEvent(input$rstBtn, {
        cat(paste0(class(self)[1], '::observeEvent(input$rstBtn) from - ', self$id, '\n'))
        showModal(self$dataModal())
      })

      observeEvent(input$close, {removeModal() })
      
      
      observeEvent(self$rv$isReseted, ignoreInit = F, { 
        cat(paste0(class(self)[1], '::', 'observeEvent(c(self$rv$isReseted) from -- ', self$id, ' --\n'))
        #browser()
        
        if (!is.null(self$child.process))
          lapply(self$config$steps, function(x){
            self$child.process[[x]]$SetReseted(self$rv$isReseted)
          })
        
        self$ActionOn_Reset()
        self$rv$current.pos <- 1 
      })
      

      observeEvent(req(!is.null(self$rv$isSkipped)), ignoreNULL=F, { 
        cat(paste0(class(self)[1], '::observeEvent(isSkipped()) from - ', self$id, '\n'))
        #if(verbose=='skip') 
        
        print(self$rv$isSkipped)
         # browser()
        self$ActionOn_isSkipped()
      })
      
      ###############################################################
      ###                    MODULE SERVER                        ###
      ###############################################################
      moduleServer(self$id, function(input, output, session) {
        cat(paste0(class(self)[1], '::moduleServer(input, output, session) from - ', self$id, '\n'))
        
        observe({self$input <- input})
        
        
        observe({
          cat(paste0(class(self)[1], '::observe() from - ', self$id, '\n'))
          # if (verbose=='skip') 
          #browser()
          self$rv$status <- status()
          self$rv$isAllSkipped <- sum(rep(global$SKIPPED, self$length)==self$rv$status)==self$length
          self$rv$isAllUndone <- sum(rep(global$UNDONE, self$length)==self$rv$status)==self$length
          
          #self$Force_ToggleState_Screens()
          
          shinyjs::toggleState(id = "prevBtn", condition = self$rv$current.pos > 1)
          shinyjs::toggleState(id = "nextBtn", condition = self$rv$current.pos < self$length)
          shinyjs::hide(selector = paste0(".page_", self$id))
          shinyjs::show(self$config$steps[self$rv$current.pos])
        })
        
        
        
        observeEvent(req(input$modal_ok > 0), ignoreInit=F,{
          cat(paste0(class(self)[1], '::observeEvent(req(c(input$modal_ok))) from - ', self$id, '\n'))
          self$rv$reset_OK <- input$rstBtn
          self$rv$current.pos <- 1
          removeModal()
        })
        
        output$SkippedInfoPanel <- renderUI({
          cat(paste0(class(self)[1], '::output$SkippedInfoPanel from - ', self$id, '\n'))
          #browser()
          current_step_skipped <- self$rv$status[self$rv$current.pos] == global$SKIPPED
          entire_process_skipped <- isTRUE(sum(self$rv$status) == global$SKIPPED * self$length)
          req(current_step_skipped || entire_process_skipped)
          
          
          if (entire_process_skipped)
            txt <- "this step is disabled because it has been skipped."
          else if (current_step_skipped)
            txt <- "this step is disabled because it has been skipped."
          
          wellPanel(
            style = "background-color: #7CC9F0; opacity: 0.72; padding: 0px; align: center; vertical-align: center;",
            height = 100,
            width=300,
            align="center",
            p(style = "color: black;", paste0('Info: ',txt))
          )
        })
        
        observeEvent(input$prevBtn, ignoreInit = TRUE, {self$NavPage(-1)})
        observeEvent(input$nextBtn, ignoreInit = TRUE, {self$NavPage(1)})
        
       
        ###########---------------------------#################
        output$show_dataIn <- renderUI({
          cat(paste0(class(self)[1], '::output$show_dataIn from - ', self$id, '\n'))
          req(dataIn())
          tagList(
            # h4('show dataIn()'),
            lapply(names(dataIn()), function(x){tags$p(x)})
          )
        })
        
        output$show_rv_dataOut <- renderUI({
          cat(paste0(class(self)[1], '::output$show_rv_dataOut from - ', self$id, '\n'))
          self$dataOut$trigger
          tagList(
            #h4('show self$dataOut$value'),
            lapply(names(self$rv$dataIn), function(x){tags$p(x)})
          )
        })
        
        
        
        
      reactive({self$dataOut})
      })
    }
)
)