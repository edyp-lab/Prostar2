# Use of the tip in this page to unshare reactiveValues between different instances
# of the same class
# https://community.rstudio.com/t/r6-class-reactivevalues-property-and-instantiation/31025/2


redBtnClass <- "btn-danger"
PrevNextBtnClass <- "btn-info"
btn_success_color <- "btn-success"
optionsBtnClass <- "info"

btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"




ScreenManager <- R6Class(
  "ScreenManager",
  private = list(),
  public = list(
    # Declaration of variables
    #input = NULL,
    id = NULL,
    ns = NULL,
    style = NULL,
    child.process = NULL,
    length = NULL,
    config = NULL,
    screens = NULL,
    modal_txt = NULL,
    timeline  = NULL,
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
        trigger = 0,
        value = NULL
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
        tl.tags.enabled = setNames(rep(FALSE, self$length), self$config$steps),
        local.reset = NULL,
        isAllSkipped = FALSE,
        isAllUndone = TRUE,
        isReseted = NULL,
        isSkipped = NULL)
      
   
      self$rv$status <- setNames(rep(global$UNDONE, self$length), self$config$steps)
      
      self$timeline <- TimelineDraw$new(self$ns('TL_draw'), 
                                            mandatory = self$config$mandatory)
      self$timeline$server(status = reactive({self$rv$status}),
                           position = reactive({self$rv$current.pos}),
                           enabled = reactive({self$rv$tl.tags.enabled})
                           )
      self$Additional_Initialize_Class()

    },
    
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
    
    Additional_Initialize_Class = function(){},
    GetStringStatus = function(name){
      if (name==global$VALIDATED) "Validated"
      else if (name==global$UNDONE) "Undone"
      else if (name==global$SKIPPED) 'Skipped'
    },
    
    Timestamp = function(){ 
      cat(paste0(class(self)[1], '::Timestamp() from - ', self$id, '\n'))
      as.numeric(Sys.time())
    },
    
    Send_Result_to_Caller = function(){
      cat(paste0(class(self)[1], '::Send_Result_to_Caller() from - ', self$id, '\n'))
      #self$dataOut$value <- self$rv$dataIn
      self$dataOut$trigger <- self$Timestamp()
      self$dataOut$value <- self$rv$dataIn

    },
    
    Get_Result = function(){self$dataOut$value},
    
    
    EncapsulateScreens = function(){},
    InitializeDataIn = function(){ 
      cat(paste0(class(self)[1], '::', 'InitializeDataIn() from - ', self$id, '\n'))
      if (verbose==T) browser()
      self$rv$dataIn <- self$rv$temp.dataIn
    },
    
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
    
    GetMaxValidated_AllSteps = function(){
      cat(paste0(class(self)[1], '::', 'GetMaxValidated_AllSteps() from - ', self$id, '\n'))
      val <- NULL
      ind <- grep(global$VALIDATED, self$rv$status)
      val <- if (length(ind) > 0) max(ind) else 0
      val
    },

    ActionOn_New_DataIn = function(){},
    Additional_Server_Funcs = function(){},
    
    SetModalTxt = function(txt){self$modal_txt <- txt},
    
    Change_Current_Pos = function(i){ self$rv$current.pos <- 1},
    
    #-------------------------------------------------------
    ToggleState_Screens = function(cond, range){},

    ToggleState_ResetBtn = function(cond){
      cat(paste0(class(self)[1], '::ToggleState_Steps() from - ', self$id, '\n'))
      #browser()
      shinyjs::toggleState(self$ns('rstBtn'), condition = cond)
      # lapply(range, function(x){
      #   shinyjs::toggleState(paste0(self$ns(self$config$steps[x]), '-div_TL_ResetBtn'), condition = cond)
      #   #Send to TL the enabled/disabled tags
      #   self$rv$tl.tags.enabled[x] <- cond
      # })
    },
    
    # ToggleState_Child_Screens = function(cond, range){
    #   cat(paste0(class(self)[1], '::ToggleState_Steps() from - ', self$id, '\n'))
    #   #browser()
    #   lapply(range, function(x){
    #     shinyjs::toggleState(paste0(self$ns(self$config$steps[x]), '-div_Screens'), condition = cond)
    #     #Send to TL the enabled/disabled tags
    #     self$rv$tl.tags.enabled[x] <- cond
    #   })
    # },
    # 
    # ToggleState_Child_ResetBtn = function(cond, range){
    #   cat(paste0(class(self)[1], '::ToggleState_Steps() from - ', self$id, '\n'))
    #   #browser()
    #   lapply(range, function(x){
    #     shinyjs::toggleState(paste0(self$ns(self$config$steps[x]), '-div_TL_ResetBtn'), condition = cond)
    #     #Send to TL the enabled/disabled tags
    #     self$rv$tl.tags.enabled[x] <- cond
    #   })
    # },
    
    
    ResetScreens = function(){
      cat(paste0(class(self)[1], '::ResetScreens() from - ', self$id, '\n'))
      
      lapply(1:self$length, function(x){
        shinyjs::reset(self$config$steps[x])
      })
    },
    
    
    
    Initialize_Status_Process = function(){
      cat(paste0(class(self)[1], '::', 'Initialize_Status_Process() from - ', self$id, '\n'))
      self$rv$status <- setNames(rep(global$UNDONE, self$length),self$config$steps)
    },
    
    Set_Skipped = function(){},
    Set_Reseted = function(){},
    
    ValidateCurrentPos = function(){},
    
    
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
    NavPage = function(direction) {
      newval <- self$rv$current.pos + direction 
      newval <- max(1, newval)
      newval <- min(newval, self$length)
      if(newval == 0)
        browser()
      
      self$rv$current.pos <- newval
      cat(paste0('new position = ', self$rv$current.pos, '\n'))
    },
    
    Main_UI = function(){
      #cat(paste0(class(self)[1], '::', 'Main_UI() from - ', self$id, '\n'))
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
              column(width=2, 
                     div(id = self$ns('div_TL_LeftSide'),
                         style = self$btn_style,
                         shinyjs::disabled(
                           actionButton(self$ns("prevBtn"), "<<",
                                        class = PrevNextBtnClass,
                                        style = 'padding:4px; font-size:80%')
                           )
                         ),
                     div(id = self$ns('div_TL_ResetBtn'),
                         shinyjs::disabled(
                           actionButton(self$ns("rstBtn"), paste0("Reset entire ", self$type),
                                        class = redBtnClass,
                                        style = 'padding:4px; font-size:80%'))
                     )
              ),
              column(width=8, 
                     div(id = self$ns('div_TL'),
                         style = self$btn_style,
                         self$timeline$ui())
                     ),
              column(width=2,
                     div(id = self$ns('div_TL_RightSide'),
                         style = self$btn_style,
                         actionButton(self$ns("nextBtn"),
                                      ">>",
                                      class = PrevNextBtnClass,
                                      style='padding:4px; font-size:80%')
                         )
                     )
              ),
            
        div(id = self$ns('div_Screens'),
            uiOutput(self$ns('SkippedInfoPanel')),
            self$EncapsulateScreens()
            ),
        fluidRow(
          column(width=2,
                 tags$b(h4(style = 'color: blue;', "Input of process")),
                 uiOutput(self$ns('show_dataIn'))),
          column(width=2,
                 tags$b(h4(style = 'color: blue;', "Input of process")),
                 uiOutput(self$ns('show_rv_dataIn'))),
          column(width=2,
                 tags$b(h4(style = 'color: blue;', "Output of process")),
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
    server = function(dataIn = reactive({NULL})) {
      cat(paste0(class(self)[1], '::server(dataIn) from - ', self$id, '\n'))
      
     # self$Additional_Server_Funcs()
      
      #
      # Catch a new dataset sent by the caller
      #
      observeEvent(dataIn(), ignoreNULL = F, ignoreInit = T,{
        cat(paste0(class(self)[1], '::observeEvent(dataIn()) from --- ', self$id, '\n'))
        
        self$Change_Current_Pos(1)
        self$rv$temp.dataIn <- dataIn()
        self$ActionOn_New_DataIn() # Used by class pipeline
        browser()
        
        if(is.null(dataIn())){
          self$ToggleState_Screens(FALSE, 1:self$length)
           self$ToggleState_ResetBtn(FALSE)

        } else {
          self$ToggleState_ResetBtn(TRUE) #Enable the reset button
          self$ToggleState_Screens(TRUE, 1) #Enable the first screen
          # Disable all screens after the first mandatory not validated
          firstM <- self$GetFirstMandatoryNotValidated()
          if (!is.null(firstM) && self$length > 1) {
            offset <- as.numeric(firstM != self$length)
            self$ToggleState_Screens(cond = FALSE, range = (firstM + offset):self$length)
          }
        }
        
      })
      
      
      
      
      # Catch new status event
      observeEvent(self$rv$status, ignoreInit = T, {
        cat(paste0(class(self)[1], '::observe((self$rv$status) from - ', self$id, '\n'))
        
        self$Discover_Skipped_Steps()
        self$rv$isAllSkipped <- sum(rep(global$SKIPPED, self$length)==self$rv$status)==self$length
        self$rv$isAllUndone <- sum(rep(global$UNDONE, self$length)==self$rv$status)==self$length
        
        # Disable all steps if all steps are skipped
        if (self$rv$isAllSkipped){
          self$ToggleState_Screens(FALSE, 1:self$length)
          self$ToggleState_ResetBtn(FALSE)
           }
        # Disable all steps if all steps are undone (such as after a reset)
        # Same action as for new dataIn() value
        if (self$rv$isAllUndone){
          self$ToggleState_Screens(TRUE, 1)
          if(self$length > 1)
            self$ToggleState_Screens(TRUE, 2:self$length)
          self$ToggleState_ResetBtn(TRUE)
          
          # Disable all screens after the first mandatory not validated
          firstM <- self$GetFirstMandatoryNotValidated()
          if (!is.null(firstM) && self$length > 1) {
            offset <- as.numeric(firstM != self$length)
            self$ToggleState_Screens(cond = FALSE, range = (firstM + offset):self$length)
          }
        }
        
         # Disable all previous steps from each VALIDATED step
         # and enable all further steps 
         ind.max <- self$GetMaxValidated_AllSteps()
         if (!is.null(ind.max)){
           self$ToggleState_Screens(cond = FALSE, range = 1:ind.max)
           if (ind.max < self$length){
                 offset <- 1
                 self$ToggleState_Screens(cond = TRUE, range = (offset + ind.max):self$length)
               }
         }
         
         # Disable all screens after the first mandatory not validated
         firstM <- self$GetFirstMandatoryNotValidated()
          if (!is.null(firstM) && self$length > 1) {
            offset <- as.numeric(firstM != self$length)
            self$ToggleState_Screens(cond = FALSE, range = (firstM + offset):self$length)
            }
      })
      
      
      
      
      observeEvent(self$rv$current.pos, ignoreInit = T,{
        cat(paste0(class(self)[1], '::observe(self$rv$current.pos) from - ', self$id, '\n'))
        
        shinyjs::toggleState(id = self$ns("prevBtn"), condition = self$rv$current.pos > 1)
        shinyjs::toggleState(id = self$ns("nextBtn"), condition = self$rv$current.pos < self$length)
        shinyjs::hide(selector = paste0(".page_", self$id))
        shinyjs::show(self$ns(self$config$steps[self$rv$current.pos]))
      })
      

      ###############################################################
      ###                    MODULE SERVER                        ###
      ###############################################################
      moduleServer(self$id, function(input, output, session) {
        cat(paste0(class(self)[1], '::moduleServer(input, output, session) from - ', self$id, '\n'))
        
        # observe({
        #   cat(paste0(class(self)[1], '::self$input <- input from - ', self$id, '\n'))
        #   self$input <- input
        #   })
        

          self$Additional_Server_Funcs()
        self$screens <- self$GetScreensDefinition(input, output)

        
        observeEvent(input$rstBtn, {
          cat(paste0(class(self)[1], '::observeEvent(input$rstBtn) from - ', self$id, '\n'))
          showModal(self$dataModal())
        })
        
        observeEvent(input$close, {removeModal() })
        
        observeEvent(req(input$modal_ok > 0), ignoreInit=F, {
          cat(paste0(class(self)[1], '::observeEvent(req(c(input$modal_ok))) from - ', self$id, '\n'))
          self$rv$local.reset <- input$rstBtn
          self$Set_All_Reset()
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
        
        output$show_rv_dataIn <- renderUI({
          cat(paste0(class(self)[1], '::output$show_rv_dataIn from - ', self$id, '\n'))
          req(self$rv$dataIn)
          tagList(
            # h4('show dataIn()'),
            lapply(names(self$rv$dataIn), function(x){tags$p(x)})
          )
        })
        
        output$show_rv_dataOut <- renderUI({
          cat(paste0(class(self)[1], '::output$show_rv_dataOut from - ', self$id, '\n'))
          #self$dataOut$trigger
          tagList(
            #h4('show self$dataOut$value'),
            lapply(names(self$dataOut$value), function(x){tags$p(x)})
          )
        })
        
        
        output$show_status <- renderUI({
          #req(self$rv$status, self$rv$current.pos, self$rv$tl.tags.enabled)
          tagList(lapply(1:self$length, 
                         function(x){
                           color <- if(self$rv$tl.tags.enabled[x]) 'black' else 'lightgrey'
                           if (x == self$rv$current.pos)
                             tags$p(style = paste0('color: ', color, ';'),
                                    tags$b(paste0('---> ', self$config$steps[x], ' - ', self$GetStringStatus(self$rv$status[[x]])), ' <---'))
                           else 
                             tags$p(style = paste0('color: ', color, ';'),
                                    paste0(self$config$steps[x], ' - ', self$GetStringStatus(self$rv$status[[x]])))
                         }))
        })
        
        reactive({self$dataOut})
      })
    }
)
)