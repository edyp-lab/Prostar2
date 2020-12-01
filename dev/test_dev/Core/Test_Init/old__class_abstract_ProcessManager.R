
    GetStringStatus = function(status){
      cat(paste0(class(self)[1], '::GetStringStatus() from - ', self$id, '\n'))
      if (status==self$global$VALIDATED) "Validated"
      else if (status==self$global$UNDONE) "Undone"
      else if (status==self$global$SKIPPED) 'Skipped'
    },
    
    
    
    
    
    
    
    GetMaxValidated_BeforeCurrentPos = function(){
      cat(paste0(class(self)[1], '::', 'GetMaxValidated_BeforeCurrentPos() from - ', self$id, '\n'))
      ind.max <- NULL
      indices.validated <- which(self$config$status == self$global$VALIDATED)
      if (length(indices.validated) > 0){
        ind <- which(indices.validated < self$rv$current.pos)
        if(length(ind) > 0)
          ind.max <- max(ind)
      }
      
      # if (ind.max == 0)
      #   ind.max <- 1
       
      ind.max
    },
    
    
    # Test if a process module (identified by its name) has been skipped.
    # This function is called each time the list config$isDone is updated
    # because one can know the status 'Skipped' only when a further module
    # has been validated
    is.skipped = function(name){
      cat(paste0(class(self)[1], '::', 'is.skipped() from - ', self$id, '\n'))
      if(verbose=='skip') browser()
      pos <- which(name == self$config$steps)
      return(self$GetStatusPosition(pos) == self$global$SKIPPED)
    },
    
    InitializeModule = function(){
      #self$config$screens <- self$CreateScreens()
      #self$config$screens <- self$GetScreensDefinition()
      self$rv$current.pos <- 1
    },

    GetCurrentStepName = function(){
      cat(paste0(class(self)[1], '::GetCurrentStepName() from - ', self$id, '\n'))
      self$config$steps[self$rv$current.pos]
    },
    
    Unskip = function(pos){
      cat(paste0(class(self)[1], '::Unskip() from - ', self$id, '\n'))
      self$config$status[pos] <- self$global$UNDONE
    },
    
    GetStatusPosition = function(pos){
      cat(paste0(class(self)[1], '::GetStatusPosition() from - ', self$id, '\n'))
      self$config$status[pos]
    },
    
    # This function cannot be implemented in the timeline module because 
    # the id of the screens to reset are not known elsewhere.
    # Trying to reset the global 'div_screens' in the timeline module
    # does not work
    ResetScreens = function(){
      cat(paste0(class(self)[1], '::ResetScreens() from - ', self$id, '\n'))
      
      lapply(1:self$length, function(x){
        shinyjs::reset(NS(self$id)(self$config$steps[x]))
      })
    },

    
    
   
    
    
    

    
    Actions_On_Reset = function(){
      cat(paste0(class(self)[1], '::', 'ActionsOnReset() from - ', self$id, '\n'))
      #browser()
      self$ResetScreens()
      self$rv$dataIn <- NULL
      self$Initialize_Status_Process()
      self$Send_Result_to_Caller()
      self$InitializeDataIn()
    },
    

    
    Actions_On_New_DataIn = function(data){
      cat(paste0(class(self)[1], '::', 'Actions_On_New_DataIn() from - ', self$id, '\n'))
      if (verbose=='skip') browser()
      # This variable serves as a tampon while waiting the user click on the
      # validate button in the Description screen.
      # Once done, this variable is observed and the real rv$dataIn function can be
      # instanciated
      self$rv$temp.dataIn <- data
      
      
      # Test if input is NA or not
      inputExists <- !is.null(data)
      
      #Test if a dataset is already loaded
      tmpExists <- !is.null(self$rv$dataIn)
      
      
      if (tmpExists && inputExists){
        # this case is either the module is skipped or validated
        #self$rv$current.pos <- length(self$config$status)
        self$ActionsOn_Tmp_Input()
      } else if (tmpExists && !inputExists) {
        # The module has been reseted
        browser()
        self$ActionsOn_Tmp_NoInput()
      } else if (!tmpExists && inputExists){
        # The current position is pointed on a new module
        self$ActionsOn_NoTmp_Input()
      } else if (!tmpExists && !inputExists){
        # Initialization of Prostar
        self$ActionsOn_NoTmp_NoInput()
      }
    },
  
    
    

    
    # SERVER
    server = function(dataIn = NULL, 
                      reset = reactive({NULL}),
                      isSkipped = reactive({NULL})) {

      
      observeEvent(reset(), ignoreInit = F, { 
        cat(paste0(class(self)[1], '::', 'observeEvent(remoteReset()) from - ', self$id, '\n'))
        print("remote reset activated")
        
        # Used to transmit info of local Reset to child processes
        self$rv$reset <- reset()
        self$Actions_On_Reset()
        })
      


      
      
      # MODULE SERVER
      moduleServer(self$id, function(input, output, session) {
        

      reactive({self$dataOut})
      }
  )
)