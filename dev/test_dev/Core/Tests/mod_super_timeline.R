
mod_super_timeline_ui <- function(id){
  ns <- NS(id)
  tagList(
    useShinyjs(),
    mod_timeline_ui(ns("timeline")),
    uiOutput(ns('show_screens')),
    hr(),
    wellPanel(
      h3('Pipeline'),
      p('dataIn() :'),
      verbatimTextOutput(ns('show_dataIn')),
      p('rv$dataIn :'),
      verbatimTextOutput(ns('show_rv_dataIn')),
      p('rv$dataOut'),
      verbatimTextOutput(ns('show_rv_dataOut'))
    )
  )
}

#' @param dataIn xxx
#'
#' 
#' 
mod_super_timeline_server <- function(id, dataIn=NULL){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      source(file.path('.', 'code_general.R'), local=TRUE)$value
      
      rv <- reactiveValues(
        force.position = c(FALSE, FALSE, FALSE, FALSE, FALSE)
      )
      
      # variables to communicate with the navigation module
      rv.process_config <- reactiveValues(
        type = 'pipeline',
        process.name = 'Pipeline protein',
        stepsNames = c("Description", "Filtering", "Normalization", "Imputation", "Summary"),
        isDone =  c(TRUE, rep(FALSE,4)),
        mandatory =  c(FALSE, rep(TRUE,4))
      )
      
      
      
      tl.update <- reactiveValues(
        current.pos = 1,
        actions = list(rst = TRUE,
                       nxt = TRUE,
                       prv = TRUE)
      )
      
      
      pos <- mod_timeline_server("timeline", 
                                 style = 2, 
                                 process_config = rv.process_config, 
                                 tl.update = tl.update)
      
      
      # Initialization of the process
      observeEvent(req(dataIn()), {
        print('------ MODULE SUPER_TIMELINE : Initialisation du module ------')
        rv$dataIn <- dataIn()
        
        rv$remoteReset <- rep(length(rv.process_config$stepsNames), FALSE)
        
        # Instantiation of the screens
        CreateScreens()
        InitScreens()
        
        tl.update$actions$nxt <- condNextBtn()
        tl.update$actions$nxt <- condPrevBtn()
        })
      

      #Catch a reset command from timeline
      observeEvent(req(pos$rstBtn()!=0), {
        ReinitScreens()
        rv.process_config$isDone <- c(TRUE, rep(FALSE, length(rv.process_config$stepsNames)-1))
        tl.update$current.pos <- 1
        
       # rv$dataOut <- dataIn()
        rv$dataOut <- NULL
      })
      
      
      
      observeEvent(tl.update$current.pos,  ignoreInit = T, {
        lapply(tl.update$current.pos, function(x){
            if (rv.process_config$isDone[tl.update$current.pos])
            rv$force.position[x] <- rv$force.position[x] + 1
          })

        # Display the screen corresponding to the new position
        DisplayCurrentStep()
        tl.update$actions$nxt <- condNextBtn()
        tl.update$actions$nxt <- condPrevBtn()
      })
      
      
      
      observeEvent(rv.process_config$isDone,  ignoreInit = T, {
        rv.process_config$mandatory
          
        tl.update$actions$nxt <- condNextBtn()
        tl.update$actions$nxt <- condPrevBtn()
      })
      
      
      
      
      #####################################################################
      ## screens of the module
      ##
      ############### SCREEN 1 ######################################
      output$screen1 <- renderUI({
        tagList(
          tags$h3(paste0('Pipeline ', rv.process_config$name))
        )
      })
      
      
      ############### SCREEN 2 ######################################
      
      output$screen2 <- renderUI({
        tagList(
          div(id=ns('screen2'),
              tags$h3(rv.process_config$stepsName[2]),
              mod_wf_wf1_A_ui(ns('mod_A_nav'))
          )
        )
      })

      rv$tmpA <- mod_wf_wf1_A_server("mod_A_nav",
                                     dataIn = reactive({rv$dataIn}),
                                     remoteReset = reactive({pos$rstBtn()}),
                                     forcePosition = reactive({rv$force.position[2]})
                                     )
      
      observeEvent(req(rv$tmpA$dataOut()),  { 
        rv$dataOut <- rv$tmpA$dataOut()
      })
      
      observeEvent(rv$tmpA$validated(),  { 
       rv.process_config$isDone[2] <- rv$tmpA$validated()
      })
      
      observeEvent(req(rv$tmpA$reseted()!=0), {
        ind <- grep(rv.process_config$stepsNames[2], names(rv$tmpA$dataOut()))
        if (length(ind)>0){
          rv$dataIn <- rv$tmpA$dataOut()[ , ,-c(ind:length(rv$tmpA$dataOut())) ]
          rv$dataOut <- rv$dataIn 
          rv.process_config$isDone[ind:length(rv.process_config$isDone)] <- FALSE
        }
        
      })
      
      ############### SCREEN 3 ######################################
      output$screen3 <- renderUI({
        
        tagList(
          div(id=ns('screen3'),
              tags$h3(rv.process_config$stepsName[3]),
              mod_wf_wf1_B_ui(ns('mod_B_nav'))
          )
        )
      })
      
      rv$tmpB <- mod_wf_wf1_B_server("mod_B_nav",
                                     dataIn = reactive({rv$tmpA$dataOut()}),
                                     remoteReset = reactive({pos$rstBtn()}),
                                     forcePosition = reactive({rv$force.position[3]})
                                     )
      
      observeEvent(req(rv$tmpB$dataOut()),  { 
        #print('MODULE SUPER_TL : New value for rv$tmpB$dataOut() :')
        #print(paste0("      names(rv$tmpB$dataOut()) = ", paste0(names(rv$tmpB$dataOut()), collapse=' - ')))
        rv$dataOut <- rv$tmpB$dataOut()
      })
      
      observeEvent(rv$tmpB$validated(),  { 
        #print('MODULE SUPER_TL : New value for rv$tmpB$validated() :')
        #print(paste0("      rv$tmpB$validated() = ", rv$tmpB$validated()))
        rv.process_config$isDone[3] <- rv$tmpB$validated()
      })
      
      observeEvent(req(rv$tmpB$reseted()!=0), {
        #print(paste0('MODULE SUPER_TL : New value for rv$tmpB$reseted() : ', rv$tmpB$reseted()))
        ind <- grep(rv.process_config$stepsNames[3], names(rv$tmpA$dataOut()))
        if (length(ind)>0){
          rv$dataIn <- rv$tmpA$dataOut()[ , ,-c(ind:length(rv$tmpA$dataOut())) ]
          rv$dataOut <- rv$dataIn 
          rv.process_config$isDone[ind:length(rv.process_config$isDone)] <- FALSE
        }
      })
      
      
      ############### SCREEN 4 ######################################
      output$screen4 <- renderUI({
        
        tagList(
          div(id=ns('screen4'),
              tags$h3(rv.process_config$stepsName[4]),
              mod_wf_wf1_C_ui(ns('mod_C_nav'))
          )
        )
      })
      
      rv$tmpC <- mod_wf_wf1_C_server("mod_C_nav",
                                     dataIn = reactive({rv$tmpB$dataOut()}),
                                     remoteReset = reactive({pos$rstBtn()}) ,
                                     forcePosition = reactive({rv$force.position[4]})
                                     )
      
      
      observeEvent(req(rv$tmpC$dataOut()),  { 
        #print('MODULE SUPER_TL : New value for rv$tmpC$dataOut() :')
        #print(paste0("      names(rv$tmpC$dataOut()) = ", paste0(names(rv$tmpC$dataOut()), collapse=' - ')))
         rv$dataOut <- rv$tmpC$dataOut()
      })
      
      observeEvent(rv$tmpC$validated(),  { 
        #print('MODULE SUPER_TL : New value for rv$tmpC$validated() :')
        #print(paste0("      rv$tmpC$validated() = ", rv$tmpC$validated()))
        rv.process_config$isDone[4] <- rv$tmpC$validated()
      })
      
      observeEvent(req(rv$tmpC$reseted()!=0), {
        #print(paste0('MODULE SUPER_TL : New value for rv$tmpC$reseted() : ', rv$tmpC$reseted()))
        ind <- grep(rv.process_config$stepsNames[4], names(rv$tmpA$dataOut()))
        if (length(ind)>0){
          rv$dataIn <- rv$tmpA$dataOut()[ , ,-c(ind:length(rv$tmpA$dataOut())) ]
          rv$dataOut <- rv$dataIn 
          rv.process_config$isDone[ind:length(rv.process_config$isDone)] <- FALSE
        }
        
      })
      
      ############### SCREEN 5 ######################################
      output$screen5 <- renderUI({
        
        tagList(
          div(id='screen5',
              tags$h3(rv.process_config$stepsName[5])
          )
        )
      })
      
      ##########################################################
      
      list(dataOut = reactive({rv$dataOut}),
           validated = reactive({rv.process_config$isDone[length(rv.process_config$isDone)]}),
           reseted = reactive({pos$rstBtn()})
      )
    }
  )
}

