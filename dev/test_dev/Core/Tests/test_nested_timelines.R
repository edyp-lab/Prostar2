source(file.path('../../../../R', 'mod_navigation.R'), local=TRUE)$value
source(file.path('../../../../R', 'global.R'), local=TRUE)$value
source(file.path('../Workflows/wf1', 'mod_wf_wf1_A.R'), local=TRUE)$value
source(file.path('../Workflows/wf1', 'mod_wf_wf1_B.R'), local=TRUE)$value
source(file.path('../Workflows/wf1', 'mod_wf_wf1_C.R'), local=TRUE)$value


options(shiny.fullstacktrace = T)

ui <- fluidPage(
  tagList(
    uiOutput('show'),
    uiOutput('obj')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  r.nav <- reactiveValues(
    name = "Pipeline",
    stepsNames = c("Description", "Proc 1", "Proc 2", "Proc 3", 'Summary'),
    ll.UI = list( screenStep1 = uiOutput("screen1"),
                  screenStep2 = uiOutput("screen2"),
                  screenStep3 = uiOutput("screen3"),
                  screenStep4 = uiOutput("screen4"),
                  screenStep4 = uiOutput("screen5")),
    isDone =  c(TRUE, FALSE, FALSE, FALSE, FALSE),
    mandatory =  c(FALSE, FALSE, TRUE, TRUE, TRUE),
    reset = FALSE
    
  )
  
  
  rv <- reactiveValues(
    current.obj = list(original = 0),
    tmpA = NULL,
    tmpB = NULL,
    tmpC = NULL
  )
  
  
  screens <- mod_navigation_server("pipeline_nav",
                                   style = 2,
                                   pages = r.nav)
  
  output$show <- renderUI({
    tagList(
      mod_navigation_ui("pipeline_nav"),
      hr(),
      screens()
    )
  })
  
  output$obj <- renderUI({
    rv$current.obj
    paste0(unlist(rv$current.obj), collapse=' ')
  })
  
  
  observeEvent(req(r.nav$reset),{
    r.nav$reset  <- FALSE
    print('reset activated from navigation module')
    r.nav$isDone <- c(TRUE, rep(FALSE, length(r.nav$stepsNames)-1))
    for (i in 1:length(r.nav$stepsNames))
      shinyjs::reset(paste0('screen', i))
    rv$dataIn <- dataIn()
    rv$dataOut <- NULL
  })
  
 
  #####################################################################
  ## screens of the module
  
  ############### SCREEN 1 ######################################
  
  output$screen1 <- renderUI({
    tagList(
      tags$h3('Description of the pipeline')
    )
  })
  
  # observeEvent(input$done1,{r.nav$isDone[1] <- TRUE})
  #
  ############### SCREEN 2 ######################################
  output$screen2 <- renderUI({
    tagList(
      div(id='screen2',
          tags$h3('Processus 1'),
          mod_wf_wf1_A_ui('mod_A_nav')
      )
    )
  })
  
  rv$tmpA <- mod_wf_wf1_A_server("mod_A_nav", dataIn = reactive({rv$current.obj}) )
  observeEvent(rv$tmpA(),  { rv$current.obj <- rv$tmpA() })
  

  ############### SCREEN 3 ######################################
  output$screen3 <- renderUI({
    
    tagList(
      div(id='screen3',
          tags$h3('Processus 2'),
          mod_wf_wf1_B_ui('mod_B_nav')
      )
    )
  })
  
  rv$tmpB <- mod_wf_wf1_B_server("mod_B_nav", dataIn = reactive({rv$current.obj}) )
  observeEvent(rv$tmpB(),  { rv$current.obj <- rv$tmpB() })
  
  ############### SCREEN 4 ######################################
  output$screen4 <- renderUI({
    
    tagList(
      div(id='screen4',
          tags$h3('Processus 3'),
          mod_wf_wf1_C_ui('mod_C_nav')
      )
    )
  })
  
  rv$tmpC <- mod_wf_wf1_C_server("mod_C_nav", dataIn = reactive({rv$current.obj}) )
  observeEvent(rv$tmpC(),  { rv$current.obj <- rv$tmpC() })
  
  
  ############### SCREEN 5 ######################################
  output$screen4 <- renderUI({
    
    tagList(
      div(id='screen4',
          tags$h3('Summary')
      )
    )
  })
 
}


shinyApp(ui, server)
