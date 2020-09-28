library(rhandsontable)
library(shiny)
source(file.path('../nav_style/', 'mod_navigation.R'), local=TRUE)$value
source(file.path('../../R', 'global.R'), local=TRUE)$value

ui <- fluidPage(
  tagList(
    uiOutput('show'),
    uiOutput('obj')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  r.nav <- reactiveValues(
    name = "test",
    stepsNames = c("Description", "Step 1", "Step 2", "Step 3"),
    ll.UI = list( screenStep1 = uiOutput("screen1"),
                  screenStep2 = uiOutput("screen2"),
                  screenStep3 = uiOutput("screen3"),
                  screenStep4 = uiOutput("screen4")),
    isDone =  c(TRUE, FALSE, FALSE, FALSE),
    mandatory =  c(FALSE, FALSE, TRUE, TRUE),
    reset = FALSE
    
  )
  
  
  
  screens <- mod_navigation_server("test_nav",
                                  style = 1,
                                  pages = r.nav  )
  
  output$show <- renderUI({
    tagList(
      mod_navigation_ui("test_nav"),
      screens()
    )
  })
  
  output$obj <- renderUI({
    paste0(unlist(rv.data$data), collapse=' ')
  })
  
  
  reset <- reactive({
    r.nav$isDone <- c(TRUE,rep(FALSE, 3))
    for (i in 1:length(r.nav$stepsNames))
      shinyjs::reset(paste0('screen', i))
  })
  
  
  observeEvent(req(r.nav$reset),{
    r.nav$reset  <- FALSE
    print('reset activated from navigation module')
    reset()
  })

  
  ##------------------------------------------------------------
  #default values for the widgets
  r.params <- reactiveValues(
    select1 = 1,
    select2 = 10,
    select3 = 100
  )
  
  rv.data <- reactiveValues(
    data = list(original = 'original')
  )
  
  #*observeEvent(input$done1,{r.nav$isDone[1] <- TRUE})
  observeEvent(input$done2,{
    r.nav$isDone[2] <- TRUE
    rv.data$data <- append(rv.data$data, input$select1)})
  
  observeEvent(input$done3,{r.nav$isDone[3] <- TRUE
  rv.data$data <- append(rv.data$data, input$select2)})
  
  observeEvent(input$done4,{r.nav$isDone[4] <- TRUE
  rv.data$data <- append(rv.data$data, input$select3)})
  
  
  output$screen1 <- renderUI({
    tagList(
      tags$h1('Description of the module')
    )
      
  })
  
  
  
  output$screen2 <- renderUI({
 
        tagList(
          div(id='screen2',
              tags$h2('Step 1'),
      actionButton('done2', 'Set done 1'),
      selectInput('select1', 'Select 1', 
                  choices = 1:5, 
                  selected = 1,
                  width = '150px')
        )
    )
  })
  
  
  output$screen3 <- renderUI({
    
        tagList(
          div(id='screen3',
              tags$h3('Step 2'),
      actionButton('done3', 'Set done 2'),
      selectInput('select2', 'Select 2',
                  choices = 1:5,
                  selected = r.params$select2,
                  width = '150px')
        )
    )
  })
  
  output$screen4 <- renderUI({
    
        tagList(
          div(id='screen4',
              tags$h1('Step 4'),
      actionButton('done4', 'Validate')
    )
    )
  })
  
}


shinyApp(ui, server)
