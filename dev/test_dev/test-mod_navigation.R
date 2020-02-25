
source(file.path('../../R', 'mod_navigation.R'), local=TRUE)$value


ui <- fluidPage(
  tagList(
    br(),br(),
    mod_navigation_ui('test_nav')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  navmodparams <- reactiveValues(
    obj = list(name = "test",
                      stepsNames = c("Screen 1", "Screen 2","Screen 3"),
                      ll.UI = list( screenStep1 = uiOutput("screen1"),
                                    screenStep2 = uiOutput("screen2"),
                                    screenStep3 = uiOutput("screen3")),
                      isDone =  c(FALSE,FALSE, FALSE),
                      forceReset = FALSE)
  )
  

  
  callModule(mod_navigation_server, "test_nav",
             pages = reactive({navmodparams$obj})
  )
  
  resetFunc <- reactive({
    navmodparams$obj@isDone <- rep(FALSE, 3)
  })
  
  
  
  observeEvent(input$done1,{navmodparams$obj@isDone[1] <- TRUE})
  observeEvent(input$done2,{navmodparams$obj@isDone[2] <- TRUE})
  observeEvent(input$done3,{navmodparams$obj@isDone[3] <- TRUE})
                            
  output$screen1 <- renderUI({
   
      tagList(
        tags$h1('Screen 1'),
        actionButton('done1', 'Set done 1'),
        selectInput('select1', 'Select 1', choices = 1:5)
    )

  })
  
  
  
  output$screen2 <- renderUI({
    tagList(
        tags$h2('Screen 2'),
        actionButton('done2', 'Set done 2'),
        selectInput('select2', 'Select 2', choices = 1:5)
    )
  })
  
  output$screen3 <- renderUI({
    tagList(
       tags$h3('Screen 3'),
       actionButton('done3', 'Set done 3'),
       selectInput('select3', 'Select 3', choices = 1:5)
    )
  })

}


shinyApp(ui, server)
