
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
    obj = NavStructure(name = "test",
                      stepsNames = c("Screen 1", "Screen 2","Screen 3"),
                      ll.UI = list( screenStep1 = uiOutput("screen1"),
                                    screenStep2 = uiOutput("screen2"),
                                    screenStep3 = uiOutput("screen3")),
                      isDone =  c(FALSE,FALSE, TRUE),
                      forceReset = FALSE)
  )
  

  
  callModule(mod_navigation_server, "test_nav",
             pages = reactive({navmodparams$obj})
  )
  
  resetFunc <- reactive({
    navmodparams$obj@isDone <- rep(FALSE, 3)
  })
  
  
  output$screen1 <- renderUI({
    tagList(
        tags$h1('Screen 1')
    )
  })
  
  
  
  output$screen2 <- renderUI({
    tagList(
        tags$h2('Screen 2')
    )
  })
  
  output$screen3 <- renderUI({
    tagList(
       tags$h3('Screen 3')
    )
  })

}


shinyApp(ui, server)
