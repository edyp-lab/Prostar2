
source(file.path('../../R', 'mod_navigation.R'), local=TRUE)$value


ui <- fluidPage(
  tagList(
    mod_navigation_ui('test_nav')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  # navmodparams <- reactiveValues(
  #   processTest = list(name = "test",
  #                      stepsNames = c("screen1", "screen2","screen3"),
  #                      isMandatory = rep(FALSE,3),
  #                      ll.UI = list( screenStep1 = uiOutput("screen1"),
  #                                    screenStep2 = uiOutput("screen2"),
  #                                    screenStep3 = uiOutput("screen3")),
  #                     # rstFunc = reactive({resetFunc()}),
  #                       isDone =  rep(FALSE,3),
  #                       forceReset = FALSE,
  #                       iconType='bubble'
  #   )
  #   )

  navmodparams <- reactiveValues(
    obj = NavStructure(name = "test",
                      stepsNames = c("screen1", "screen2","screen3"),
                      isMandatory = rep(FALSE,3),
                      ll.UI = list( screenStep1 = uiOutput("screen1"),
                                    screenStep2 = uiOutput("screen2"),
                                    screenStep3 = uiOutput("screen3")),
                      #rstFunc = reactive({resetFunc()}),
                      isDone =  c(FALSE,FALSE, TRUE),
                      forceReset = FALSE,
                      iconType='bubble')
  )
  

  
  callModule(mod_navigation_server, "test_nav",
             pages = reactive({navmodparams$obj})
  )
  
  resetFunc <- reactive({
    nav_mod_params$stepsDone <- rep(FALSE, 3)
  })
  
  
  output$screen1 <- renderUI({
    tagList(
      div(
        id = "screen1",
        tags$p('screen 1')
      )
    )
  })
  
  
  
  output$screen2 <- renderUI({
    tagList(
      div(
        id = "screen2",
        tags$p('screen 2')
      )
    )
  })
  
  output$screen3 <- renderUI({
    tagList(
      div(
        id = "screen3",
        tags$p('screen 3')
      )
    )
  })

}


shinyApp(ui, server)
