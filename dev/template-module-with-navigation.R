
ui <- fluidPage(
  tagList(
    br(),br(),
    mod_navigation_ui('nav')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  r.nav <- reactiveValues(
    name = "test",
    stepsNames = c("Screen 1", "Screen 2","Screen 3"),
    ll.UI = list( screenStep1 = uiOutput("screen1"),
                  screenStep2 = uiOutput("screen2"),
                  screenStep3 = uiOutput("screen3")),
    isDone =  c(FALSE,FALSE, FALSE),
    mandatory =  c(FALSE,TRUE, FALSE),
    reset = FALSE
  )
  
  #default values for the widgets
  r.widgets <- reactiveValues(
    select1 = 1,
    select2 = 1,
    select3 = 1
  )
  
  
  
  callModule(mod_navigation_server, "nav",style=2, pages = r.nav)
  
  observeEvent(req(r.nav$reset),{
    ## do not modify this part
    r.nav$isDone <- rep(FALSE, 3)
    r.nav$reset <- FALSE
    ## end of no modifiable part
    
    ## update widgets whose names are in r.widgets with the value in this list
    updateSelectInput(session,'select1', selected=r.params[['select1']])
    updateSelectInput(session,'select2', selected=r.params[['select2']])
    updateSelectInput(session,'select3', selected=r.params[['select3']])
  })
  
  
  
  ##------------------------------------------------------------
  
  
  observeEvent(input$done1,{r.nav$isDone[1] <- TRUE})
  observeEvent(input$done2,{r.nav$isDone[2] <- TRUE})
  observeEvent(input$done3,{r.nav$isDone[3] <- TRUE})
  
  
  output$screen1 <- renderUI({
    tagList(
      
    )
  })
  
  
  
  output$screen2 <- renderUI({
    tagList(
      
    )
  })
  
  
}


shinyApp(ui, server)