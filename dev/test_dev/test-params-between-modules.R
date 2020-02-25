

ui <- fluidPage(
  tagList(
    selectInput('chooseObj', 'choose obj to modify', choices=c('None'='None', 'obj 1'='obj1', 'obj 2'='obj 3', 'obj3'='obj3')),
    uiOutput('chooseItem_ui'),
    uiOutput('showVar')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  r <- reactiveValues(
    obj1 = list(A='A',
                 B='B',
                 C='C'
          ),
    obj2 = 'obj2',
    obj3 = 'obj3'
  )
  
  
  output$chooseItem_ui <- renderUI({
    req(input$chooseObj)
    if (input$chooseObj != 'obj1'){return(NULL)}
    
    selectInput('chooseItem', 'Choose item in object A',choices=c('None'='None', 'A'='A1', 'B'='B', 'C'='C')) 
  })
  
  
  output$showVar <- renderUI({
    tagList(
      h3("App"),
      p("contenu de la variable r$obj1 :"),
      HTML(unlist(r$obj1)),
      p("contenu de la variable r$obj2 :"),
      HTML(r$obj2),
      p("contenu de la variable r$obj3 :"),
      HTML(r$obj3)
    )
  })
  
}


shinyApp(ui, server)
