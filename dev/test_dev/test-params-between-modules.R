library(shiny)

ui <- fluidPage(
  tagList(
    fluidRow(
      column(width=4,selectInput('chooseObj', 
                                  'choose obj to modify', 
                                  choices=c('None'='None', 'obj 1'='obj1', 'obj 2'='obj2', 'obj3'='obj3'),
                                 width=150)
             ),
      column(width=4,uiOutput('chooseItem_ui')
             ),
      column(width=4,actionButton('btn', 'Random change value')
      )
    ),
    
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
    
    selectInput('chooseItem', 
                'Choose item in object A',
                choices=c('None'='None', 'A'='A', 'B'='B', 'C'='C'),
                width = 150) 
  })
  
  
  observeEvent(input$btn,{
    input$chooseObj
    input$chooseItem
    
    if (input$chooseObj == 'obj2'){
      r$obj2 <- sample(20:29, 1)
    } else if(input$chooseObj == 'obj3'){
      r$obj3 <- sample(30:39, 1)
    } else if (input$chooseObj == 'obj1'){
      if (input$chooseItem == 'A'){
        r$obj1$A <- paste0('A_',sample(1:10,1))
      } else if (input$chooseItem == 'B') {
        r$obj1$B <- paste0('B_',sample(1:10,1))
      } else if (input$chooseItem == 'C'){
        r$obj1$C <- paste0('C_',sample(1:10,1))
      }
    }
    
  })
  
  output$showVar <- renderUI({
    r$obj1
    r$obj2
    r$obj3
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
