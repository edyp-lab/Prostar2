library(shiny)


mod_view_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width=4,selectInput(ns('chooseObj'), 
                                 'choose obj to modify', 
                                 choices=c('None'='None', 'obj 1'='obj1', 'obj 2'='obj2', 'obj3'='obj3'),
                                 width=150),
             uiOutput(ns('chooseItem_ui'))),
      column(width=4,actionButton(ns('btn'), 'Random change value in MODULE'),
             uiOutput(ns('showVar')),
             verbatimTextOutput(ns('showMsg'))
      )
    )
  )
}


mod_view_server <- function(input, output, session){
  ns <- session$ns
  
  rv <- reactiveValues(
    logMod = NULL
  )
  
  
  
  output$chooseItem_ui <- renderUI({
    req(input$chooseObj)
    if (input$chooseObj != 'obj1'){return(NULL)}
    
    selectInput(ns('chooseItem'), 
                'Choose item in object A',
                choices=c('None'='None', 'A'='A', 'B'='B', 'C'='C'),
                width = 150) 
  })
  
  observeEvent(input$btn,{
    input$chooseObj
    input$chooseItem
    
    if (input$chooseObj == 'obj2'){
      tmp <- paste0('mod_',sample(20:29, 1))
      session$userData$settings$obj2 <- tmp
      #session$userData$settings$obj2 <- tmp
    } else if(input$chooseObj == 'obj3'){
      tmp <- paste0('mod__',sample(30:39, 1))
      session$userData$settings$obj3 <- tmp
      #session$userData$settings$obj3 <- tmp
    } else if (input$chooseObj == 'obj1'){
      if (input$chooseItem == 'A'){
        tmp <- paste0('mod_A_',sample(1:10,1))
        session$userData$settings$obj1$A <- tmp
        #session$userData$settings$obj1$A <- tmp
      } else if (input$chooseItem == 'B') {
        tmp <- paste0('mod_B_',sample(1:10,1))
        session$userData$settings$obj1$B <- tmp
        #session$userData$settings$obj1$B <- tmp
      } else if (input$chooseItem == 'C'){
        tmp <- paste0('mod_C_',sample(1:10,1))
        session$userData$settings$obj1$C <- tmp
        #session$userData$settings$obj1$C <- tmp
      }
    }
   # rv$logMod <- ''
    
    
  })
  
  
  output$showVar <- renderUI({
    tagList(
      h3("Module"),
      p("contenu de la variable session$userData$settings$obj1 :"),
      HTML(unlist((session$userData$settings$obj1))),
      p("contenu de la variable session$userData$settings$obj2 :"),
      HTML(session$userData$settings$obj2),
      p("contenu de la variable session$userData$settings$obj3 :"),
      HTML(session$userData$settings$obj3)
    )
  })
  
  observeEvent(session$userData$settings,{ rv$logMod <- c(rv$logMod,paste0("module :" ,date(), '  ', "event on session$userData$settings"))})
  observeEvent(session$userData$settings$obj1,{ rv$logMod <- c(rv$logMod,paste0("module :" ,date(), '  ', "event on session$userData$settings$obj1"))})
  observeEvent(session$userData$settings$obj1$A,{ rv$logMod <- c(rv$logMod,paste0("module :" ,date(), '  ', "event on session$userData$settings$obj1$A"))})
  observeEvent(session$userData$settings$obj1$B,{ rv$logMod <- c(rv$logMod,paste0("module :" ,date(), '  ', "event on session$userData$settings$obj1$B"))})
  observeEvent(session$userData$settings$obj1$C,{ rv$logMod <- c(rv$logMod,paste0("module :" ,date(), '  ', "event on session$userData$settings$obj1$C"))})
  observeEvent(session$userData$settings$obj2,{ rv$logMod <- c(rv$logMod,paste0("module :" ,date(), '  ', "event on session$userData$settings$obj2"))})
  observeEvent(session$userData$settings$obj3,{ rv$logMod <- c(rv$logMod,paste0("module :" ,date(), '  ', "event on session$userData$settings$obj3"))})

  
  
  output$showMsg <- renderText({ paste0(rv$logMod, '\n')})
 
}




######------------------------------------------------------------------


ui <- fluidPage(
  tagList(
    fluidRow(
      column(width=6,
             fluidRow(
               column(width=4,h1('App'),
                      selectInput('chooseObj', 
                                  'choose obj to modify', 
                                  choices=c('None'='None', 'obj 1'='obj1', 'obj 2'='obj2', 'obj3'='obj3'),
                                  width=150)),
               column(width=4,uiOutput('chooseItem_ui')),
               column(width=4,actionButton('btn', 'Random change value in APP'))),
             uiOutput('showVar'),
             verbatimTextOutput('showLogVar')
      ),
      
      column(width=6,
             h1('Module'),
             mod_view_ui("view")
      )
      
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {

  
  session$userData$settings <- reactiveValues(
    obj1=list(A=1,
              B=1,
              C=1),
    obj2= 2,
    obj3 = 2
    )
  
  rv <- reactiveValues(
    logVar = NULL
    )
  
  logApp <- NULL
  
  
callModule(mod_view_server,id = "view")
  
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
      session$userData$settings$obj2 <- sample(20:29, 1)
    } else if(input$chooseObj == 'obj3'){
      session$userData$settings$obj3 <- sample(30:39, 1)
    } else if (input$chooseObj == 'obj1'){
      if (input$chooseItem == 'A'){
        session$userData$settings$obj1$A <- paste0('A_',sample(1:10,1))
      } else if (input$chooseItem == 'B') {
        session$userData$settings$obj1$B <- paste0('B_',sample(1:10,1))
      } else if (input$chooseItem == 'C'){
        session$userData$settings$obj1$C <- paste0('C_',sample(1:10,1))
      }
    }

  })
  
  output$showVar <- renderUI({
    tagList(
      h3("App"),
      p("contenu de la variable session$userData$settings$obj1 :"),
      HTML(unlist(session$userData$settings$obj1)),
      p("contenu de la variable session$userData$settings$obj2 :"),
      HTML(session$userData$settings$obj2),
      p("contenu de la variable session$userData$settings$obj3 :"),
      HTML(session$userData$settings$obj3)
    )
  })
  
  output$showLogVar <- renderText({ paste0(rv$logVar, '\n')})
  
  
  observeEvent(session$userData$settings,{ rv$logVar <- c(rv$logVar,paste0("App :" ,date(), '  ', "event on session$userData$settings"))})
  observeEvent(session$userData$settings$obj1,{ rv$logVar <- c(rv$logVar,paste0("App :" ,date(), '  ', "event on session$userData$settings$obj1"))})
  observeEvent(session$userData$settings$obj1$A,{ rv$logVar <- c(rv$logVar,paste0("App :" ,date(), '  ', "event on session$userData$settings$obj1$A"))})
  observeEvent(session$userData$settings$obj1$B,{ rv$logVar <- c(rv$logVar,paste0("App :" ,date(), '  ', "event on session$userData$settings$obj1$B"))})
  observeEvent(session$userData$settings$obj1$C,{ rv$logVar <- c(rv$logVar,paste0("App :" ,date(), '  ', "event on session$userData$settings$obj1$C"))})
  observeEvent(session$userData$settings$obj2,{ rv$logVar <- c(rv$logVar,paste0("App :" ,date(), '  ', "event on session$userData$settings$obj2"))})
  observeEvent(session$userData$settings$obj3,{ rv$logVar <- c(rv$logVar,paste0("App :" ,date(), '  ', "event on session$userData$settings$obj3")) })
  
  
}


shinyApp(ui, server)
