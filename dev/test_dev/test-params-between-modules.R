library(shiny)


mod_view_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width=4,selectInput(ns('chooseObj'), 
                         'choose obj to modify', 
                         choices=c('None'='None', 'obj 1'='obj1', 'obj 2'='obj2', 'obj3'='obj3'),
                         width=150)),
      column(width=4,uiOutput(ns('chooseItem_ui'))),
      column(width=4,actionButton(ns('btn'), 'Random change value in MODULE'),
                      actionButton(ns('session_btn'), 'add values to session'),
                      uiOutput(ns('showVar')),
                      verbatimTextOutput(ns('showMsg'))
      )
)
  )
}


mod_view_server <- function(input, output, session, data){
  ns <- session$ns
  
  rv <- reactiveValues(
    res = NULL,
    logMod = NULL
  )
  
  
  
  observeEvent(data,{ rv$res <- data})
  
  
  
  
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
      data$obj2 <- tmp
      session$userData$settings$obj2 <- tmp
    } else if(input$chooseObj == 'obj3'){
      tmp <- paste0('mod__',sample(30:39, 1))
      data$obj3 <- tmp
      session$userData$settings$obj3 <- tmp
    } else if (input$chooseObj == 'obj1'){
      if (input$chooseItem == 'A'){
        tmp <- paste0('mod_A_',sample(1:10,1))
        data$obj1$A <- tmp
        session$userData$settings$obj1$A <- tmp
      } else if (input$chooseItem == 'B') {
        tmp <- paste0('mod_B_',sample(1:10,1))
        data$obj1$B <- tmp
        session$userData$settings$obj1$B <- tmp
      } else if (input$chooseItem == 'C'){
        tmp <- paste0('mod_C_',sample(1:10,1))
        data$obj1$C <- tmp
        session$userData$settings$obj1$C <- tmp
      }
    }
    rv$logMod <- ''

    
  })
  
  
  output$showVar <- renderUI({
    tagList(
      h3("Module"),
      p("contenu de la variable data$obj1 :"),
      HTML(unlist(reactiveValuesToList(data$obj1))),
      p("contenu de la variable data$obj2 :"),
      HTML(data$obj2),
      p("contenu de la variable data$obj3 :"),
      HTML(data$obj3),
      br(),
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
  
  observeEvent(data,{ rv$logMod <- c(rv$logMod,paste0("module :" ,date(), '  ', "event on data"))})
  observeEvent(data$obj1,{ rv$logMod <- c(rv$logMod,paste0("module :" ,date(), '  ', "event on data$obj1"))})
  observeEvent(data$obj1$A,{ rv$logMod <- c(rv$logMod,paste0("module :" ,date(), '  ', "event on data$obj1$A"))})
  observeEvent(data$obj1$B,{ rv$logMod <- c(rv$logMod,paste0("module :" ,date(), '  ', "event on data$obj1$B"))})
  observeEvent(data$obj1$C,{ rv$logMod <- c(rv$logMod,paste0("module :" ,date(), '  ', "event on data$obj1$C"))})
  observeEvent(data$obj2,{ rv$logMod <- c(rv$logMod,paste0("module :" ,date(), '  ', "event on data$obj2"))})
  observeEvent(data$obj3,{ rv$logMod <- c(rv$logMod,paste0("module :" ,date(), '  ', "event on data$obj3"))})
  
  
  output$showMsg <- renderText({ paste0(rv$logMod, '\n')})
  
  return(reactive({rv$res}))
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
             verbatimTextOutput('showLogVar'),
             #tags$head(tags$style("#showLogVar{overflow-y:scroll; max-height: 150px; background: ghostwhite;}")),
             hr(),
             uiOutput('showRes'),
             verbatimTextOutput('showLogRes')
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
  
  r <- reactiveValues(
    obj1 = reactiveValues(
                  A='A',
                 B='B',
                 C='C'
          ),
    obj2 = 'obj2',
    obj3 = 'obj3'
  )
  
  
  rv <- reactiveValues(
    res = NULL,
    logRes = NULL
  )
  
  
  
  session$userData$settings <- list(
    obj1=list(A=1,
              B=1,
              C=1),
    obj2= 2,
    obj3 = 2
    )
  
  logApp <- NULL
  
  
  rv$res <- callModule(mod_view_server,id = "view",data= r)
  
  output$chooseItem_ui <- renderUI({
    req(input$chooseObj)
    if (input$chooseObj != 'obj1'){return(NULL)}
    
    selectInput('chooseItem', 
                'Choose item in object A',
                choices=c('None'='None', 'A'='A', 'B'='B', 'C'='C'),
                width = 150) 
  })
  
  observeEvent(rv$res(),{
    rv$logRes <- ''
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
    
    rv$logVar <- ''
  })
  
  output$showVar <- renderUI({
    tagList(
      h3("App"),
      p("contenu de la variable r$obj1 :"),
      HTML(unlist(reactiveValuesToList(r$obj1))),
      p("contenu de la variable r$obj2 :"),
      HTML(r$obj2),
      p("contenu de la variable r$obj3 :"),
      HTML(r$obj3)
    )
  })
  
  output$showRes <- renderUI({
    req(rv$res())
    tagList(
      h3("App - res"),
      p("contenu de la variable rv$res$obj1 :"),
      HTML(unlist(reactiveValuesToList(rv$res()$obj1))),
      p("contenu de la variable rv$res$obj2 :"),
      HTML(rv$res()$obj2),
      p("contenu de la variable rv$res$obj3 :"),
      HTML(rv$res()$obj3)
    )
  })
  
  output$showLogRes <- renderText({ paste0(rv$logRes, '\n')})
  output$showLogVar <- renderText({ paste0(rv$logVar, '\n')})
  
  
   observeEvent(rv$res(),{ rv$logRes <- c(rv$logRes,paste0("res on App :" ,date(), '  ', "event on rv$res()"))})
   observeEvent(rv$res()$obj1,{ rv$logRes <- c(rv$logRes,paste0("res on App :" ,date(), '  ', "event on rv$res()$obj1"))})
   observeEvent(rv$res()$obj1$A,{ rv$logRes <- c(rv$logRes,paste0("res on App :" ,date(), '  ', "event on rv$res()$obj1$A"))})
   observeEvent(rv$res()$obj1$B,{ rv$logRes <- c(rv$logRes,paste0("res on App :" ,date(), '  ', "event on rv$res()$obj1$B"))})
   observeEvent(rv$res()$obj1$C,{ rv$logRes <- c(rv$logRes,paste0("res on App :" ,date(), '  ', "event on rv$res()$obj1$C"))})
   observeEvent(rv$res()$obj2,{ rv$logRes <- c(rv$logRes,paste0("res on App :" ,date(), '  ', "event on rv$res()$obj2"))})
   observeEvent(rv$res()$obj3,{ rv$logRes <- c(rv$logRes,paste0(date(), '  ', "event on rv$res()$obj3")) })
   
   observeEvent(r,{ rv$logVar <- c(rv$logVar,paste0("App :" ,date(), '  ', "event on r"))})
   observeEvent(r$obj1,{ rv$logVar <- c(rv$logVar,paste0("App :" ,date(), '  ', "event on r$obj1"))})
   observeEvent(r$obj1$A,{ rv$logVar <- c(rv$logVar,paste0("App :" ,date(), '  ', "event on r$obj1$A"))})
   observeEvent(r$obj1$B,{ rv$logVar <- c(rv$logVar,paste0("App :" ,date(), '  ', "event on r$obj1$B"))})
   observeEvent(r$obj1$C,{ rv$logVar <- c(rv$logVar,paste0("App :" ,date(), '  ', "event on r$obj1$C"))})
   observeEvent(r$obj2,{ rv$logVar <- c(rv$logVar,paste0("App :" ,date(), '  ', "event on r$obj2"))})
   observeEvent(r$obj3,{ rv$logVar <- c(rv$logVar,paste0("App :" ,date(), '  ', "event on r$obj3")) })
   
  
}


shinyApp(ui, server)
