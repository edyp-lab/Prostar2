library(shiny)


mod_A_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('show'))
    
  )
}


mod_A_server <- function(input, output, session, data){
  ns <- session$ns
  
  rv <- reactiveValues(
    res = NULL
  )
  
  
  observe({
    data()
    rv$res <- data()
  })
  
  
  output$show <- renderUI({
    p(paste0('show in module A : ', data(), collapse=' '))
  })

  return(reactive({rv$res}))
}


mod_B_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_A_ui(ns('modA')),
    uiOutput(ns('show'))
  )
}


mod_B_server <- function(input, output, session, data){
  ns <- session$ns
  
  r <- reactiveValues(
    res = NULL
  )
  
  r$res <- callModule(mod_A_server, 'modA',
                      data = reactive(data()))
  
  output$show <- renderUI({
    req(r$res())
    p(paste0('show in module B : ', r$res(), collapse=' '))
  })
}


######------------------------------------------------------------------


ui <- fluidPage(
  tagList(
    mod_B_ui('toto')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
 callModule(mod_B_server,id = "toto",data= reactive({"titi"}))
}


shinyApp(ui, server)