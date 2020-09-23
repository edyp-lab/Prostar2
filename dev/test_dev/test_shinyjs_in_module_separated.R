library(shinyjs)

# Module UI

#' @title   mod_navigation_ui and mod_navigation_server
#' @description  A shiny Module. The sass source code for timeline was inspired by 
#'  : https://codepen.io/cjl750/pen/mXbMyo
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' @param isDone xxxxx
#' @param screens xxxxx
#' @param rstFunc xxxxx
#' @param iconType xxxxxx
#'
#' @rdname mod_navigation
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList
#' @importFrom shinyjs disabled inlineCSS
mod_navigation_ui <- function(id){
  ns <- NS(id)

}

# Module Server

#' @rdname mod_navigation
#' @export
#' @keywords internal
#' @import shiny shinyjs
#' @importFrom sass sass
mod_navigation_server <- function(id, pages, btns){

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

   timeline <- reactive({
      tagList(
        shinyjs::useShinyjs(),
        if ('reset' %in% btns) 
          actionButton(ns("rstBtn"), "reset"),
        if ('undo' %in% btns) 
          actionButton(ns("undoBtn"), "undo"),
        if ('skip' %in% btns)
          actionButton(ns("skipBtn"), "skip"),
        actionButton(ns('test'), 'test')
      )
    })
    
   
   observeEvent(input$rstBtn,{ pages$reset <- input$rstBtn})
   observeEvent(input$skipBtn,{ pages$skip <- input$skipBtn})
   observeEvent(input$undoBtn,{ pages$undo <- input$undoBtn})
   
   observe({
     input$rstBtn
     shinyjs::toggleState('test', condition=FALSE)
   })
   
   list(timeline = reactive(timeline()),
                  screens = reactive(pages$ll.UI)
    )

}

)}







ui <- fluidPage(
  tagList(
    uiOutput('show')
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
    isDone =  c(FALSE, FALSE, FALSE),
    mandatory =  c(FALSE, TRUE, FALSE),
    reset = FALSE,
    skip = FALSE,
    undo = FALSE
  )
  
 
  rv.nav <- mod_navigation_server("test_nav", 
                                  pages=r.nav,
                                  btns = c('reset', 'skip')
  )
  
  observeEvent(req(r.nav$reset),{
  print('reset button activated from module')
    r.nav$reset <- FALSE
  })
  
  observeEvent(req(r.nav$skip),{
    print('skip button activated from module')
    r.nav$skip <- FALSE
  })
  
  observeEvent(req(r.nav$undo),{
    print('undo button activated from module')
    r.nav$undo <- FALSE
  })
  
  
  output$show <- renderUI({
    tagList(
      rv.nav$timeline(),
      rv.nav$screens()
    )
  })

  
}


shinyApp(ui, server)

