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
mod_navigation_server <- function(id, pages){

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(pages(), {
      print(pages())
      shinyjs::toggle("rstBtn", condition = pages() == 3)
    })
    
    
    timeline <- renderUI({
      tagList(
        shinyjs::useShinyjs(),
        shinyjs::hidden(actionButton(ns("rstBtn"), "reset"))
      )
    })
    # list(bars=reactive({bars()}),
    #      screens=reactive(tagList(pages$ll.UI))
    # )
    
    reactive(list(timeline = timeline(),
                  screens = tagList(pages()$ll.UI))
  )
}

)}







ui <- fluidPage(
  tagList(
    uiOutput('show'),
    actionButton('test', 'Click me')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  rv.nav <- mod_navigation_server("test_nav", pages=reactive(input$test))
  
  output$show <- renderUI({
    tagList(
      mod_navigation_ui('test_nav')
      #rv.nav$bars(),
      #rv.nav$screens()
    )
  })

  
}


shinyApp(ui, server)

