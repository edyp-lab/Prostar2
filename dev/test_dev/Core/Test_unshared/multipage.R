library(shiny)
library(shinyjs)

NUM_PAGES <- 5

screens <-
  lapply(seq(NUM_PAGES), function(i) {
    div(
      class = "page",
      id = paste0("step", i),
      h4(paste0("Step", i)),
      actionButton(paste0('btn',i), paste0('btn ',i))
    )
  })

#screens <- list()
ui <- fluidPage(
  useShinyjs(),
  hidden(
    lapply(seq(NUM_PAGES), function(i) {
      div(
        class = "page",
        id = paste0("step", i),
        h4(paste0("Step", i)),
        actionButton(paste0('btn',i), paste0('btn ',i))
      )
    })
  ),
  br(),
  actionButton("prevBtn", "< Previous"),
  actionButton("nextBtn", "Next >"),
  actionButton('toggle', 'toggle')
)

server <- function(input, output, session) {
  rv <- reactiveValues(page = 1)
  
  observe({
    toggleState(id = "prevBtn", condition = rv$page > 1)
    toggleState(id = "nextBtn", condition = rv$page < NUM_PAGES)
    hide(selector = ".page")
    show(paste0("step", rv$page))
  })
  
  observeEvent(input$toggle,{
    shinyjs::toggleState('step2', condition=input$toggle%%2 == 0)
  })
  
  navPage <- function(direction) {
    rv$page <- rv$page + direction
  }
  
  observeEvent(input$prevBtn, navPage(-1))
  observeEvent(input$nextBtn, navPage(1))
}

shinyApp(ui, server)