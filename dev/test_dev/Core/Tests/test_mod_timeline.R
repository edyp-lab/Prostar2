source(file.path('../../../../R', 'global.R'), local=TRUE)$value
source(file.path('.', 'mod_timeline.R'), local=TRUE)$value


ui <- fluidPage(
  tagList(
    mod_timeline_ui('time')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {

  
  pages <- reactiveValues(
    stepsNames = c("A - Description", "A - Step 1", "A - Step 2", "A - Step 3"),
    mandatory = c(FALSE, FALSE, TRUE, TRUE),
    current = 2,
    isDone = c(TRUE, FALSE, FALSE, FALSE),
    nextBtn = TRUE,
    prevBtn = TRUE
  )
  
  
  pos <- mod_timeline_server("time", style = 2,pages = pages)

  observeEvent(req(pos()$rstBtn), {
    print(pos()$rstBtn)
  })

}


shinyApp(ui, server)
