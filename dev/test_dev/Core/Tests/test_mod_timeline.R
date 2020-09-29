source(file.path('../../../../R', 'global.R'), local=TRUE)$value
source(file.path('.', 'mod_timeline.R'), local=TRUE)$value


ui <- fluidPage(
  tagList(
    mod_timeline_ui("timeline")
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {

  
  config <- reactiveValues(
    stepsNames = c("A - Description", "A - Step 1", "A - Step 2", "A - Step 3"),
    mandatory = c(FALSE, FALSE, TRUE, TRUE),
    current.pos = 1,
    isDone = c(TRUE, FALSE, FALSE, FALSE)
  )
  
  rv.engine <- reactiveValues(
    current.pos = 1,
    actions = list(rst = TRUE,
                   nxt = TRUE,
                   prv = TRUE)
  )
  
  mod_timeline_server("timeline", style = 4, process_config = config, tl.update = rv.engine)
}


shinyApp(ui, server)
