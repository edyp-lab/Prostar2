source(file.path('../../R', 'mod_insert_md.R'), local=TRUE)$value
source(file.path('../../R', 'mod_release_notes.R'), local=TRUE)$value
source(file.path('../../R', 'global.R'), local=TRUE)$value


ui <- fluidPage(
  tagList(
    mod_release_notes_ui('rl')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  mod_release_notes_server("rl")
}


shinyApp(ui, server)
