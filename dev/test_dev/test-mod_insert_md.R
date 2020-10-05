
source(file.path('../../R', 'mod_insert_md.R'), local=TRUE)$value
source(file.path('../../R', 'global.R'), local=TRUE)$value


ui <- fluidPage(
  mod_insert_md_ui('FAQ_MD')
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  mod_insert_md_server("FAQ_MD", URL_FAQ)
}


shinyApp(ui, server)
