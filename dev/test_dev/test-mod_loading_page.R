library(shinyjs)


source(file.path('../../R', 'mod_loading_page.R'), local=TRUE)$value


ui <- fluidPage(
  tagList(
    mod_loading_page_ui('loadPage')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  mod_loading_page_server("loadPage")
}


shinyApp(ui, server)
