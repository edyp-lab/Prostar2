
source(file.path('../../R', 'mod_homepage.R'), local=TRUE)$value
source(file.path('../../R', 'mod_insert_md.R'), local=TRUE)$value


ui <- fluidPage(
 
  navbarPage(
    mod_homepage_ui('home')
    )
  )

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  mod_homepage_server('home')
  
}


shinyApp(ui, server)
