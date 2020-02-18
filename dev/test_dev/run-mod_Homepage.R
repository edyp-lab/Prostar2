
source(file.path('../../R', 'mod_homepage.R'), local=TRUE)$value


ui <- fluidPage(
 
  navbarPage(
    mod_homepage_ui('home')
    )
  )

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  callModule(mod_homepage,'home')
  
}


shinyApp(ui, server)
