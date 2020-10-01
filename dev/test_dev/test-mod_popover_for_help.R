library(shiny)
library(shinyBS)

source(file.path('../../R', 'mod_popover_for_help.R'), local=TRUE)$value


ui <- fluidPage(
  tagList(
    mod_popover_for_help_ui("pop")
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  dat <- list(title = h1('Test'), 
              content="explanation"
  )
  # dat <- list(
  #   title = tags$h3('Test of the module Popover.'),
  # content = tags$p("The content of the window.")
  # )
  mod_popover_for_help_server("pop",
                              data = dat)
}


shinyApp(ui, server)
