
source(file.path('../../R', 'mod_popover_for_help.R'), local=TRUE)$value


ui <- fluidPage(
  tagList(
    mod_popover_for_help_ui('pop')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  dat <- list(title = tags$h1('Test'), 
              content=HTML("<ul><li>First item</li> <li>Second item</li></ul>")
              )
  # dat <- list(
  #   title = tags$h3('Test of the module Popover.'),
  # content = tags$p("The content of the window.")
  # )
  callModule(mod_popover_for_help_server, "pop", data=reactive(dat))
}


shinyApp(ui, server)
