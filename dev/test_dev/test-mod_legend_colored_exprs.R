
ui <- fluidPage(
  mod_legend_colored_exprs_ui("legend_colored_exprs")
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  callModule(mod_legend_colored_exprs_server,'legend_colored_exprs')
  
}


shinyApp(ui=ui, server=server)