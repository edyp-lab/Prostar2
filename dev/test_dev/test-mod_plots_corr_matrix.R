library(highcharter)
library(DAPAR)

ui <- fluidPage(
  mod_plots_corr_matrix_ui('plots_corr_matrix')
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  
  library(DAPARdata)
  data("Exp1_R25_prot")
  
  # obj est un msnset
  callModule(mod_plots_corr_matrix_server,'plots_corr_matrix', obj = Exp1_R25_prot)
}


shinyApp(ui=ui, server=server)


## To be copied in the UI
# mod_plots_corr_matrix_ui("plots_corr_matrix_ui_1")

## To be copied in the server
# callModule(mod_plots_corr_matrix_server, "plots_corr_matrix_ui_1")