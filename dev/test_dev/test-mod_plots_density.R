library(highcharter)
library(DAPAR)

ui <- fluidPage(
  mod_plots_density_ui('plots_density')
)



# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  
  library(DAPARdata)
  data("Exp1_R25_prot")
  
  
  # obj est un msnset
  callModule(mod_plots_density_server,'plots_density',
             obj = Exp1_R25_prot)
  
}


shinyApp(ui=ui, server=server)
