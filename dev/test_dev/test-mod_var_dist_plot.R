library(highcharter)
library(shinycssloaders)
library(MSnbase)
library(DAPAR)

source("~/Github/master/Prostar/inst/ProstarApp/ui/ui_Configure.R")
# plotWidth <- "800px"
# plotHeight <- "600px"

ui <- fluidPage(
  mod_var_dist_plot_ui('varDistPlot')
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  
  library(DAPARdata)
  data("Exp1_R25_prot")
  data <- get("Exp1_R25_prot")
  
  # obj est un msnset
  callModule(mod_var_dist_plot_server,'varDistPlot', obj = data)
}


shinyApp(ui=ui, server=server)