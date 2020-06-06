library(highcharter)
library(shinycssloaders)
library(MSnbase)
library(DAPAR)

source(file.path("../../R","mod_plots_pca.R"), local=TRUE)$value
source(file.path("../../R","mod_format_DT.R"), local=TRUE)$value

ui <- fluidPage(
  mod_plots_pca_ui('pca')
)



server <- function(input, output, session) {
  
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  obj <- Exp1_R25_prot
  Biobase::exprs(obj)[which(is.na(Biobase::exprs(obj)))] <- 0
  # obj est un msnset
  #callModule(mod_plots_var_dist_server,'varDistPlot', obj = Exp1_R25_prot)
  callModule(mod_plots_pca_server,'pca', obj = reactive({NULL}))
  #callModule(mod_plots_var_dist_server,'varDistPlot', obj = mae)
}


shinyApp(ui=ui, server=server)