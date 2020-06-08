library(shiny)
library(highcharter)
library(SummarizedExperiment)
library(shinycssloaders)

source(file.path("../../R","mod_plots_pca.R"), local=TRUE)$value
source(file.path("../../R","mod_format_DT.R"), local=TRUE)$value

ui <- fluidPage(
  mod_plots_pca_ui('pca')
)



server <- function(input, output, session) {
  
  utils::data(Exp1_R25_pept, package='DAPARdata2')
  
  obj <- Exp1_R25_pept[[2]]
  conds <- colData(obj)[['Condition']]
  SummarizedExperiment::assay(obj)[which(is.na(SummarizedExperiment::assay(obj)))] <- 0
  
  callModule(mod_plots_pca_server,'pca',
             obj=reactive({obj}),
             conds=reactive({conds}),
             style=reactive({NULL})
  )
}


shinyApp(ui=ui, server=server)
