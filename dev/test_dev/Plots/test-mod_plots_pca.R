library(shiny)
library(highcharter)
library(SummarizedExperiment)
library(shinycssloaders)
library(DT)

source(file.path("../../../R/Plots","mod_plots_pca.R"), local=TRUE)$value
source(file.path("../../../R","mod_format_DT.R"), local=TRUE)$value


ui <- fluidPage(
  mod_plots_pca_ui('pca')
)



server <- function(input, output, session) {
  
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  obj <- Exp1_R25_prot[[2]]
  coldata <- colData(Exp1_R25_prot)
  SummarizedExperiment::assay(obj)[which(is.na(SummarizedExperiment::assay(obj)))] <- 0
  
  mod_plots_pca_server('pca',
                       obj=reactive({obj}),
                       coldata=reactive({coldata})
  )
}


shinyApp(ui=ui, server=server)
