library(shiny)
library(S4Vectors)
library(highcharter)
library(visNetwork)
library(SummarizedExperiment)


source(file.path("../../R","mod_format_DT.R"), local=TRUE)$value
source(file.path("../../R/Plots","mod_plots_se_explorer.R"), local=TRUE)$value
source(file.path("../../R/Plots","mod_plots_cc.R"), local=TRUE)$value

ui <- fluidPage(
  mod_plots_cc_ui('plots_cc')
)


server <- function(input, output, session) {
  
  utils::data(Exp1_R25_pept, package='DAPARdata2')
  
  obj <- Exp1_R25_pept[[2]]
  
  
  mod_plots_cc_server('plots_cc', 
                      cc = reactive({metadata(obj)$list.cc}),
                      matAdj = reactive({metadata(obj)$list.matAdj}),
                      obj = reactive({obj})
  )
}

shinyApp(ui=ui, server=server)
