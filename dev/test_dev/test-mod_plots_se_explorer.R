library(shiny)
library(DT)
library(SummarizedExperiment)


source(file.path("../../R","mod_plots_se_explorer.R"), local=TRUE)$value
source(file.path("../../R","mod_plots_legend_colored_exprs.R"), local=TRUE)$value


ui <- fluidPage(
  mod_plots_se_explorer_ui('se_explorer')
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  obj <- Exp1_R25_prot[[2]]
  originOfValues <- metadata(Exp1_R25_prot)[['OriginOfValues']]
  colData <- colData(Exp1_R25_prot)
  
  callModule(mod_plots_se_explorer_server,'se_explorer',
             obj = reactive({obj}),
             originOfValues = reactive({originOfValues}),
             colData = reactive({colData}))
}


shinyApp(ui, server)
