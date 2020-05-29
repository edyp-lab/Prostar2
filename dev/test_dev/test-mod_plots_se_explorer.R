library(dplyr)
library(DT)
library(shiny)


source(file.path("../../R","mod_plots_msnset_explorer.R"), local=TRUE)$value
source(file.path("../../R","mod_plots_legend_colored_exprs.R"), local=TRUE)$value


ui <- fluidPage(
  mod_plots_msnset_explorer_ui('msnset_explorer')
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  require(DAPARdata2)
  data('Exp1_R25_prot')
  obj <- Exp1_R25_prot[[2]]
  metadata <- metadata(Exp1_R25_prot)
  colData <- colData(Exp1_R25_prot)
  
  callModule(mod_plots_msnset_explorer_server,'msnset_explorer',
             obj = reactive({obj}),
             metadata = reactive({metadata}),
             colData = reactive({colData}))
}


shinyApp(ui, server)
