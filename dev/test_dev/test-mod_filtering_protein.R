
source(file.path('../../R', 'mod_navigation.R'), local=TRUE)$value

source(file.path('../../R', 'mod_filtering_protein.R'), local=TRUE)$value
source(file.path('../../R', 'missingValuesFilter.R'), local=TRUE)$value
source(file.path("../../R", "modules/Misc/modulePopover.R"),  local = TRUE)$value
source(file.path("../../R", "modules/Plots/moduleGroupMVPlots.R"),  local = TRUE)$value
source(file.path("../../R", "mod_settings.R"),  local = TRUE)$value


library(rhandsontable)
library(DAPAR2)


ui <- fluidPage(
  mod_filtering_protein_ui('filtering_protein')
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  library(DAPARdata2)
  data("Exp1_R25_prot")
  data <- get("Exp1_R25_prot")
  
  # obj est un msnset de type protein
  callModule(mod_filtering_protein_server,'filtering_protein',
             obj = data)
}


shinyApp(ui=ui, server=server)
