
source(file.path('~/Github/2.0/Prostar2/R', 'mod_navigation.R'), local=TRUE)$value

source(file.path('~/Github/2.0/Prostar2/R', 'mod_filtering_protein.R'), local=TRUE)$value
source(file.path('~/Github/2.0/DAPAR/R', 'missingValuesFilter.R'), local=TRUE)$value
source(file.path('~/Github/2.0/DAPAR/R', 'utils.R'), local=TRUE)$value
source(file.path("~/Github/2.0/Prostar/inst/ProstarApp/src", "modules/Misc/modulePopover.R"),  local = TRUE)$value
source(file.path("~/Github/2.0/Prostar/inst/ProstarApp/src", "modules/Plots/moduleGroupMVPlots.R"),  local = TRUE)$value
source(file.path("~/Github/2.0/Prostar/inst/ProstarApp/src/modules/Menu_Home/moduleSettings.R"),  local = TRUE)$value

library(MSnbase)
library(rhandsontable)


ui <- fluidPage(
  mod_filtering_protein_ui('filtering_protein')
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  library(DAPARdata)
  data("Exp1_R25_prot")
  data <- get("Exp1_R25_prot")
  
  # obj est un msnset de type protein
  callModule(mod_filtering_protein_server,'filtering_protein',
             obj = data)
}


shinyApp(ui=ui, server=server)
