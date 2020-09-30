library(visNetwork)
library(highcharter)


source(file.path("../../../R/Plots","mod_graph_pept_prot.R"), local=TRUE)$value



ui <- fluidPage(
  mod_graph_pept_prot_ui('plots_cc')
)


server <- function(input, output, session) {
  
  utils::data(Exp1_R25_pept, package='DAPARdata2')
  
  r <- reactiveValues(
    settings = NULL
  )
  obj <- Exp1_R25_prot[[length(names(Exp1_R25_prot))]]
  
  mod_graph_pept_prot_server('plots_cc', 
                               obj = reactive({obj}),
                               cc = reactive({xxxx}),
                               matAdj = reactive({xxx})
  )
}


shinyApp(ui=ui, server=server)
