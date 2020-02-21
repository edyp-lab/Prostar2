library(shinyjs)
library(rhandsontable)
source(file.path('../../R', 'mod_build_design.R'), local=TRUE)$value


ui <- fluidPage(
  useShinyjs(),
  tagList(
    mod_build_design_ui('buildDesign')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  require(DAPARdata)
  data(Exp1_R25_prot)
  obj <- Exp1_R25_prot
  
  callModule(mod_build_design_server, 
             'buildDesign', 
             sampleNames=reactive({colnames(Biobase::exprs(obj))}))
}


shinyApp(ui, server)
