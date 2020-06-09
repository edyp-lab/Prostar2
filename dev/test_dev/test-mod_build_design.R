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
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  obj <- Exp1_R25_prot
  
  rv.test <- reactiveValues(
    res = NULL
  )
  rv.test$res <- callModule(mod_build_design_server, 
             'buildDesign', 
             sampleNames=reactive({colnames(Biobase::exprs(obj))}))
  
  observeEvent(req(rv.test$res() ),{
    print(rv.test$res() )
  })
}


shinyApp(ui, server)
