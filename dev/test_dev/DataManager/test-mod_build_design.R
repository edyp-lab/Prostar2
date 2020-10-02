library(shiny)
library(shinyjs)
library(rhandsontable)
source(file.path('../../R/DataManager', 'mod_build_design.R'), local=TRUE)$value
source(file.path('../../R/DataManager', 'mod_build_design_example.R'), local=TRUE)$value
source(file.path('../../R', 'mod_insert_md.R'), local=TRUE)$value
source(file.path('../../R', 'global.R'), local=TRUE)$value


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
  # rv.test$res <- mod_build_design_server('buildDesign', 
  #                                        sampleNames=reactive({colnames(colData(obj))}))
  rv.test$res <- mod_build_design_server('buildDesign', 
                                         sampleNames=reactive({colData(obj)$Sample.name}))
  
  observeEvent(req(rv.test$res() ),{
    print(rv.test$res() )
  })
}


shinyApp(ui, server)
