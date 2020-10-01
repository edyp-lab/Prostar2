library(shiny)
library(QFeatures)
library(DT)

source(file.path('../../R', 'global.R'), local=TRUE)$value
source(file.path('../../R', 'mod_format_DT.R'), local=TRUE)$value


ui <- fluidPage(
  mod_format_DT_ui('test_format_DT')
)


server <- function(input, output, session) {
  
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  obj <- Exp1_R25_prot
  #obj <- NULL
  
  mod_format_DT_server('test_format_DT', 
                       table2show = reactive({as.data.frame(assay(obj[[2]]))}),
                       style = reactive(NULL)
                       # style = reactive({ list(cols = colnames(colData(obj)),
                       #                         vals = colnames(colData(obj))[2],
                       #                         unique = unique(colData(obj)$Condition),
                       #                         pal = RColorBrewer::brewer.pal(3,'Dark2')[1:2])})
  ) 
  
  #callModule(mod_format_DT_server,'test_format_DT', table2show = reactive({NULL}))
  
}


shinyApp(ui, server)
