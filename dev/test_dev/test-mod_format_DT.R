library(Features)
source(file.path('../../R', 'global.R'), local=TRUE)$value
source(file.path('../../R', 'mod_format_DT.R'), local=TRUE)$value


ui <- fluidPage(
  mod_format_DT_ui('test_format_DT')
)


server <- function(input, output, session) {
  
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  obj <- Exp1_R25_prot
  
  callModule(mod_format_DT_server,'test_format_DT', table2show = reactive({as.data.frame(colData(obj))}))
  #callModule(mod_format_DT_server,'test_format_DT', table2show = reactive({NULL}))
  
}


shinyApp(ui, server)
