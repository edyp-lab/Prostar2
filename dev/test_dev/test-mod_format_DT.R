
source(file.path('../../R', 'mod_format_DT.R'), local=TRUE)$value


ui <- fluidPage(
  mod_format_DT_ui('test_format_DT')
)


server <- function(input, output, session) {
  
  
  require(DAPARdata)
  data('Exp1_R25_prot')
  obj <- Exp1_R25_prot
  
  #callModule(mod_format_DT_server,'test_format_DT', table2show = Biobase::pData(obj))
  callModule(mod_format_DT_server,'test_format_DT', table2show = NULL)
  
}


shinyApp(ui, server)
