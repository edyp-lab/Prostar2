library(shinyjs)
library(DAPAR)

source(file.path('../../R', 'mod_plots_tracking.R'), local=TRUE)$value


ui <- fluidPage(
  tagList(
    htmlOutput('show_res_type'),
    htmlOutput('show_res_list'),
    htmlOutput('show_res_rand'),
    htmlOutput('show_res_col'),
    htmlOutput('show_res_list_indices'),
    htmlOutput('show_res_list_rand'),
    htmlOutput('show_res_list_col'),
  mod_plots_tracking_ui('plots_tracking')
  )
)


server <- function(input, output, session) {

  require(DAPARdata)
  data('Exp1_R25_prot')
  
  r <- reactiveValues(
    res = NULL
  )
  
  obj <- Exp1_R25_prot
  Biobase::fData(obj) <- cbind(Biobase::fData(obj), ProtOfInterest=sample(c(0,1), nrow(obj), TRUE))
  r$res <- callModule(mod_plots_tracking_server,'plots_tracking', 
             obj = obj, 
             params=reactive({NULL}), 
             reset=({FALSE}) )
  #callModule(mod_plots_tracking_server,'plots_tracking', obj = NULL, params=NULL, )
  
  output$show_res <- renderText({HTML(r$res())})
}


shinyApp(ui, server)
