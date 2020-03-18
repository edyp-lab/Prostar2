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
  
  r$res <- callModule(mod_plots_tracking_server,'plots_tracking', 
             obj = Exp1_R25_prot, 
             params=reactive({NULL}), 
             reset=({FALSE}) )
  #callModule(mod_plots_tracking_server,'plots_tracking', obj = NULL, params=NULL, )
  
  
  
  
  output$show_res_type <- renderText({HTML(r$res()$type)})
  output$show_res_list <- renderText({HTML(r$res()$list)})
  output$show_res_rand <- renderText({HTML(r$res()$rand)})
  output$show_res_col <- renderText({HTML(r$res()$col)})
  output$show_res_list_indices <- renderText({HTML(r$res()$list.indices)})
  output$show_res_list_rand <- renderText({HTML(r$res()$list.rand)})
  output$show_res_list_col <- renderText({HTML(r$res()$list.col)})
  
}


shinyApp(ui, server)
