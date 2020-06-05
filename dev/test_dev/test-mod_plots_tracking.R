library(shiny)
library(SummarizedExperiment)


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
  
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  keyId <- metadata(Exp1_R25_prot)[['keyId']]
  obj<-Exp1_R25_prot[[2]]
  
  r <- reactiveValues(
    res = NULL
  )
  
  rowData(obj) <- cbind(rowData(obj), ProtOfInterest=sample(c(0,1), nrow(obj), TRUE))
  
  r$res <- callModule(mod_plots_tracking_server,'plots_tracking', 
                      obj = reactive({obj}), 
                      params=reactive({NULL}),
                      keyId=reactive({keyId}),
                      reset=reactive({FALSE}) )
  
  
  output$show_res <- renderText({HTML(r$res())})
  output$show_res_type <- renderText({HTML(r$res()$type)})
  output$show_res_list <- renderText({HTML(r$res()$list)})
  output$show_res_rand <- renderText({HTML(r$res()$rand)})
  output$show_res_col <- renderText({HTML(r$res()$col)})
  output$show_res_list_indices <- renderText({HTML(r$res()$list_indices)})
  output$show_res_list_rand <- renderText({HTML(r$res()$list_rand)})
  output$show_res_list_col <- renderText({HTML(r$res()$list_col)})
}


shinyApp(ui, server)
