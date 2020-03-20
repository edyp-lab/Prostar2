
#library(highcharter)
library(shinyjqui)
library(shiny)
library(shinyBS)
#library(DAPAR)


#source(file.path("..", "mod_plots_corr_matrix.R"), local = TRUE)$value

ui <- basicPage(
  htmlOutput("button_ui"),
  jqui_draggable(
    shinyBS::bsModal("modal", "foo", trigger = "button_ui", "bar")
    #shinyBS::bsModal("modal", "foo", trigger = "button_ui", uiOutput("plotcorrMatrixlarge"))
    )
)

server = function(input, output, session) {

  jqui_resizable("#modal .modal-content")
  
  output$button_ui <- renderUI({
    actionButton("button", "Show modal")
  })
  
  #########################
  # require(DAPARdata)
  # data('Exp1_R25_prot')
  # callModule(mod_plots_corr_matrix_server, "corrMatrixPlot_AbsPanel", obj = Exp1_R25_prot)
  # 
  # output$plotcorrMatrixlarge <- renderUI({
  #   mod_plots_corr_matrix_ui("corrMatrixPlot_AbsPanel")
  # })
  #########################
}

runApp(list(ui = ui, server = server))
