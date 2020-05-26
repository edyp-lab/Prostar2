# Code issu de https://github.com/ebailey78/shinyBS/issues/57
# A servi de base pour test-modal_dragg-resizable.R

library(shinyjqui)
library(shiny)
library(shinyBS)



ui <- basicPage(
  tagList(
    tags$head(tags$style(".modal-dialog { width:75% }")),
    
    htmlOutput("button_ui"),
    jqui_draggable(
      shinyBS::bsModal("modal", "foo", trigger = "button_ui", "bar")
      
    )
  )
)

server = function(input, output, session) {
  
  jqui_resizable("#modal .modal-content")
  
  output$button_ui <- renderUI({
    actionButton("button", "Show modal")
  })
  
}

runApp(list(ui = ui, server = server))
