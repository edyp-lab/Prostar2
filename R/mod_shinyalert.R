#' shinyalert UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_shinyalert_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    useShinyalert()
  )
}
    
#' shinyalert Server Function
#'
#' @noRd 
mod_shinyalert_server <- function(input, output, session){
  ns <- session$ns
 
  rv.shinyalert <- reactiveValues(
    value = NULL
  )
  
  
  observeEvent(input$shinyalert,{
    rv.shinyalert$value <- input$shinyalert
  })
  
  
  shinyalert(
    title = "Hello",
    text = "This is a modal",
    size = "xs", 
    closeOnEsc = TRUE,
    closeOnClickOutside = FALSE,
    html = FALSE,
    type = "info",
    showConfirmButton = TRUE,
    showCancelButton = TRUE,
    confirmButtonText = "OK",
    confirmButtonCol = "#15A4E6",
    cancelButtonText = "Cancel",
    timer = 0,
    imageUrl = "",
    animation = TRUE
  )
  
  return(reactive({rv.shinyalert$value}))
}
    
## To be copied in the UI
# mod_shinyalert_ui("shinyalert_ui_1")
    
## To be copied in the server
# callModule(mod_shinyalert_server, "shinyalert_ui_1")
 
