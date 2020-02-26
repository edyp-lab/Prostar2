# Module UI
  
#' @title   mod_popover_for_help_ui and mod_popover_for_help_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' @param data A list of two items:
#' * title: xxxx
#' * content: xxxx
#'
#' @rdname mod_popover_for_help
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' @importFrom shinyjs inlineCSS
mod_popover_for_help_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::inlineCSS(pop_css),
    uiOutput(ns("customPopover"))
  )
}
    
# Module Server
    
#' @rdname mod_popover_for_help
#' @export
#' @keywords internal
#' @importFrom shinyBS bsPopover
    
mod_popover_for_help_server <- function(input, output, session, data){
  ns <- session$ns
  
  output$customPopover <- renderUI({
    req(data())
    
    div(
      div(
        # edit1
        style="display:inline-block; vertical-align: middle; padding-bottom: 5px;",
        data()$title
      ),
      div(
        # edit2
        style="display:inline-block; vertical-align: middle;padding-bottom: 5px;",
        if (!is.null(data()$color) && ('white' == data()$color)) {
          tags$button(id=ns("q1"), tags$sup("[?]"), class="Prostar_tooltip_white")
        } else {
          tags$button(id=ns("q1"), tags$sup("[?]"), class="Prostar_tooltip")
        },
        shinyBS::bsPopover(id = ns("q1"), 
                           title = "",
                           content = data()$content,
                           placement = "right", 
                           trigger = "hover", 
                           options = list(container = "body")
        )
      )
    )
    
    
  })
}


pop_css <- "button.Prostar_tooltip {
    background:none;
    color: #2EA8B1;
    border:none;
    padding-left:1ch;
    font: inherit;
    /*border is optional*/
        /*border-bottom:1px solid #444;*/
    cursor: pointer;
    font-weight: bold;
    display: inline-block;
    padding:0;
}

button.Prostar_tooltip_white {
    background:none;
    color: white;
    border:none;
    padding-left:1ch;
    font: inherit;
    /*border is optional*/
        /*border-bottom:1px solid #444;*/
    cursor: pointer;
    font-weight: bold;
    display: inline-block;
    padding:0;
}

.input-color {
    position: relative;
}
.input-color input {
    padding-left: 15px;
    border: 0px;
    background: transparent;
}
.input-color .color-box {
    width: 15px;
    height: 15px;
    display: inline-block;
    background-color: #ccc;
    position: absolute;
    left: 5px;
    top: 5px;
    
}"

    
## To be copied in the UI
# mod_popover_for_help_ui("popover_for_help_ui_1")
    
## To be copied in the server
# callModule(mod_popover_for_help_server, "popover_for_help_ui_1")
 
