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
#' @importFrom shinyjs inlineCSS useShinyjs
mod_popover_for_help_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(pop_css),
    div(
      div(
        # edit1
        style="display:inline-block; vertical-align: middle; padding-bottom: 5px;",
        uiOutput(ns("write_title_ui"))
      ),
      div(style="display:inline-block; vertical-align: middle;padding-bottom: 5px;",
          uiOutput(ns("dot")),
          uiOutput(ns("show_Pop"))
      )
    )
  )
}

# Module Server

#' @rdname mod_popover_for_help
#' 
#' @export
#' 
#' @keywords internal
#' 
#' @importFrom shinyBS bsPopover addPopover bsTooltip
#' 
mod_popover_for_help_server <- function(id, data){
  
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$write_title_ui <- renderUI({
      req(data)
      data$title
    })
    
    output$dot <- renderUI({
      tags$button(tags$sup("[?]"), class="Prostar_tooltip")
    })
    
    output$show_Pop <- renderUI({
      shinyBS::bsTooltip(ns("dot"), data$content, trigger = "hover")
      
    })
    
    
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

