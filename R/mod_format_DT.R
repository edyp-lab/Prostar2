# Module UI
  
#' @title   mod_format_DT_ui and mod_format_DT_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_format_DT
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' @importFrom DT dataTableOutput
mod_format_DT_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(
      div( style="display:inline-block; vertical-align: middle; align: center;",
           DT::dataTableOutput(ns("dt"))
      )
    )
  )
}
    
# Module Server
    
#' @rdname mod_format_DT
#' @export
#' @keywords internal
#' @importFrom DT dataTableProxy renderDT replaceData datatable
#' @importFrom htmlwidgets JS    
mod_format_DT_server <- function(input, output, session,
                                 table2show,
                                 withBtns=NULL,
                                 showRownames=FALSE,
                                 dom='Bt'){
  ns <- session$ns
  
  observe({

    if (is.null(table2show())){
      warning("The parameter table2show is null.")
      return(NULL)
    }
  })
  
  proxy = DT::dataTableProxy(session$ns('dt'), session)
  
  observe({DT::replaceData(proxy, table2show(), resetPaging = FALSE)  })
  
  output$dt <- DT::renderDT({
    req(table2show())
    if (length(table2show())==0){return(NULL)}
    isolate({
      DT::datatable(table2show(), 
                    extensions = c('Scroller', 'Buttons'),
                    escape = FALSE,
                    rownames= showRownames,
                    option=list(initComplete = initComplete(),
                                dom = dom,
                                server = FALSE,
                                autoWidth=TRUE,
                                columnDefs = list(list(width='150px',targets= "_all")),
                                ordering = FALSE
                    )
      )
    })
    
  })
  
  initComplete <- function(){
    
    return (htmlwidgets::JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': 'darkgrey', 'color': 'black'});",
      "}"))
  }
}
    
## To be copied in the UI
# mod_format_DT_ui("format_DT_ui_1")
    
## To be copied in the server
# callModule(mod_format_DT_server, "format_DT_ui_1")
 
