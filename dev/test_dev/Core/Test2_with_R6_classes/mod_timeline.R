btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"


# Module UI

#' @title   mod_navigation_ui and mod_navigation_server
#' @description  A shiny Module. The sass source code for timeline was inspired by 
#'  : https://codepen.io/cjl750/pen/mXbMyo
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' @param status xxxxx
#' @param screens xxxxx
#' @param rstFunc xxxxx
#' @param iconType xxxxxx
#'
#' @rdname mod_navigation
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList
#' @importFrom shinyjs disabled inlineCSS
mod_timeline_ui <- function(id){
  ns <- NS(id)
  
 
}

# Module Server

#' @rdname mod_navigation
#' 
#' @param id the id to connect the ui and server parts of the module
#' 
#' @param style An integer which codes for the style of timeline
#' 
#' @param config A list of xx elements to configure and update the timeline:
#'   * type: 
#'   * process.name:
#'   * position An integer which specify the position to which to go. This is
#'   used by the caller to force the 
#' 
#' @param  btns xxx
#' 
#' @export
#' 
#' @keywords internal
#' 
#' @import shiny shinyjs
#' 
#' @importFrom sass sass
#' 
mod_timeline_server <- function(id, style=2, config, wake = NULL){
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
   
    
    output$timeline3 <- renderUI({
      config
      
      color <- rep("lightgrey", current$nbSteps)
      colorForCursor <- rep("white", current$nbSteps)
      
      for (i in 1:length(config$stepsNames)){
        status <- config$status[[i]]
        col <- ifelse(!is.null(config$steps) && config$steps[[i]], "red", orangeProstar)
        ifelse(status==VALIDATED, color[i] <- "green", color[i] <- col)
      }
      
      colorForCursor[current$val] <- "black"
      
      steps <- config$steps
      colorCurrentPos <- colorForCursor
      paste0("     ", steps, "     ")
      rows.color <- rows.text <-  rows.cursor <- list()
      rows.text <- list()
      for( i in 1:length( color ) ) {
        rows.color[[i]] <-lapply( color[i], function( x ) tags$th(  style=paste0("background-color:", x,"; height: 20px;" ) ))
        rows.cursor[[i]] <-lapply( colorCurrentPos[i], function( x ) tags$th(  style=paste0("background-color:", x,"; height: 5px;" ) ))
        rows.text[[i]] <- lapply( steps[i], function( x ) tags$td( x ) ) 
      }
      
      html.table <-  tags$table(style = "width: 100%; text-align: center;border: 1;border-collapse: separate;border-spacing: 10px;padding-top: 0px;",
                                tags$tr( rows.color ),
                                tags$tr( rows.cursor ),
                                tags$tr( rows.text )
      )
      
      html.table
    })
    

    
    list(rstBtn = reactive(current$reset_OK),
         prvBtn = reactive(input$prevBtn),
         nxtBtn = reactive(input$nextBtn),
         saveBtn = reactive({input$saveExitBtn}),
         pos = reactive({current$val})
    )
    
  })
}
