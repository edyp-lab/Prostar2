btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"

VALIDATED <- 1
UNDONE <- 0
SKIPPED <- -1
RESETED <- 2
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
  
  tagList(
    uiOutput(ns('load_css_style')),
    uiOutput(ns("timelineStyle"))
    )
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
mod_timeline_server <- function(id, style=2, config, pos){
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$timelineStyle <- renderUI({ 
      uiOutput(ns(paste0('timeline', style))) })
    
    
    output$load_css_style <- renderUI({
            req(style != 3)

      file <- paste0('./Timelines/timeline',style, '.sass')
      #code <- code_sass_timeline[[paste0('style',style)]],"\n")
      code <- strsplit(readLines(file),"\n")
      firstLine <- code[[1]][1]
      prefix <- substr(firstLine, 1, unlist(gregexpr(pattern =':',firstLine)))
      suffix <- substr(firstLine, unlist(gregexpr(pattern =';',firstLine)), nchar(firstLine))
      
      code[[1]][1] <- paste0(prefix, length(config$steps), suffix, collapse='')
      
      shinyjs::inlineCSS( sass::sass(paste(unlist(code), collapse = '')))
   
    })

    
    output$timeline2 <- renderUI({
     
      status.clone <- rep('', length(config$steps))
  
      if( !is.null(config$steps))
        status.clone[which(unlist(config$steps))] <- 'mandatory'
      
      status.clone[which(unlist(config$status) == VALIDATED)] <- 'complete'
      status.clone[which(unlist(config$status) == SKIPPED)] <- 'skipped'
      
      active  <- rep('', length(config$steps))
       active[pos] <- 'active'
      
       txt <- "<ul class='timeline' id='timeline'>"
      for (i in 1:length(config$steps)){
        txt <- paste0(txt, "<li class='li ",status.clone[i]," ",active[i],"'><div class='timestamp'></div><div class='status'><h4>", names(config$steps)[i],"</h4></div></li>")
      }
      txt <- paste0(txt,"</ul>")
      
      HTML(txt)
    })

  })
}
