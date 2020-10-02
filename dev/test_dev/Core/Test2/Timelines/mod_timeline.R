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
#' @param isDone xxxxx
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
    shinyjs::useShinyjs(),
    uiOutput(ns("load_css_style")),

    fluidRow(
      align= 'center',
      column(width=2,div(style=btn_style,
                         actionButton(ns("rstBtn"), "Reset ",
                                      class = redBtnClass,
                                      style='padding:4px; font-size:80%'),
                         shinyjs::disabled(actionButton(ns("prevBtn"), "<<",
                                                        class = PrevNextBtnClass,
                                                        style='padding:4px; font-size:80%'))
      )),
      column(width=8,div( style = btn_style, uiOutput(ns("timelineStyle"))) ),
      column(width=2,div(style=btn_style,
                         actionButton(ns("nextBtn"), ">>",
                                      class = PrevNextBtnClass,
                                      style='padding:4px; font-size:80%'))
             )
      ),
    uiOutput(ns('show_screens'))
    )
}

# Module Server

#' @rdname mod_navigation
#' 
#' @param style xxx
#' 
#' @param config xxxx
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
mod_timeline_server <- function(id, style=1, config, actions, position){

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$timelineStyle <- renderUI({ uiOutput(ns(paste0('timeline', style))) })
    
    # Disable screens marks as F in the vector actions$screens
    observeEvent(actions$screens, ignoreInit = T,{
      lapply(1:current$nbSteps, 
             function(x){ shinyjs::toggleState(paste0('screen', x), cond = actions$screens[[x]])})
      print(paste0(unlist(actions$screens), collapse=' '))
      })
    
    # observeEvent(position,{ 
    #   print(paste0('in TL, new position : ', position))
    #   current$val <- position
    # })
    
    
    current <- reactiveValues(
      val = 1,
      nbSteps = NULL
    )
    
    observeEvent(req(config),{
      current$nbSteps <- length(config$stepsNames)
      current$val <- 1
      InitScreens()
    })
    
    
    
    InitScreens <- reactive({
      # initialisation of the screens
      for (i in 1:current$nbSteps)
        config$screens[[i]] <- if (i == current$val)
          div(id = ns(paste0("screen", i)),  config$screens[[i]])
      else
        shinyjs::hidden(div(id = ns(paste0("screen", i)),  config$screens[[i]]))
    })
    

    
    navPage <- function(direction) {
      newval <- current$val + direction 
      newval <- max(1, newval)
      newval <- min(newval, current$nbSteps)
      current$val <- newval
    }
    
    
    observeEvent(input$prevBtn, ignoreInit = TRUE, {navPage(-1)})
    observeEvent(input$nextBtn, ignoreInit = TRUE, {navPage(1)})
    
  
    output$show_screens <- renderUI({tagList(config$screens)})
    
   
    
    observeEvent(current$val, {
      shinyjs::toggleState('prevBtn', cond = (current$val > 1) && actions$btns$prv)
      shinyjs::toggleState('nextBtn', cond = (current$val < current$nbSteps) &&  actions$btns$nxt)
      
      # Display current page
      lapply(1:current$nbSteps, function(x){
        shinyjs::toggle(paste0('screen', x), condition = x==current$val)})
    })
    
    
    
    ## Functions for timeline and styles
    
    output$load_css_style <- renderUI({
      req(length(config$stepsNames))
      req(style != 3)
      

        file <- paste0('./Timelines/timeline',style, '.sass')
      #code <- code_sass_timeline[[paste0('style',style)]],"\n")
      code <- strsplit(readLines(file),"\n")
      firstLine <- code[[1]][1]
      prefix <- substr(firstLine, 1, unlist(gregexpr(pattern =':',firstLine)))
      suffix <- substr(firstLine, unlist(gregexpr(pattern =';',firstLine)), nchar(firstLine))
      
      code[[1]][1] <- paste0(prefix, current$nbSteps, suffix, collapse='')
      
      shinyjs::inlineCSS( sass::sass(paste(unlist(code), collapse = '')))

      
      
    })
    

    
    #### -----
    ### Definition of timelines style
    output$timeline1 <- renderUI({
      config
      status <- rep('',length(config$stepsNames))
      status[current$val] <- ' active'
      steps <- config$stepsNames
      txt <- "<div class='flex-parent'> <div class='input-flex-container'>"
      for (i in 1:length(config$stepsNames)){
        txt <- paste0(txt, "<div class='input",status[i], "'><span name='", steps[i],"'></span>  </div>")
      }
      txt <- paste0(txt,"</div></div>")
      HTML(txt)
    })
    
    
    output$timeline2 <- renderUI({
      config
      status <- rep('', length(config$stepsNames))
      if( !is.null(config$mandatory))
        status[which(config$mandatory)] <- 'mandatory'
      
      #status <- rep('',length(config$stepsNames))
      status[which(config$isDone)] <- 'complete'
      
      active  <- rep('', length(config$stepsNames))
      active[current$val] <- 'active'
      
      steps <- config$stepsNames
      txt <- "<ul class='timeline' id='timeline'>"
      for (i in 1:length(config$stepsNames)){
        txt <- paste0(txt, "<li class='li ",status[i]," ",active[i],"'><div class='timestamp'></div><div class='status'><h4>", steps[i],"</h4></div></li>")
      }
      txt <- paste0(txt,"</ul>")
      
      HTML(txt)
    })
    
    
    output$timeline3 <- renderUI({
      config
      
      color <- rep("lightgrey", length(config$stepsNames))
      colorForCursor <- rep("white", length(config$stepsNames))
      
      for (i in 1:length(config$stepsNames)){
        status <- config$isDone[i]
        col <- ifelse(!is.null(config$mandatory) && config$mandatory[i], "red", orangeProstar)
        ifelse(status, color[i] <- "green", color[i] <- col)
      }
      
      colorForCursor[current$val] <- "black"
      
      steps <- config$stepsNames
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
    
    
    
    # return value of the tl.update
    list(rstBtn = reactive(input$rstBtn),
         position = reactive(current$val)
         )
    
  })
}



