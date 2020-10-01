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
#' @param pages xxxx
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
mod_timeline_server <- function(id, style=1, pages, actions){

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$timelineStyle <- renderUI({ uiOutput(ns(paste0('timeline', style))) })
    
    observeEvent(actions,{
      shinyjs::toggleState('nextBtn', condition=actions$nxt)
      shinyjs::toggleState('rstBtn', condition=actions$rst)
      shinyjs::toggleState('prevBtn', condition=actions$prv)
   })
    
    
    current <- reactiveValues(
      # This variable is the indice of the current screen
      val = NULL,
      nbSteps = NULL
    )
    
    observeEvent(req(pages),{
      current$nbSteps <- length(pages$stepsNames)
      current$val <- 1

      pages$screens[[1]] <- div(id = ns(paste0("screen", 1)),  pages$screens[[1]])
      for (i in 2:current$nbSteps){
        pages$screens[[i]] <- shinyjs::hidden(div(id = ns(paste0("screen", i)),  pages$screens[[i]]))
      }
      
    })
    
    
    navPage <- function(direction) {
      newval <- current$val + direction 
      newval <- max(1, newval)
      newval <- min(newval, current$nbSteps)
      current$val <- newval
    }
    
    
    observeEvent(input$prevBtn, ignoreInit = TRUE, {navPage(-1)})
    observeEvent(input$nextBtn, ignoreInit = TRUE, {navPage(1)})
    
  
    output$show_screens <- renderUI({tagList(pages$screens)})
    
    observeEvent(current$val, {
      lapply(1:current$nbSteps, function(x){shinyjs::toggle(paste0('screen', x), 
                                                            condition = x==current$val)})
    })
    
    
    
    ## Functions for timeline and styles
    
    output$load_css_style <- renderUI({
      req(length(pages$stepsNames))
      req(style != 3)
      

        file <- paste0('./Timelines/timeline',style, '.sass')
      #code <- code_sass_timeline[[paste0('style',style)]],"\n")
      code <- strsplit(readLines(file),"\n")
      firstLine <- code[[1]][1]
      prefix <- substr(firstLine, 1, unlist(gregexpr(pattern =':',firstLine)))
      suffix <- substr(firstLine, unlist(gregexpr(pattern =';',firstLine)), nchar(firstLine))
      
      code[[1]][1] <- paste0(prefix, length(pages$stepsNames), suffix, collapse='')
      
      shinyjs::inlineCSS( sass::sass(paste(unlist(code), collapse = '')))

      
      
    })
    

    
    output$timeline9 <- renderUI({
      txt <- "<div class='inliner'></div>
        <div class='inlined'>
          
          <!-- Start component -->
          <div class='progress-meter'>
            <div class='track'>
              <span class='progress'></span>
                </div>
                <ol class='progress-points' data-current='4'>
                  <li class='progress-point'>
                    <span class='label'>Lorem ipsum</span>
                      </li>
                      <li class='progress-point'>
                        <span class='label'>Aliquam tincidunt</span>
                          </li>
                          <li class='progress-point'>
                            <span class='label'>Vestibulum auctor</span>
                              </li>
                              <li class='progress-point'>
                                <span class='label'>Lorem ipsum</span>
                                  </li>
                                  <li class='progress-point'>
                                    <span class='label'>Aliquam tincidunt</span>
                                      </li>
                                      </ol>
                                      </div>
                                      <!-- End component -->
                                      
                                      <!-- Demo only -->
                                      <div class='controls'>
                                        <button class='trigger'>Toggle progress</button>
                                          <p>Click any point to navigate to it directly</p>
                                          </div>
                                          </div>
                                          "
                                          
                                          HTML(txt)
    })

    
    
    
    #### -----
    ### Definition of timelines style
    output$timeline1 <- renderUI({
      pages
      status <- rep('',length(pages$stepsNames))
      status[current$val] <- ' active'
      steps <- pages$stepsNames
      txt <- "<div class='flex-parent'> <div class='input-flex-container'>"
      for (i in 1:length(pages$stepsNames)){
        txt <- paste0(txt, "<div class='input",status[i], "'><span name='", steps[i],"'></span>  </div>")
      }
      txt <- paste0(txt,"</div></div>")
      HTML(txt)
    })
    
    
    output$timeline2 <- renderUI({
      pages
      status <- rep('', length(pages$stepsNames))
      if( !is.null(pages$mandatory))
        status[which(pages$mandatory)] <- 'mandatory'
      
      #status <- rep('',length(pages$stepsNames))
      status[which(pages$isDone)] <- 'complete'
      
      active  <- rep('', length(pages$stepsNames))
      active[current$val] <- 'active'
      
      steps <- pages$stepsNames
      txt <- "<ul class='timeline' id='timeline'>"
      for (i in 1:length(pages$stepsNames)){
        txt <- paste0(txt, "<li class='li ",status[i]," ",active[i],"'><div class='timestamp'></div><div class='status'><h4>", steps[i],"</h4></div></li>")
      }
      txt <- paste0(txt,"</ul>")
      
      HTML(txt)
    })
    
    
    output$timeline3 <- renderUI({
      pages
      
      color <- rep("lightgrey", length(pages$stepsNames))
      colorForCursor <- rep("white", length(pages$stepsNames))
      
      for (i in 1:length(pages$stepsNames)){
        status <- pages$isDone[i]
        col <- ifelse(!is.null(pages$mandatory) && pages$mandatory[i], "red", orangeProstar)
        ifelse(status, color[i] <- "green", color[i] <- col)
      }
      
      colorForCursor[current$val] <- "black"
      
      steps <- pages$stepsNames
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



