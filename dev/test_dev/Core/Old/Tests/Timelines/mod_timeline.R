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
      column(width=2,uiOutput(ns('show_leftBtns'))),
      column(width=8,div( style = btn_style, uiOutput(ns("timelineStyle"))) ),
      column(width=2,uiOutput(ns('show_rightBtns')))
      )
    )
}

# Module Server

#' @rdname mod_navigation
#' 
#' @param style xxx
#' 
#' @param process_config xxxx
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
mod_timeline_server <- function(id, style=1, process_config, tl.update, showSkip = FALSE){

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$timelineStyle <- renderUI({ uiOutput(ns(paste0('timeline', style))) })
    
    observeEvent(tl.update$actions,{
      shinyjs::toggleState('nextBtn', condition=tl.update$actions$nxt)
      shinyjs::toggleState('rstBtn', condition=tl.update$actions$rst)
      shinyjs::toggleState('prevBtn', condition=tl.update$actions$prv)
      shinyjs::toggleState('skipBtn', condition=tl.update$actions$skip)
   })
    
    
    
    output$show_leftBtns <- renderUI({
      tagList(
        div(style=btn_style,
            actionButton(ns("rstBtn"), paste0("Reset ", process_config$type),
                   class = redBtnClass,
                   style='padding:4px; font-size:80%'),
            shinyjs::disabled(actionButton(ns("prevBtn"), "<<",
                                           class = PrevNextBtnClass,
                                           style='padding:4px; font-size:80%'))
        )
      )
    })
    
    output$show_rightBtns <- renderUI({
      tagList(
        div(style=btn_style,
            actionButton(ns("nextBtn"), ">>",
                         class = PrevNextBtnClass,
                         style='padding:4px; font-size:80%')),
            if(showSkip)
              actionButton(ns("skipBtn"), "Skip process",
                 class = redBtnClass,
                 style='padding:4px; font-size:80%')
      )
    })
    
    ## Functions for timeline and styles
    
    output$load_css_style <- renderUI({
      req(length(process_config$stepsNames))
      req(style != 3)
      

        file <- paste0('./Timelines/timeline',style, '.sass')
      #code <- code_sass_timeline[[paste0('style',style)]],"\n")
      code <- strsplit(readLines(file),"\n")
      firstLine <- code[[1]][1]
      prefix <- substr(firstLine, 1, unlist(gregexpr(pattern =':',firstLine)))
      suffix <- substr(firstLine, unlist(gregexpr(pattern =';',firstLine)), nchar(firstLine))
      
      code[[1]][1] <- paste0(prefix, length(process_config$stepsNames), suffix, collapse='')
      
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
      process_config
      status <- rep('',length(process_config$stepsNames))
      status[tl.update$current.pos] <- ' active'
      steps <- process_config$stepsNames
      txt <- "<div class='flex-parent'> <div class='input-flex-container'>"
      for (i in 1:length(process_config$stepsNames)){
        txt <- paste0(txt, "<div class='input",status[i], "'><span name='", steps[i],"'></span>  </div>")
      }
      txt <- paste0(txt,"</div></div>")
      HTML(txt)
    })
    
    
    output$timeline2 <- renderUI({
      process_config
      status <- rep('', length(process_config$stepsNames))
      if( !is.null(process_config$mandatory))
        status[which(process_config$mandatory)] <- 'mandatory'
      
      #status <- rep('',length(process_config$stepsNames))
      status[which(process_config$isDone)] <- 'complete'
      
      active  <- rep('', length(process_config$stepsNames))
      active[tl.update$current.pos] <- 'active'
      
      steps <- process_config$stepsNames
      txt <- "<ul class='timeline' id='timeline'>"
      for (i in 1:length(process_config$stepsNames)){
        txt <- paste0(txt, "<li class='li ",status[i]," ",active[i],"'><div class='timestamp'></div><div class='status'><h4>", steps[i],"</h4></div></li>")
      }
      txt <- paste0(txt,"</ul>")
      
      HTML(txt)
    })
    
    
    output$timeline3 <- renderUI({
      process_config
      
      color <- rep("lightgrey", length(process_config$stepsNames))
      colorForCursor <- rep("white", length(process_config$stepsNames))
      
      for (i in 1:length(process_config$stepsNames)){
        status <- process_config$isDone[i]
        col <- ifelse(!is.null(process_config$mandatory) && process_config$mandatory[i], "red", orangeProstar)
        ifelse(status, color[i] <- "green", color[i] <- col)
      }
      
      colorForCursor[tl.update$current.pos] <- "black"
      
      steps <- process_config$stepsNames
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
    
    
    output$timeline4 <- renderUI({
      # Source: https://codepen.io/chrisgannon/pen/djjZBq
      
    tl <- "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 800 600'>
<defs>
      <filter id='blur' x='-100%' y='-100%' width='250%' height='250%'>
        <feGaussianBlur stdDeviation='2' result='coloredBlur' />
        <feOffset dx='2' dy='5' result='offsetblur'></feOffset>
       <feFlood id='glowAlpha' flood-color='#FFFcf9' flood-opacity='1'></feFlood>
    <feComponentTransfer>
      <feFuncA type='linear' slope='0.2'/>
      </feComponentTransfer>
      <feComposite in2='offsetblur' operator='in'></feComposite>
      <feMerge>
      <feMergeNode/>          
      <feMergeNode in='SourceGraphic'></feMergeNode>
      </feMerge>
      </filter>   
      <g id='preservationText'>
      <path d='M4.4,8.54H1.58v4.73H0V.18H4.37A5,5,0,0,1,6.88.73,3.35,3.35,0,0,1,8.31,2.21a4.67,4.67,0,0,1,.46,2.11,4.76,4.76,0,0,1-.46,2.13A3.44,3.44,0,0,1,6.88,8,4.82,4.82,0,0,1,4.4,8.54ZM1.58,1.59V7.13H4.4a3.27,3.27,0,0,0,1.66-.36,2.06,2.06,0,0,0,.88-1,3.63,3.63,0,0,0,.27-1.44,3.05,3.05,0,0,0-.62-2,2.74,2.74,0,0,0-2.22-.76Z' fill='#2597bf'/>
      <path d='M19.69,13.27l-2.6-5.09h-3.2v5.09H12.3V.18h4.38A5,5,0,0,1,19.2.73a3.38,3.38,0,0,1,1.42,1.46,4.48,4.48,0,0,1,.45,2,4,4,0,0,1-.61,2.27,3.72,3.72,0,0,1-1.75,1.37l2.82,5.44ZM13.89,1.59V6.72H16.5a4.29,4.29,0,0,0,1.81-.32,2,2,0,0,0,.93-.91,3,3,0,0,0,.27-1.3A3.4,3.4,0,0,0,19.29,3a2.06,2.06,0,0,0-.84-1,3.32,3.32,0,0,0-1.77-.39Z' fill='#2597bf'/>
      <path d='M32.61,11.86v1.41h-8V.18h7.9V1.59H26.19V6h5.65V7.42H26.19v4.44Z' fill='#2597bf'/>
      <path d='M43.38,3.55a2.36,2.36,0,0,0-1-1.6,3.44,3.44,0,0,0-2-.54,3.28,3.28,0,0,0-2.1.62,1.91,1.91,0,0,0-.79,1.58,1.6,1.6,0,0,0,.61,1.29,5.09,5.09,0,0,0,1.92.85l1.33.36a6.45,6.45,0,0,1,2.77,1.36,2.9,2.9,0,0,1,.91,2.22,3.51,3.51,0,0,1-.55,1.94A3.9,3.9,0,0,1,42.93,13a6,6,0,0,1-2.57.5,5.18,5.18,0,0,1-3.35-1,3.51,3.51,0,0,1-1.35-2.6h1.63a2.07,2.07,0,0,0,.5,1.22,2.66,2.66,0,0,0,1.12.71,4.28,4.28,0,0,0,1.45.24,4.19,4.19,0,0,0,1.63-.3,2.83,2.83,0,0,0,1.15-.83,2,2,0,0,0,.42-1.25,1.56,1.56,0,0,0-.64-1.34,6.81,6.81,0,0,0-2-.83l-1.61-.46A6.19,6.19,0,0,1,36.91,5.8,2.76,2.76,0,0,1,36,3.66a3.14,3.14,0,0,1,.6-1.92A3.94,3.94,0,0,1,38.22.46,5.39,5.39,0,0,1,40.49,0a5.15,5.15,0,0,1,2.24.46,3.8,3.8,0,0,1,1.55,1.26,3.41,3.41,0,0,1,.63,1.83Z' fill='#2597bf'/>
      <path d='M56.6,11.86v1.41h-8V.18h7.9V1.59H50.18V6h5.65V7.42H50.18v4.44Z' fill='#2597bf'/>
      <path d='M67.65,13.27l-2.6-5.09h-3.2v5.09H60.26V.18h4.38a5,5,0,0,1,2.52.55,3.38,3.38,0,0,1,1.42,1.46,4.48,4.48,0,0,1,.45,2,4,4,0,0,1-.61,2.27,3.72,3.72,0,0,1-1.75,1.37l2.82,5.44ZM61.85,1.59V6.72h2.61a4.29,4.29,0,0,0,1.81-.32,2,2,0,0,0,.93-.91,3,3,0,0,0,.27-1.3A3.4,3.4,0,0,0,67.25,3a2.06,2.06,0,0,0-.84-1,3.32,3.32,0,0,0-1.77-.39Z' fill='#2597bf'/>
      <path d='M72.83.18l3.89,11h.16l3.88-11h1.66l-4.8,13.09H76L71.17.18Z' fill='#2597bf'/>
      <path d='M84.77,13.27H83.11L87.91.18h1.64l4.81,13.09H92.69l-1.23-3.5H86Zm3.88-11L86.5,8.36H91L88.81,2.25Z' fill='#2597bf'/>
      <path d='M104.91.18V1.59H100.8V13.27H99.21V1.59H95.1V.18Z' fill='#2597bf'/>
      <path d='M109.85.18V13.27h-1.58V.18Z' fill='#2597bf'/>
      <path d='M125,6.72a7.91,7.91,0,0,1-.75,3.58,5.56,5.56,0,0,1-2,2.33,5.82,5.82,0,0,1-6,0,5.56,5.56,0,0,1-2-2.33,7.91,7.91,0,0,1-.75-3.58,7.9,7.9,0,0,1,.75-3.57,5.56,5.56,0,0,1,2-2.33,5.82,5.82,0,0,1,6,0,5.56,5.56,0,0,1,2,2.33A7.9,7.9,0,0,1,125,6.72Zm-1.53,0a6.53,6.53,0,0,0-.57-2.87,4.18,4.18,0,0,0-1.53-1.77,4.15,4.15,0,0,0-4.3,0,4.18,4.18,0,0,0-1.53,1.77A6.67,6.67,0,0,0,115,6.72a6.64,6.64,0,0,0,.56,2.87A4.27,4.27,0,0,0,117,11.37a4.15,4.15,0,0,0,4.3,0,4.27,4.27,0,0,0,1.53-1.78A6.5,6.5,0,0,0,123.44,6.72Z' fill='#2597bf'/>
      <path d='M138.65.18V13.27h-1.53L130.24,3h-.13V13.27h-1.58V.18h1.53L137,10.48h.12V.18Z' fill='#2597bf'/>
      </g>
      </defs>
      
      <line id='trackBg' class='track' x1='188' y1='312.5' x2='613' y2='312.5' fill='none'  stroke-miterlimit='10' stroke='#E6E6E8' opacity='0.1'/>
      <line id='track' class='track' x1='188' y1='312.5' x2='613' y2='312.5' fill='none'  stroke-miterlimit='10' stroke='#E6E6E8'/>
      <g id='boxGroup' stroke-miterlimit='10' stroke='#E6E6E8'>
      <rect x='167.5' y='269' width='26' height='26' fill='#283D67' />
      <rect x='207' y='269' width='26' height='26' fill='#5184E5' />
      <rect x='247' y='269' width='26' height='26' fill='#5184E5' />
      <rect x='287' y='269' width='26' height='26' fill='#5184E5' />
      <rect x='327' y='269' width='26' height='26' fill='#2dcbe0' />
      <rect x='367' y='269' width='26' height='26' fill='#2dcbe0' />
      <rect x='407' y='269' width='26' height='26' fill='#2dcbe0' />
      <rect x='447' y='269' width='26' height='26' fill='#2dcbe0' />
      <rect x='487' y='269' width='26' height='26' fill='#2dcbe0' />
      <rect x='527' y='269' width='26' height='26' fill='#FBCE47' />
      <rect x='567' y='269' width='26' height='26' fill='#FBCE47' />
      <rect x='607' y='269' width='26' height='26' fill='#F5566D' />
      </g>
      <g id='dividerGroup' fill='none'  stroke-miterlimit='10' stroke-width='1' stroke='#E6E6E8'>
      <line x1='179.5' y1='299' x2='179.5' y2='304'/>
      <line x1='219.5' y1='299' x2='219.5' y2='304'/>
      <line x1='259.5' y1='299' x2='259.5' y2='304'/>
      <line x1='299.5' y1='299' x2='299.5' y2='304'/>
      <line x1='339.5' y1='299' x2='339.5' y2='304'/>
      <line x1='379.5' y1='299' x2='379.5' y2='304'/>
      <line x1='419.5' y1='299' x2='419.5' y2='304'/>
      <line x1='459.5' y1='299' x2='459.5' y2='304'/>
      <line x1='499.5' y1='299' x2='499.5' y2='304'/>
      <line x1='539.5' y1='299' x2='539.5' y2='304'/>
      <line x1='579.5' y1='299' x2='579.5' y2='304'/>
      <line x1='619.5' y1='299' x2='619.5' y2='304'/>
      </g>
      <rect id='lowEndBox' class='endBox' x='177' y='309' width='6' height='6' fill='#283D67'/>
      <rect id='highEndBox' class='endBox' x='618' y='309' width='6' height='6' fill='#F5566D'/>
      <g id='numberGroupBg' fill='#f2f2f2' opacity='0.1'>
      <path d='M180.39,322V332.9h-1.57v-8.61h-.09a1.46,1.46,0,0,1-.73.51,3.88,3.88,0,0,1-1.52.26v-1.38a2.35,2.35,0,0,0,1.87-.8,3.35,3.35,0,0,0,.41-.59l.16-.3Z'/>
      <path d='M222.68,331.45v1.45h-7.39v-1.23l3.74-3.81a12,12,0,0,0,1.33-1.57,2.24,2.24,0,0,0,.41-1.25,1.87,1.87,0,0,0-.49-1.29,1.72,1.72,0,0,0-1.36-.55,1.57,1.57,0,0,0-1,.31,1.8,1.8,0,0,0-.56.83,3.2,3.2,0,0,0-.17,1.09h-1.64a4.15,4.15,0,0,1,.37-1.74,3.21,3.21,0,0,1,1.14-1.34,3.4,3.4,0,0,1,1.93-.51,3.77,3.77,0,0,1,1.78.4,3,3,0,0,1,1.21,1.11,3.08,3.08,0,0,1,.43,1.63,3.52,3.52,0,0,1-.64,2,14.11,14.11,0,0,1-1.58,1.93l-2.44,2.51v.06Z'/>
      <path d='M258,328.13v-1.47h.79a1.86,1.86,0,0,0,1.42-.53,1.73,1.73,0,0,0,.49-1.25,1.65,1.65,0,0,0-.49-1.25,2,2,0,0,0-2.53,0,1.4,1.4,0,0,0-.49,1.12h-1.61a2.71,2.71,0,0,1,.4-1.48,2.84,2.84,0,0,1,1.16-1,3.91,3.91,0,0,1,1.77-.37,4.3,4.3,0,0,1,1.77.35,2.78,2.78,0,0,1,1.22,1,3,3,0,0,1,.44,1.66,2.79,2.79,0,0,1-.41,1.36,2.68,2.68,0,0,1-1.25,1.11,2.56,2.56,0,0,1,1.45,1.06,3,3,0,0,1,.42,1.52,2.86,2.86,0,0,1-.5,1.69,3.21,3.21,0,0,1-1.33,1.07,4.59,4.59,0,0,1-1.85.37,4.34,4.34,0,0,1-1.71-.33,2.94,2.94,0,0,1-1.24-1,2.78,2.78,0,0,1-.46-1.64h1.68a1.46,1.46,0,0,0,.47,1.14,1.8,1.8,0,0,0,1.28.43,2.11,2.11,0,0,0,1.4-.46,1.57,1.57,0,0,0,.57-1.3,1.6,1.6,0,0,0-.58-1.35,2.4,2.4,0,0,0-1.49-.43Z'/>
      <path d='M303,329.37v1.46H301.5v2.07H300v-2.07h-5v-1.1l4.73-7.74h1.73v7.38Zm-6.09,0H300v-5.32l-.23.54Z'/>
      <path d='M337.07,327.9l-1.46-.33.64-5.58h5.89v1.51h-4.46l-.39,3h.06a2.42,2.42,0,0,1,.73-.44,2.88,2.88,0,0,1,1.08-.2,3.56,3.56,0,0,1,1.78.44,3.18,3.18,0,0,1,1.2,1.24,3.82,3.82,0,0,1,.44,1.86,4,4,0,0,1-.42,1.83,3.19,3.19,0,0,1-1.25,1.3,4,4,0,0,1-2,.48,3.54,3.54,0,0,1-2.37-.8,3.08,3.08,0,0,1-1-2.28h1.62a2.16,2.16,0,0,0,.59,1.24,1.71,1.71,0,0,0,1.26.46,1.92,1.92,0,0,0,1.45-.62,2.26,2.26,0,0,0,.59-1.61,2.08,2.08,0,0,0-.62-1.65,2.16,2.16,0,0,0-1.45-.53,2.84,2.84,0,0,0-1.11.19A2.51,2.51,0,0,0,337.07,327.9Z'/>
      <path d='M380.77,324.77a3.82,3.82,0,0,0-.31-.72,1.48,1.48,0,0,0-1.41-.8,1.85,1.85,0,0,0-1,.31,2.25,2.25,0,0,0-.77,1.11,6.82,6.82,0,0,0-.35,2.23,2.69,2.69,0,0,1,1-.73,3.38,3.38,0,0,1,1.27-.24,3.1,3.1,0,0,1,1.77.5,3.33,3.33,0,0,1,1.13,1.31,4,4,0,0,1,.39,1.74,3.91,3.91,0,0,1-.41,1.77,3.29,3.29,0,0,1-1.21,1.3,3.52,3.52,0,0,1-1.9.5,3.16,3.16,0,0,1-2.72-1.33,6.61,6.61,0,0,1-.95-3.85v-.38a10.7,10.7,0,0,1,.38-3,4,4,0,0,1,1.18-2,3.23,3.23,0,0,1,2.15-.7,3.1,3.1,0,0,1,2.22.78,3.93,3.93,0,0,1,1.12,2.18ZM379,327.33a2,2,0,0,0-1.48.57,1.94,1.94,0,0,0-.56,1.43,2.63,2.63,0,0,0,.29,1.3,1.9,1.9,0,0,0,.74.77,2,2,0,0,0,1,.25,1.73,1.73,0,0,0,1.37-.58,2.29,2.29,0,0,0,.52-1.59,2.83,2.83,0,0,0-.19-1.07,1.7,1.7,0,0,0-.61-.78A1.78,1.78,0,0,0,379,327.33Z'/>
      <path d='M422.55,322v1.35l-4.21,9.56h-1.78l4.26-9.41h-5.31V322Z'/>
      <path d='M462.55,329.84a3.11,3.11,0,0,1-.46,1.72,3.06,3.06,0,0,1-1.26,1.1,4.3,4.3,0,0,1-3.58,0,3,3,0,0,1-1.28-1.1,3.1,3.1,0,0,1-.47-1.72,3,3,0,0,1,.44-1.63,3,3,0,0,1,1.19-1,2.61,2.61,0,0,1-1.39-2.41,2.89,2.89,0,0,1,.43-1.6,2.71,2.71,0,0,1,1.18-1,4.14,4.14,0,0,1,1.69-.33,4.06,4.06,0,0,1,1.68.33,2.65,2.65,0,0,1,1.16,1,2.81,2.81,0,0,1,.43,1.6,2.71,2.71,0,0,1-.38,1.46,2.59,2.59,0,0,1-1,1,3,3,0,0,1,1.2,1.06A2.93,2.93,0,0,1,462.55,329.84Zm-1.64-.06a1.92,1.92,0,0,0-.25-1,1.86,1.86,0,0,0-.69-.7,1.78,1.78,0,0,0-1-.26,1.72,1.72,0,0,0-.95.26,1.88,1.88,0,0,0-.68.69,1.94,1.94,0,0,0-.26,1,1.88,1.88,0,0,0,.26,1,1.9,1.9,0,0,0,.69.69,2,2,0,0,0,1,.25,1.76,1.76,0,0,0,1.33-.55A1.87,1.87,0,0,0,460.91,329.78Zm-.2-5a1.45,1.45,0,0,0-.49-1.15,1.9,1.9,0,0,0-2.38,0,1.4,1.4,0,0,0-.5,1.15,1.56,1.56,0,0,0,.51,1.21,1.69,1.69,0,0,0,1.2.47,1.65,1.65,0,0,0,1.17-.46A1.57,1.57,0,0,0,460.71,324.82Z'/>
      <path d='M497.18,330.12a3.23,3.23,0,0,0,.3.72,1.7,1.7,0,0,0,.53.57,1.56,1.56,0,0,0,.89.23,1.83,1.83,0,0,0,1-.32,2.18,2.18,0,0,0,.77-1.1A6.91,6.91,0,0,0,501,328a2.64,2.64,0,0,1-1,.72,3.2,3.2,0,0,1-1.27.25,3.1,3.1,0,0,1-1.77-.5,3.36,3.36,0,0,1-1.14-1.31,4,4,0,0,1-.39-1.74,3.79,3.79,0,0,1,.42-1.77,3.31,3.31,0,0,1,1.21-1.31,3.47,3.47,0,0,1,1.9-.49,3.12,3.12,0,0,1,2.71,1.33,6.61,6.61,0,0,1,.95,3.85v.38a10.72,10.72,0,0,1-.37,3,4.09,4.09,0,0,1-1.19,2,3.21,3.21,0,0,1-2.15.69,3.13,3.13,0,0,1-2.22-.77,3.93,3.93,0,0,1-1.12-2.18Zm-.13-4.71a2.83,2.83,0,0,0,.2,1.07,1.64,1.64,0,0,0,.61.78,1.76,1.76,0,0,0,1.07.3,2,2,0,0,0,1.48-.57,1.94,1.94,0,0,0,.55-1.44,2.62,2.62,0,0,0-.28-1.29,1.9,1.9,0,0,0-.74-.77,2,2,0,0,0-1-.25,1.74,1.74,0,0,0-1.37.58A2.34,2.34,0,0,0,497.05,325.41Z'/>
      <path d='M535.89,322V332.9h-1.57v-8.61h-.09a1.51,1.51,0,0,1-.73.51,3.91,3.91,0,0,1-1.52.26v-1.38a2.41,2.41,0,0,0,1.14-.24,2.52,2.52,0,0,0,.73-.56,3.35,3.35,0,0,0,.41-.59l.16-.3Zm10.34,4.73V328a6.12,6.12,0,0,1-1,3.72,3.17,3.17,0,0,1-2.71,1.32,3.26,3.26,0,0,1-2.75-1.32,6,6,0,0,1-1-3.72v-1.29a5.68,5.68,0,0,1,1-3.62,3.29,3.29,0,0,1,2.74-1.26,3.24,3.24,0,0,1,2.71,1.26A5.74,5.74,0,0,1,546.23,326.72Zm-1.67,1.55v-1.82a4.44,4.44,0,0,0-.52-2.31,1.66,1.66,0,0,0-1.51-.86,1.7,1.7,0,0,0-1.54.86,4.34,4.34,0,0,0-.53,2.31v1.82a4.85,4.85,0,0,0,.53,2.4,1.7,1.7,0,0,0,1.55.93,1.65,1.65,0,0,0,1.51-.93A5,5,0,0,0,544.56,328.27Z'/>
      <path d='M577,322V332.9h-1.57v-8.61h-.09a1.51,1.51,0,0,1-.73.51,3.91,3.91,0,0,1-1.52.26v-1.38a2.41,2.41,0,0,0,1.14-.24,2.52,2.52,0,0,0,.73-.56,3.35,3.35,0,0,0,.41-.59l.16-.3Zm6.82,0V332.9h-1.57v-8.61h-.09a1.46,1.46,0,0,1-.73.51,3.86,3.86,0,0,1-1.51.26v-1.38a2.35,2.35,0,0,0,1.86-.8,2.89,2.89,0,0,0,.41-.59,2.1,2.1,0,0,1,.17-.3Z'/>
      <path d='M615.9,322V332.9h-1.57v-8.61h-.08a1.55,1.55,0,0,1-.74.51,3.86,3.86,0,0,1-1.51.26v-1.38a2.35,2.35,0,0,0,1.86-.8,2.89,2.89,0,0,0,.41-.59c.09-.17.15-.27.17-.3Zm10.26,9.46v1.45h-7.39v-1.23l3.74-3.81a11.22,11.22,0,0,0,1.33-1.57,2.24,2.24,0,0,0,.41-1.25,1.87,1.87,0,0,0-.49-1.29,1.71,1.71,0,0,0-1.35-.55,1.58,1.58,0,0,0-1,.31,1.8,1.8,0,0,0-.56.83,3.46,3.46,0,0,0-.17,1.09H619a4.15,4.15,0,0,1,.37-1.74,3.21,3.21,0,0,1,1.14-1.34,3.4,3.4,0,0,1,1.93-.51,3.77,3.77,0,0,1,1.78.4,3,3,0,0,1,1.21,1.11,3.08,3.08,0,0,1,.43,1.63,3.52,3.52,0,0,1-.64,2,14.11,14.11,0,0,1-1.58,1.93l-2.44,2.51v.06Z'/>
      </g> 
      <g id='numberGroup' fill='#f2f2f2'>
      <path d='M180.39,322V332.9h-1.57v-8.61h-.09a1.46,1.46,0,0,1-.73.51,3.88,3.88,0,0,1-1.52.26v-1.38a2.35,2.35,0,0,0,1.87-.8,3.35,3.35,0,0,0,.41-.59l.16-.3Z'/>
      <path d='M222.68,331.45v1.45h-7.39v-1.23l3.74-3.81a12,12,0,0,0,1.33-1.57,2.24,2.24,0,0,0,.41-1.25,1.87,1.87,0,0,0-.49-1.29,1.72,1.72,0,0,0-1.36-.55,1.57,1.57,0,0,0-1,.31,1.8,1.8,0,0,0-.56.83,3.2,3.2,0,0,0-.17,1.09h-1.64a4.15,4.15,0,0,1,.37-1.74,3.21,3.21,0,0,1,1.14-1.34,3.4,3.4,0,0,1,1.93-.51,3.77,3.77,0,0,1,1.78.4,3,3,0,0,1,1.21,1.11,3.08,3.08,0,0,1,.43,1.63,3.52,3.52,0,0,1-.64,2,14.11,14.11,0,0,1-1.58,1.93l-2.44,2.51v.06Z'/>
      <path d='M258,328.13v-1.47h.79a1.86,1.86,0,0,0,1.42-.53,1.73,1.73,0,0,0,.49-1.25,1.65,1.65,0,0,0-.49-1.25,2,2,0,0,0-2.53,0,1.4,1.4,0,0,0-.49,1.12h-1.61a2.71,2.71,0,0,1,.4-1.48,2.84,2.84,0,0,1,1.16-1,3.91,3.91,0,0,1,1.77-.37,4.3,4.3,0,0,1,1.77.35,2.78,2.78,0,0,1,1.22,1,3,3,0,0,1,.44,1.66,2.79,2.79,0,0,1-.41,1.36,2.68,2.68,0,0,1-1.25,1.11,2.56,2.56,0,0,1,1.45,1.06,3,3,0,0,1,.42,1.52,2.86,2.86,0,0,1-.5,1.69,3.21,3.21,0,0,1-1.33,1.07,4.59,4.59,0,0,1-1.85.37,4.34,4.34,0,0,1-1.71-.33,2.94,2.94,0,0,1-1.24-1,2.78,2.78,0,0,1-.46-1.64h1.68a1.46,1.46,0,0,0,.47,1.14,1.8,1.8,0,0,0,1.28.43,2.11,2.11,0,0,0,1.4-.46,1.57,1.57,0,0,0,.57-1.3,1.6,1.6,0,0,0-.58-1.35,2.4,2.4,0,0,0-1.49-.43Z'/>
      <path d='M303,329.37v1.46H301.5v2.07H300v-2.07h-5v-1.1l4.73-7.74h1.73v7.38Zm-6.09,0H300v-5.32l-.23.54Z'/>
      <path d='M337.07,327.9l-1.46-.33.64-5.58h5.89v1.51h-4.46l-.39,3h.06a2.42,2.42,0,0,1,.73-.44,2.88,2.88,0,0,1,1.08-.2,3.56,3.56,0,0,1,1.78.44,3.18,3.18,0,0,1,1.2,1.24,3.82,3.82,0,0,1,.44,1.86,4,4,0,0,1-.42,1.83,3.19,3.19,0,0,1-1.25,1.3,4,4,0,0,1-2,.48,3.54,3.54,0,0,1-2.37-.8,3.08,3.08,0,0,1-1-2.28h1.62a2.16,2.16,0,0,0,.59,1.24,1.71,1.71,0,0,0,1.26.46,1.92,1.92,0,0,0,1.45-.62,2.26,2.26,0,0,0,.59-1.61,2.08,2.08,0,0,0-.62-1.65,2.16,2.16,0,0,0-1.45-.53,2.84,2.84,0,0,0-1.11.19A2.51,2.51,0,0,0,337.07,327.9Z'/>
      <path d='M380.77,324.77a3.82,3.82,0,0,0-.31-.72,1.48,1.48,0,0,0-1.41-.8,1.85,1.85,0,0,0-1,.31,2.25,2.25,0,0,0-.77,1.11,6.82,6.82,0,0,0-.35,2.23,2.69,2.69,0,0,1,1-.73,3.38,3.38,0,0,1,1.27-.24,3.1,3.1,0,0,1,1.77.5,3.33,3.33,0,0,1,1.13,1.31,4,4,0,0,1,.39,1.74,3.91,3.91,0,0,1-.41,1.77,3.29,3.29,0,0,1-1.21,1.3,3.52,3.52,0,0,1-1.9.5,3.16,3.16,0,0,1-2.72-1.33,6.61,6.61,0,0,1-.95-3.85v-.38a10.7,10.7,0,0,1,.38-3,4,4,0,0,1,1.18-2,3.23,3.23,0,0,1,2.15-.7,3.1,3.1,0,0,1,2.22.78,3.93,3.93,0,0,1,1.12,2.18ZM379,327.33a2,2,0,0,0-1.48.57,1.94,1.94,0,0,0-.56,1.43,2.63,2.63,0,0,0,.29,1.3,1.9,1.9,0,0,0,.74.77,2,2,0,0,0,1,.25,1.73,1.73,0,0,0,1.37-.58,2.29,2.29,0,0,0,.52-1.59,2.83,2.83,0,0,0-.19-1.07,1.7,1.7,0,0,0-.61-.78A1.78,1.78,0,0,0,379,327.33Z'/>
      <path d='M422.55,322v1.35l-4.21,9.56h-1.78l4.26-9.41h-5.31V322Z'/>
      <path d='M462.55,329.84a3.11,3.11,0,0,1-.46,1.72,3.06,3.06,0,0,1-1.26,1.1,4.3,4.3,0,0,1-3.58,0,3,3,0,0,1-1.28-1.1,3.1,3.1,0,0,1-.47-1.72,3,3,0,0,1,.44-1.63,3,3,0,0,1,1.19-1,2.61,2.61,0,0,1-1.39-2.41,2.89,2.89,0,0,1,.43-1.6,2.71,2.71,0,0,1,1.18-1,4.14,4.14,0,0,1,1.69-.33,4.06,4.06,0,0,1,1.68.33,2.65,2.65,0,0,1,1.16,1,2.81,2.81,0,0,1,.43,1.6,2.71,2.71,0,0,1-.38,1.46,2.59,2.59,0,0,1-1,1,3,3,0,0,1,1.2,1.06A2.93,2.93,0,0,1,462.55,329.84Zm-1.64-.06a1.92,1.92,0,0,0-.25-1,1.86,1.86,0,0,0-.69-.7,1.78,1.78,0,0,0-1-.26,1.72,1.72,0,0,0-.95.26,1.88,1.88,0,0,0-.68.69,1.94,1.94,0,0,0-.26,1,1.88,1.88,0,0,0,.26,1,1.9,1.9,0,0,0,.69.69,2,2,0,0,0,1,.25,1.76,1.76,0,0,0,1.33-.55A1.87,1.87,0,0,0,460.91,329.78Zm-.2-5a1.45,1.45,0,0,0-.49-1.15,1.9,1.9,0,0,0-2.38,0,1.4,1.4,0,0,0-.5,1.15,1.56,1.56,0,0,0,.51,1.21,1.69,1.69,0,0,0,1.2.47,1.65,1.65,0,0,0,1.17-.46A1.57,1.57,0,0,0,460.71,324.82Z'/>
      <path d='M497.18,330.12a3.23,3.23,0,0,0,.3.72,1.7,1.7,0,0,0,.53.57,1.56,1.56,0,0,0,.89.23,1.83,1.83,0,0,0,1-.32,2.18,2.18,0,0,0,.77-1.1A6.91,6.91,0,0,0,501,328a2.64,2.64,0,0,1-1,.72,3.2,3.2,0,0,1-1.27.25,3.1,3.1,0,0,1-1.77-.5,3.36,3.36,0,0,1-1.14-1.31,4,4,0,0,1-.39-1.74,3.79,3.79,0,0,1,.42-1.77,3.31,3.31,0,0,1,1.21-1.31,3.47,3.47,0,0,1,1.9-.49,3.12,3.12,0,0,1,2.71,1.33,6.61,6.61,0,0,1,.95,3.85v.38a10.72,10.72,0,0,1-.37,3,4.09,4.09,0,0,1-1.19,2,3.21,3.21,0,0,1-2.15.69,3.13,3.13,0,0,1-2.22-.77,3.93,3.93,0,0,1-1.12-2.18Zm-.13-4.71a2.83,2.83,0,0,0,.2,1.07,1.64,1.64,0,0,0,.61.78,1.76,1.76,0,0,0,1.07.3,2,2,0,0,0,1.48-.57,1.94,1.94,0,0,0,.55-1.44,2.62,2.62,0,0,0-.28-1.29,1.9,1.9,0,0,0-.74-.77,2,2,0,0,0-1-.25,1.74,1.74,0,0,0-1.37.58A2.34,2.34,0,0,0,497.05,325.41Z'/>
      <path d='M535.89,322V332.9h-1.57v-8.61h-.09a1.51,1.51,0,0,1-.73.51,3.91,3.91,0,0,1-1.52.26v-1.38a2.41,2.41,0,0,0,1.14-.24,2.52,2.52,0,0,0,.73-.56,3.35,3.35,0,0,0,.41-.59l.16-.3Zm10.34,4.73V328a6.12,6.12,0,0,1-1,3.72,3.17,3.17,0,0,1-2.71,1.32,3.26,3.26,0,0,1-2.75-1.32,6,6,0,0,1-1-3.72v-1.29a5.68,5.68,0,0,1,1-3.62,3.29,3.29,0,0,1,2.74-1.26,3.24,3.24,0,0,1,2.71,1.26A5.74,5.74,0,0,1,546.23,326.72Zm-1.67,1.55v-1.82a4.44,4.44,0,0,0-.52-2.31,1.66,1.66,0,0,0-1.51-.86,1.7,1.7,0,0,0-1.54.86,4.34,4.34,0,0,0-.53,2.31v1.82a4.85,4.85,0,0,0,.53,2.4,1.7,1.7,0,0,0,1.55.93,1.65,1.65,0,0,0,1.51-.93A5,5,0,0,0,544.56,328.27Z'/>
      <path d='M577,322V332.9h-1.57v-8.61h-.09a1.51,1.51,0,0,1-.73.51,3.91,3.91,0,0,1-1.52.26v-1.38a2.41,2.41,0,0,0,1.14-.24,2.52,2.52,0,0,0,.73-.56,3.35,3.35,0,0,0,.41-.59l.16-.3Zm6.82,0V332.9h-1.57v-8.61h-.09a1.46,1.46,0,0,1-.73.51,3.86,3.86,0,0,1-1.51.26v-1.38a2.35,2.35,0,0,0,1.86-.8,2.89,2.89,0,0,0,.41-.59,2.1,2.1,0,0,1,.17-.3Z'/>
      <path d='M615.9,322V332.9h-1.57v-8.61h-.08a1.55,1.55,0,0,1-.74.51,3.86,3.86,0,0,1-1.51.26v-1.38a2.35,2.35,0,0,0,1.86-.8,2.89,2.89,0,0,0,.41-.59c.09-.17.15-.27.17-.3Zm10.26,9.46v1.45h-7.39v-1.23l3.74-3.81a11.22,11.22,0,0,0,1.33-1.57,2.24,2.24,0,0,0,.41-1.25,1.87,1.87,0,0,0-.49-1.29,1.71,1.71,0,0,0-1.35-.55,1.58,1.58,0,0,0-1,.31,1.8,1.8,0,0,0-.56.83,3.46,3.46,0,0,0-.17,1.09H619a4.15,4.15,0,0,1,.37-1.74,3.21,3.21,0,0,1,1.14-1.34,3.4,3.4,0,0,1,1.93-.51,3.77,3.77,0,0,1,1.78.4,3,3,0,0,1,1.21,1.11,3.08,3.08,0,0,1,.43,1.63,3.52,3.52,0,0,1-.64,2,14.11,14.11,0,0,1-1.58,1.93l-2.44,2.51v.06Z'/>
      </g>
      <rect id='dragger' x='127' y='269' width='26' height='26' fill='transparent'/>
      <rect id='cover' x='167' y='269' width='466' height='60' fill='transparent' />
      <use class='preservationText' xlink:href='#preservationText' x='167' y='242' opacity='0.2'/>
      
      </svg>"
    
      HTML(tl)
      })
    
    output$timeline5 <- renderUI({
      # Source: https://codepen.io/chrisgannon/pen/djjZBq
      
      tl <-"<section class='cd-horizontal-timeline'>
		<div class='timeline'>
			<div class='events-wrapper'>
				<div class='events'>
					<ol>
						<li><a href='#0' data-date='16/01/1997' class='selected'>1997 - 2001</a></li>
      <li><a href='#0' data-date='28/02/2001'>2001 - 2002</a></li>
        <li><a href='#0' data-date='20/04/2002'>2002 - 2005</a></li>
          <li><a href='#0' data-date='20/05/2006'>2006</a></li>
            <li><a href='#0' data-date='09/07/2007'>2007</a></li>
              <li><a href='#0' data-date='30/08/2008'>2008</a></li>	
                </ol>
                
                <span class='filling-line' aria-hidden='true'></span>
                  </div> <!-- .events -->
                  </div> <!-- .events-wrapper -->
                  
                  <ul class='cd-timeline-navigation'>
                    <li><a href='#0' class='prev inactive'><p>&#x21E9</p></a></li>
                      <li><a href='#0' class='next'><p>&#x21E9</p></a></li>
                        </ul> <!-- .cd-timeline-navigation -->
                        </div> <!-- .timeline -->
                        
                        <div class='events-content'>
                          <ol>
                          <li class='selected' data-date='16/01/1997'>
                            <h2>toto.</h2>
                            <p>toto</p>
                            </li>
                            
                            <li data-date='28/02/2001'>
                              <h2>toto.</h2>
                              <p>toto</p>
                              </li>
                              
                              <li data-date='20/04/2002'>
                                <h2>toto.</h2>
                                <p>toto.</p>
                                </li>
                                
                                <li data-date='20/05/2006'>
                                  <h2>toto.</h2>
                                  <p>toto.</p>
                                  </li>
                                  
                                  <li data-date='09/07/2007'>
                                    <h2>toto</h2>
                                    <p> toto</p>
                                    </li>
                                    
                                    <li data-date='30/08/2008'>
                                      <h2>Lorem </h2>
                                      <p>Lorem </p>
                                      </li>
                                      </ol>
                                      </div> <!-- .events-content -->
                                      </section>"
    HTML(tl)
    })
    
    
    output$timeline6 <- renderUI({
      # Source: https://codepen.io/chrisgannon/pen/djjZBq
      
      tl <-"<div id='content'>
  <h1>Timeline Concept</h1>

  <ul class='timeline'>
    <li class='event' data-date='12:30 - 1:00pm'>
      <h3>Registration</h3>
      <p>Get here on time, it's first come first serve. Be late, get turned away.</p>
      <p>Get here on time, it's first come first serve. Be late, get turned away.</p>
      <p>Get here on time, it's first come first serve. Be late, get turned away.</p>
    </li>
    <li class='event' data-date='2:30 - 4:00pm'>
      <h3>Opening Ceremony</h3>
      <p>Get ready for an exciting event, this will kick off in amazing fashion with MOP & Busta Rhymes as an opening show.</p>    
    </li>
    <li class='event' data-date='5:00 - 8:00pm'>
      <h3>Main Event</h3>
      <p>This is where it all goes down. You will compete head to head with your friends and rivals. Get ready!</p>    
    </li>
    <li class='event' data-date='8:30 - 9:30pm'>
      <h3>Closing Ceremony</h3>
      <p>See how is the victor and who are the losers. The big stage is where the winners bask in their own glory.</p>    
    </li>
  </ul>
</div>"
      
      HTML(tl)
    })
    
    
    
    
    
    output$timeline7 <- renderUI({
      
      txt <- "<!-- Based on: https://dribbble.com/shots/5260798-Process -->

<div class='main-container'>
    <div class='steps-container'>
        <div class='step completed'>
            <svg xmlns='http://www.w3.org/2000/svg' width='24' height='24' viewBox='0 0 24 24'>
                <path d='M20.285 2l-11.285 11.567-5.286-5.011-3.714 3.716 9 8.728 15-15.285z' />
            </svg>
            <div class='label completed'>
                Prospect
            </div>
            <div class='icon completed'>
                <i class='far fa-handshake'></i>
            </div>
        </div>
        <div class='line completed'></div>
        <div class='step completed'>
            <svg xmlns='http://www.w3.org/2000/svg' width='24' height='24' viewBox='0 0 24 24'>
                <path d='M20.285 2l-11.285 11.567-5.286-5.011-3.714 3.716 9 8.728 15-15.285z' />
            </svg>
            <div class='label completed'>
                Tour
            </div>
            <div class='icon completed'>
                <i class='far fa-map'></i>
            </div>
        </div>
        <div class='line next-step-in-progress'>
        </div>
        <div class='step in-progress'>
            <div class='preloader'></div>
            <div class='label loading'>
                Offer
            </div>
            <div class='icon in-progress'>
                <i class='far fa-money-bill-alt'></i>
            </div>
        </div>
        <div class='line prev-step-in-progress'></div>
        <div class='step'>
            <div class='label'>
                Contract
            </div>
            <div class='icon'>
                <i class='far fa-newspaper'></i>
            </div>
        </div>
        <div class='line'></div>
        <div class='step'>
            <div class='label'>
                Settled
            </div>
            <div class='icon'>
                <i class='fas fa-home'></i>
            </div>
        </div>
    </div>
</div>"
      
      HTML(txt)
    })
    
    
    
    # return value of the tl.update
    list(rstBtn = reactive(input$rstBtn),
         nextBtn = reactive(input$nextBtn),
         prevBtn = reactive(input$prevBtn),
         skipBtn = reactive(input$skipBtn)
         )
    
  })
}



