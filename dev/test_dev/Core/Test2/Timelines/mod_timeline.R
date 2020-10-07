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
                         uiOutput(ns('showResetBtn')),
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
mod_timeline_server <- function(id, style=1, config, cmd='', position){
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    verbose = T
    source(file.path('.', 'code_general.R'), local=TRUE)$value
    
    
    output$showResetBtn <- renderUI({
      actionButton(ns("rstBtn"), paste0("Reset ", config$type),
                   class = redBtnClass,
                   style='padding:4px; font-size:80%')
      })
    
    output$timelineStyle <- renderUI({ uiOutput(ns(paste0('timeline', style))) })
    
    
    observeEvent(position(),{current$val <- position()})
    
    
    current <- reactiveValues(
      val = 1,
      nbSteps = NULL,
      userToggleBtns = list(
        rst = TRUE,
        nxt = TRUE,
        prv = TRUE)
    )
    
    observeEvent(req(config),{
      current$nbSteps <- length(config$steps)
      InitScreens()
    })
    
    # Initialization of the screens with integrating them into a div specific
    # to this module (name prefixed with the ns() function
    # Those div englobs the div of the caller where screens are defined
    InitScreens <- reactive({
      config$screens <- lapply(1:current$nbSteps,
                               function(x){
                                 config$screens[[x]] <- if (x == current$val) 
                                   div(id = ns(paste0("div_screen", x)),  config$screens[[x]])
                                 else 
                                   shinyjs::hidden(div(id = ns(paste0("div_screen", x)),  config$screens[[x]]))
                               })
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
    
    NextBtn_default_logics <- reactive({
     
      end_of_tl <- current$val == current$nbSteps
      mandatory_step <- isTRUE(config$steps[[current$val]])
      validated <- isTRUE(config$isDone[[current$val]])
      !mandatory_step || validated
    })
    
    PrevBtn_default_logics <- reactive({
      start_of_tl <- current$val == 1
      !start_of_tl
    })
    
    # Catch a new position or a change in the isDone list
    observeEvent(c(current$val, config$isDone), {
      if(verbose)
        print(paste0('TL(',config$process.name, ') : observeEvent(c(current$val, config$isDone, current$userToggleBtns : ', paste0(current$userToggleBtns, collapse=' ')))
      
      shinyjs::toggleState('prevBtn', cond = PrevBtn_default_logics() && current$userToggleBtns$prv)
      shinyjs::toggleState('nextBtn', cond = NextBtn_default_logics() &&  current$userToggleBtns$nxt)
      
      # Display current page
      lapply(1:current$nbSteps, function(x){
        shinyjs::toggle(paste0('div_screen', x), condition = x==current$val)})
    })
    
    
    
    
    # Catch a command name to toggle the state enable/disable of screens
    # Each command is received with an integer as suffix which correspond to
    # an event_count in the caller module. This is used to virtually create a
    # new event which will be triggered by observeEvent event if the value
    # is the same.
    observeEvent(req(cmd()), {
      if(verbose)
        print(paste0('TL(',config$process.name, ') : New event on actions()$toggleSteps : ', paste0(cmd(), collapse=' ')))
      #browser()
      AnalyseCmd <- function(c) {
        #print(paste0('extracted cmd = ', c))
        switch(c,
               DisableAllSteps = toggleState_Steps(cond = F, i = current$nbSteps),
               EnableAllSteps = toggleState_Steps(cond = T, i = current$nbSteps),
               DisableAllPrevSteps = {
                 pos <- max(grep(TRUE, unlist(config$isDone)))
                 #pos <- current$val
                 toggleState_Steps(cond = F, i = pos)
               },
               ResetActionBtns = ResetActionsBtns()
        )
      }
      
      lapply(cmd()[-length(cmd())], function(x) AnalyseCmd(x))
    })
    
    toggleState_Steps <- function(cond, i){
      if(verbose)
        print(paste0('TL(',config$process.name, ') : toggleState_Steps() : cond = ', cond, ', i = ', i))
      
      lapply(1:i, function(x){
        shinyjs::toggleState(paste0('div_screen', x), condition = cond)})
    }
    
    ResetActionsBtns <- function(){
      lapply(current$userToggleBtns, function(x) x <- TRUE)
    }
    
    ##
    ## Functions defining timeline and styles
    ##
    output$load_css_style <- renderUI({
      req(current$nbSteps)
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
      status <- rep('', current$nbSteps)
      status[current$val] <- ' active'
      steps <- names(config$steps)
      txt <- "<div class='flex-parent'> <div class='input-flex-container'>"
      for (i in 1:current$nbSteps){
        txt <- paste0(txt, "<div class='input",status[i], "'><span name='", steps[i],"'></span>  </div>")
      }
      txt <- paste0(txt,"</div></div>")
      HTML(txt)
    })
    
    output$timeline2 <- renderUI({
      config
      status <- rep('', current$nbSteps)
      #browser()
      if( !is.null(config$steps))
        status[which(unlist(config$steps))] <- 'mandatory'
      
      #status <- rep('',length(config$stepsNames))
      status[which(unlist(config$isDone))] <- 'complete'
      
      #Compute the skipped steps
      status[which(config$isDone==F)[which(which(config$isDone == FALSE ) < GetMaxTrue(config$isDone, current$nbSteps))]] <- 'skipped'
      
      
      active  <- rep('', GetMaxTrue(config$isDone, current$nbSteps))
      active[current$val] <- 'active'
      
      steps <- names(config$steps)
      txt <- "<ul class='timeline' id='timeline'>"
      for (i in 1:current$nbSteps){
        txt <- paste0(txt, "<li class='li ",status[i]," ",active[i],"'><div class='timestamp'></div><div class='status'><h4>", steps[i],"</h4></div></li>")
      }
      txt <- paste0(txt,"</ul>")
      
      HTML(txt)
    })
    
    
    output$timeline3 <- renderUI({
      config
      
      color <- rep("lightgrey", current$nbSteps)
      colorForCursor <- rep("white", current$nbSteps)
      
      for (i in 1:length(config$stepsNames)){
        status <- config$isDone[[i]]
        col <- ifelse(!is.null(config$steps) && config$steps[[i]], "red", orangeProstar)
        ifelse(status, color[i] <- "green", color[i] <- col)
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
    
    
    
    output$timeline10 <- renderUI({
      txt <- "<section class='cd-horizontal-timeline'>
        <div class='timeline'>
          <div class='events-wrapper'>
            <div class='events'>
              <ol>
              <li><a href='#0' data-date='00/00/00' class='selected'>00:00</a></li>
                <li><a href='#0' data-date='01/00/00'>01:00</a></li>
                  <li><a href='#0' data-date='02/00/00'>02:00</a></li>
                    <li><a href='#0' data-date='03/00/00'>03:00</a></li>
                      <li><a href='#0' data-date='04/00/00'>04:00</a></li>
                        <li><a href='#0' data-date='05/00/00'>05:00</a></li>
                          <li><a href='#0' data-date='06/00/00'>06:00</a></li>
                            <li><a href='#0' data-date='07/00/00'>07:00</a></li>
                              <li><a href='#0' data-date='08/00/00'>08:00</a></li>
                                <li><a href='#0' data-date='09/00/00'>09:00</a></li>
                                  <li><a href='#0' data-date='10/00/00'>10:00</a></li>
                                    <li><a href='#0' data-date='11/00/00'>11:00</a></li>
                                      <li><a href='#0' data-date='12/00/00'>12:00</a></li>
                                        <li><a href='#0' data-date='13/00/00'>13:00</a></li>
                                          <li><a href='#0' data-date='14/00/00'>14:00</a></li>
                                            <li><a href='#0' data-date='15/00/00'>15:00</a></li>
                                              <li><a href='#0' data-date='16/00/00'>16:00</a></li>
                                                <li><a href='#0' data-date='17/00/00'>17:00</a></li>
                                                  <li><a href='#0' data-date='18/00/00'>18:00</a></li>
                                                    <li><a href='#0' data-date='19/00/00'>19:00</a></li>
                                                      <li><a href='#0' data-date='20/00/00'>20:00</a></li>
                                                        <li><a href='#0' data-date='21/00/00'>21:00</a></li>
                                                          <li><a href='#0' data-date='22/00/00'>22:00</a></li>
                                                            <li><a href='#0' data-date='23/00/00'>23:00</a></li>
                                                              </ol>
                                                              
                                                              <span class='filling-line' aria-hidden='true'></span>
                                                                </div>
                                                                <!-- .events -->
                                                                </div>
                                                                <!-- .events-wrapper -->
                                                                
                                                                <ul class='cd-timeline-navigation'>
                                                                  <li><a href='#0' class='prev inactive'>Prev</a></li>
                                                                    <li><a href='#0' class='next'>Next</a></li>
                                                                      </ul>
                                                                      <!-- .cd-timeline-navigation -->
                                                                      </div>
                                                                      <!-- .timeline -->
                                                                      
                                                                      <div class='events-content'>
                                                                        <ol>
                                                                        <li class='selected' data-date='00/00/00'>
                                                                          
                                                                          <p>Consectetur adipisicing elit.
                                                                        </p>
                                                                          </li>
                                                                          <li data-date='01/00/00'>
                                                                            <p>Lorem ipsum dolor sit amet, consectetur adipisicing elit.
                                                                          </p>
                                                                            </li>
                                                                            <li data-date='02/00/00'>
                                                                              <p>Dolor sit amet, consectetur adipisicing elit.
                                                                            </p>
                                                                              </li>
                                                                              
                                                                              <li data-date='03/00/00'>
                                                                                <p>Lorem ipsum dolor sit amet
                                                                              </p>
                                                                                </li>
                                                                                
                                                                                <li data-date='04/00/00'>
                                                                                  <p>Lorem ipsum dolor sit amet, consectetur adipisicing elit.
                                                                                </p>
                                                                                  </li>
                                                                                  
                                                                                  <li data-date='05/00/00'>
                                                                                    <p>Lorem ipsum dolor sit amet, consectetur adipisicing elit.
                                                                                  </p>
                                                                                    </li>
                                                                                    
                                                                                    <li data-date='06/00/00'>
                                                                                      <p>Lorem ipsum dolor sit amet, consectetur adipisicing elit.
                                                                                    </p>
                                                                                      </li>
                                                                                      
                                                                                      <li data-date='07/00/00'>
                                                                                        <p>Lorem ipsum dolor sit amet, consectetur adipisicing elit.
                                                                                      </p>
                                                                                        </li>
                                                                                        
                                                                                        <li data-date='08/00/00'>
                                                                                          <p>Lorem ipsum dolor sit amet, consectetur adipisicing elit. 
                                                                                        </p>
                                                                                          </li>
                                                                                          
                                                                                          <li data-date='09/00/00'>
                                                                                            <p>Lorem ipsum dolor sit amet, consectetur adipisicing elit.
                                                                                          </p>
                                                                                            </li>
                                                                                            
                                                                                            <li data-date='10/00/00'>
                                                                                              <p>Lorem ipsum dolor sit amet, consectetur adipisicing elit.
                                                                                            </p>
                                                                                              </li>
                                                                                              
                                                                                              <li data-date='11/00/00'>
                                                                                                <p>Lorem ipsum dolor sit amet, consectetur adipisicing elit.
                                                                                              </p>
                                                                                                </li>
                                                                                                <li data-date='12/00/00'>
                                                                                                  <p>Lorem ipsum dolor sit amet, consectetur adipisicing elit.
                                                                                                </p>
                                                                                                  </li>
                                                                                                  <li data-date='13/00/00'>
                                                                                                    <p>Lorem ipsum dolor sit amet, consectetur adipisicing elit.
                                                                                                  </p>
                                                                                                    </li>
                                                                                                    <li data-date='14/00/00'>
                                                                                                      <p>Lorem ipsum dolor sit amet, consectetur adipisicing elit.
                                                                                                    </p>
                                                                                                      </li>
                                                                                                      <li data-date='15/00/00'>
                                                                                                        <p>Lorem ipsum dolor sit amet, consectetur adipisicing elit.
                                                                                                      </p>
                                                                                                        </li>
                                                                                                        <li data-date='16/00/00'>
                                                                                                          <p>Lorem ipsum dolor sit amet, consectetur adipisicing elit.
                                                                                                        </p>
                                                                                                          </li>
                                                                                                          <li data-date='17/00/00'>
                                                                                                            <p>Lorem ipsum dolor sit amet, consectetur adipisicing elit.
                                                                                                          </p>
                                                                                                            </li>
                                                                                                            <li data-date='18/00/00'>
                                                                                                              <p>Lorem ipsum dolor sit amet, consectetur adipisicing elit.
                                                                                                            </p>
                                                                                                              </li>
                                                                                                              <li data-date='19/00/00'>
                                                                                                                <p>Lorem ipsum dolor sit amet, consectetur adipisicing elit.
                                                                                                              </p>
                                                                                                                </li>
                                                                                                                <li data-date='20/00/00'>
                                                                                                                  <p>Lorem ipsum dolor sit amet, consectetur adipisicing elit.
                                                                                                                </p>
                                                                                                                  </li>
                                                                                                                  <li data-date='21/00/00'>
                                                                                                                    <p>Lorem ipsum dolor sit amet, consectetur adipisicing elit.
                                                                                                                  </p>
                                                                                                                    </li>
                                                                                                                    <li data-date='22/00/00'>
                                                                                                                      <p>Lorem ipsum dolor sit amet, consectetur adipisicing elit.
                                                                                                                    </p>
                                                                                                                      </li>
                                                                                                                      <li data-date='23/00/00'>
                                                                                                                        <p>Lorem ipsum dolor sit amet, consectetur adipisicing elit.
                                                                                                                      </p>
                                                                                                                        </li>
                                                                                                                        </ol>
                                                                                                                        </div>
                                                                                                                        <!-- .events-content -->
                                                                                                                        </section>
                                                                                                                        "
   HTML(txt)
    })
    
    
    list(rstBtn = reactive(input$rstBtn),
         prvBtn = reactive(input$prevBtn),
         nxtBtn = reactive(input$nextBtn),
         pos = reactive(current$val)
    )
    
  })
}
