# Module UI
  
#' @title   mod_navigation_ui and mod_navigation_server
#' @description  A shiny Module.
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
mod_navigation_ui <- function(id){
  ns <- NS(id)
  tagList(
    inlineCSS(progressWizard_CSS),
    div(
      div( style="align: center;display:inline-block; vertical-align: top; padding: 7px",
           shinyjs::disabled(actionButton(ns("rstBtn"), "reset", 
                                          class = PrevNextBtnClass,
                                          style='padding:4px; font-size:80%'))),
      div( style="align: center;display:inline-block; vertical-align: top; padding: 7px",
           shinyjs::disabled(actionButton(ns("prevBtn"), "<<", 
                                          class = PrevNextBtnClass,
                                          style='padding:4px; font-size:80%'))),
      div( style="align: center;display:inline-block; vertical-align: top;",
           uiOutput(ns("checkPanel" ))),
      
      
      div(style="align: center;display:inline-block; vertical-align: top; padding: 7px",
          actionButton(ns("nextBtn"), ">>", 
                       class = PrevNextBtnClass, 
                       style='padding:4px; font-size:80%')
          
      )
    ),
    hr(),
    uiOutput(ns("screens"))
    
    
  )
}
    
# Module Server
    
#' @rdname mod_navigation
#' @export
#' @keywords internal
#' @import shiny shinyjs
    
mod_navigation_server <- function(input, output, session, pages){
  ns <- session$ns
  
  current <- reactiveValues(
    # This variable is the indice of the current screen
    val = 1,
    nbSteps = NULL
  )
  
  observeEvent(req(pages()),{
    current$nbSteps <- length(pages()@stepsNames)
    print(current$nbSteps)
  })
  
  ##--------------------------------------------------------------
  ## Gestion des couleurs du slideshow
  ##--------------------------------------------------------------
  
  
  output$checkPanel <- renderUI({
    current$val
    color <- rep("lightgrey",current$nbSteps)
    colorForCursor <- rep("white",current$nbSteps)
    
    status <- pages()@isDone
    #buildTable(pages()@stepsNames, color,colorForCursor, params())
    
    for (i in 1:current$nbSteps){
      status <- pages()@isDone[i]
      col <- ifelse(pages()@isMandatory[i], "red", orangeProstar)
      ifelse(status, color[i] <- "green", color[i] <- col)
    }
    
    
    status[which(status==1)] <- 'completed'
    status[current$val] <- 'active'
    status[which(status==0)] <- 'undone'
    
    if (pages()@iconType == 'bubble') {
      txt <- "<ul class='progress-indicator'>"
    } else if (pages()@iconType == 'rectangle'){
      txt <- "<ul class='progress-indicator custom-complex'>"
    }
    
    steps <- pages()@stepsNames
    for( i in 1:length(steps) ) {
      txt <- paste0(txt, "<li class=",status[i], "> <span class='bubble'> </span>", steps[i], "</li>")
    }
    txt <- paste0(txt,"</ul>")
    
    return(HTML(txt))
  })
  
  observeEvent(req(input$rstBtn),{
    current$val <- 1
  })
  
  # observeEvent(c(pages()@forceReset,input$rstBtn),{
  #   req(input$rstBtn)
  #   current$val
  #   
  #   if (!is.null(pages()@forceReset) || input$rstBtn > 0){
  #     print("ON FAIT LE RESET EFFECTIF")
  #     pages()@rstFunc
  #     current$val <- 1
  #   }
  #   
  # })
  
  
  
  observeEvent(current$val,{
    shinyjs::toggle(id = "prevBtn", condition = (current$nbSteps > 1))
    shinyjs::toggle(id = "nextBtn", condition = (current$nbSteps > 1) )
    
    shinyjs::toggleState(id = "prevBtn", condition = current$val > 1)
    shinyjs::toggleState(id = "nextBtn", condition = current$val < current$nbSteps)
  })
  
  ##--------------------------------------------------------------
  ## Navigation dans le slideshow
  ##--------------------------------------------------------------
  
  navPage <- function(direction) {
    current$val <- current$val + direction
  }
  
  observeEvent(input$prevBtn,ignoreInit = TRUE,{navPage(-1)})
  observeEvent(input$nextBtn,ignoreInit = TRUE,{navPage(1)})
  
  
  
  #bars <- reactive({ })
  
  #screens <- reactive({
  output$screens <- renderUI({
    isolate({
      ll <- NULL
      #isolate({
      
      for (i in 1:current$nbSteps){
        if (i == 1) {
          ll[[i]] <- div(id = ns(paste0("screen",i)), pages()@ll.UI[[i]])
        } else {
          ll[[i]] <- shinyjs::hidden(div(id = ns(paste0("screen",i)), pages()@ll.UI[[i]]))
        }
      }
      
      
      tagList(ll)
    })
    
  })
  
  
  
  observe({
    current$val
    
    if (current$val < current$nbSteps) {
      shinyjs::enable('nextBtn')
    } else {shinyjs::disable('nextBtn')}
    
    if (current$val == 1) {
      shinyjs::disable('prevBtn')
    } else {shinyjs::enable('prevBtn')}
    
    shinyjs::hide(selector = ".page")
    
  })
  
  observeEvent(current$val,{
    
    for (i in 1:current$nbSteps){
      shinyjs::toggle(id = paste0("screen", i), condition = current$val == i)
    }
  })
  
  
  
  
  # this return is used when one wants to dissociate buttons and screens
  # return(reactive(list(bars=bars(),
  #                 screens=pages())))
}


####################"
## definition d'une classe pour la structure utilisee par le module de navigation
#' @exportClass NavStructure
.NavStructure <- setClass("NavStructure",
                          slots= list(
                            name = "character",
                            stepsNames = "character",
                            isMandatory = "logical",
                            ll.UI = "list",
                            #rstFunc = "ANY",
                            isDone = "logical",
                            forceReset = "logical",
                            iconType = 'character'
                          )
                          
)


#' @export NavStructure
NavStructure <- function(
  name = character(),
  stepsNames = character(),
  isMandatory = logical(),
  ll.UI = list(),
  #rstFunc = NULL,
  isDone = logical(),
  forceReset = logical(),
  iconType = character()
)
{
  obj <- new("NavStructure",
             name = name,
             stepsNames = stepsNames,
             isMandatory = isMandatory,
             ll.UI = ll.UI,
             #rstFunc = rstFunc,
             isDone = isDone,
             forceReset =forceReset,
             iconType = iconType
  )
  #validObject(obj)
  
  obj
}







####################
progressWizard_CSS <- ".flexer,.progress-indicator{
  display:-webkit-box;
  display:-moz-box;
  display:-ms-flexbox;
  display:-webkit-flex;
  display:flex
}

.no-flexer,.progress-indicator.stacked{
  display:block
}

.no-flexer-element{
  -ms-flex:0;
  -webkit-flex:0;
  -moz-flex:0;
  flex:0;
}

.flexer-element,.progress-indicator>li{
  -ms-flex:1;
  -webkit-flex:1;
  -moz-flex:1;
  flex:1
}

.progress-indicator{
  margin:0 0 1em;
  padding:0;
  font-size:100%;
}

.progress-indicator>li{
  list-style:none;
  text-align:center;
  width:auto;
  padding:0;
  margin:0;
  position:relative;
  text-overflow:ellipsis;
  color:#bbb;
  display:block
}

.progress-indicator>li:hover{
  color:#6f6f6f
}

.progress-indicator>li.completed,.progress-indicator>li.completed .bubble{
  color:#65d074
}

.progress-indicator>li .bubble{
  border-radius:1000px;
  width:40px;
  height:40px;
  background-color:#bbb;
  display:block;
  margin:0 auto .5em;
  border-bottom:1px solid #888
}

.progress-indicator>li .bubble:after,.progress-indicator>li .bubble:before{
  display:block;
  position:absolute;
  top:19px;
  width:100%;
  height:3px;
  content:'';
  background-color:#bbb
}

.progress-indicator>li.completed .bubble,.progress-indicator>li.completed .bubble:after,.progress-indicator>li.completed .bubble:before{
  background-color:#65d074;
  border-color:#247830
}

.progress-indicator>li .bubble:before{
  left:0
}

.progress-indicator>li .bubble:after{
  right:0
}

.progress-indicator>li:first-child .bubble:after,.progress-indicator>li:first-child .bubble:before{
  width:50%;
  margin-left:50%
}

.progress-indicator>li:last-child .bubble:after,.progress-indicator>li:last-child .bubble:before{
  width:50%;
  margin-right:50%
}

.progress-indicator>li.active,.progress-indicator>li.active .bubble{
  color:#337AB7
}

.progress-indicator>li.active .bubble,.progress-indicator>li.active .bubble:after,.progress-indicator>li.active .bubble:before{
  background-color:#337AB7;
  border-color:#122a3f
}

.progress-indicator>li a:hover .bubble,.progress-indicator>li a:hover .bubble:after,.progress-indicator>li a:hover .bubble:before{
  background-color:#5671d0;
  border-color:#1f306e
}

.progress-indicator>li a:hover .bubble{
  color:#5671d0
}

.progress-indicator>li.danger .bubble,.progress-indicator>li.danger .bubble:after,.progress-indicator>li.danger .bubble:before{
  background-color:#d3140f;
  border-color:#440605
}

.progress-indicator>li.danger .bubble{
  color:#d3140f
}

.progress-indicator>li.warning .bubble,.progress-indicator>li.warning .bubble:after,.progress-indicator>li.warning .bubble:before
{
  background-color:#edb10a;
  border-color:#5a4304
}

.progress-indicator>li.warning .bubble{
  color:#edb10a
}

.progress-indicator>li.info .bubble,.progress-indicator>li.info .bubble:after,.progress-indicator>li.info .bubble:before{
  background-color:#5b32d6;
  border-color:#25135d
}

.progress-indicator>li.info .bubble{
  color:#5b32d6
}

.progress-indicator.stacked>li{
  text-indent:-10px;
  text-align:center;
  display:block
}

.progress-indicator.stacked>li .bubble:after,.progress-indicator.stacked>li .bubble:before{
  left:50%;
  margin-left:-1.5px;
  width:3px;
  height:100%
}

.progress-indicator.stacked .stacked-text{
  position:relative;
  z-index:10;
  top:0;
  margin-left:60%!important;
  width:45%!important;
  display:inline-block;
  text-align:left;
  line-height:1.2em
}

.progress-indicator.stacked>li a{
    border:none}.progress-indicator.stacked.nocenter>li .bubble{
      margin-left:0;
      margin-right:0
}

.progress-indicator.stacked.nocenter>li .bubble:after,.progress-indicator.stacked.nocenter>li .bubble:before{
  left:10px
}

.progress-indicator.stacked.nocenter .stacked-text{
  width:auto!important;
  display:block;
  margin-left:40px!important
}

@media handheld,screen and (max-width:400px){
  .progress-indicator{
    font-size:60%
  }
}


/* A totally custom override */
    .progress-indicator.custom-complex {
        /*background-color: #f1f1f1;
        border: 1px solid #ddd;
        border-radius: 10px;*/
        padding: 10px 5px;
        
    }
    .progress-indicator.custom-complex > li .bubble {
        height: 24px;
        width: 99%;
        border-radius: 2px;
        box-shadow: inset -5px 0 12px rgba(0, 0, 0, 0.2);
    }
    .progress-indicator.custom-complex > li .bubble:before,
    .progress-indicator.custom-complex > li .bubble:after {
        display: none;
    }"
    
## To be copied in the UI
# mod_navigation_ui("navigation_ui_1")
    
## To be copied in the server
# callModule(mod_navigation_server, "navigation_ui_1")
 
