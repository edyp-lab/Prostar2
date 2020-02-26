
code_sass_timeline_style1 <-"$numDots:3;
$parentWidthBase: 0.4;
$parentWidth: $parentWidthBase * 100vw;
$parentMaxWidth: 800px;
$dotWidth: 25px;
$dotWidthSm: 17px;
$active: #2C3E50;
$inactive: #AEB6BF;



.flex-parent{
	display: flex;
	flex-direction: column;
	justify-content: center;
	align-items: center;
	width: 100%;
	height: 100%;
	}
	
.input-flex-container{
	display: flex;
	justify-content: space-around;
	align-items: center;
	width: $parentWidth;
	height: 20px;
	max-width: $parentMaxWidth;
	position: relative;
	z-index: 0;
}


.input{
	width: $dotWidth;
	height: $dotWidth;
	background-color: $active;
	position: relative;
	border-radius: 50%;

	&::before, &::after{
		content: '';
		display: block;
		position: absolute;
		z-index: -1;
		top: 50%;
		transform: translateY(-50%);
		background-color: $active;
		width: $parentWidth / $numDots;
		height: 5px;
		max-width: $parentMaxWidth / $numDots;
	}
	
	&::before{
		left: calc(#{-$parentWidth / $numDots} + #{$dotWidth / 2});
	}
	
	&::after{
		right: calc(#{-$parentWidth / $numDots} + #{$dotWidth / 2});
		}
		
	&.active{
		background-color: $active;
	
		&::before{
			background-color: $active;
		}
		
		&::after{
			background-color: $active;
		}
	
		span{
			font-weight: 700;
			
			&::before{
				font-size: 13px;
			}
			
			&::after {
				font-size: 15px;
			}
		}
	
		&.active ~ .input{
		&, &::before, &::after{
			background-color: $inactive;
		}
		}
	}
	
	span{
		width: 1px;
		height: 1px;
		position: absolute;
		top: 50%;
		left: 50%;
		transform: translate(-50%, -50%);
		visibility: hidden;
		
		&::before, &::after {
			visibility: visible;
			position: absolute;
			left: 50%;
		}
		
		&::after {
			content: attr(name);
			top: 25px;
			transform: translateX(-50%);
			font-size: 14px;
		}
		
		&::before {
      content: attr(data-info);
      top: -65px;
      width: 70px;
      transform: translateX(-5px) rotateZ(-45deg);
      font-size: 12px;
      text-indent: -10px;
		}
    

		}
	}

.description-flex-container {
  width: 80vw;
  font-weight: 400;
  font-size: 22px;
  margin-top: 100px;
  max-width: 1000px;

  p {
    margin-top: 0;
    display: none;

    &.active {
      display: block;
    }
  }
}

	
@media (min-width: $parentMaxWidth / $parentWidthBase){
	.input{
	&::before{
		left: #{-($parentMaxWidth / $numDots) + ($dotWidth / 2)};
	}
	
	&::after{
		right: #{-($parentMaxWidth / $numDots) + ($dotWidth / 2)};
	}
	}
}

@media (max-width: 850px){
	.input{
		width: $dotWidthSm;
		height: $dotWidthSm;
	
	
		&::before, &::after{
			height: 3px;
		}
		
		&::before{
			left: calc(#{-$parentWidth / $numDots} + #{$dotWidthSm / 2});
		}
		
		&::after{
			right: calc(#{-$parentWidth / $numDots} + #{$dotWidthSm / 2});
}
}
}
"





code_sass_timeline_style2 <- ""

code_sass_timeline_style3  <- ""


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
mod_navigation_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    uiOutput(ns("load_css_style")),
    fluidRow(
      align= 'center',
      column(width=2,
             div( style="display:inline-block; vertical-align: middle; padding: 7px",
                  actionButton(ns("rstBtn"), "reset",
                                                 class = PrevNextBtnClass,
                                                 style='padding:4px; font-size:80%')),
             div( id='test',style="display:inline-block; vertical-align: middle; padding: 7px",
                  actionButton(ns("prevBtn"), "<<",
                                                 class = PrevNextBtnClass,
                                                 style='padding:4px; font-size:80%'))
            ),
      column(width=8,div( style="display:inline-block; vertical-align: middle; padding: 7px",
                          uiOutput(ns("timelineStyle")))
      ),
      column(width=2,div(style="display:inline-block; vertical-align: middle; padding: 7px",
                         shinyjs::disabled(actionButton(ns("nextBtn"), ">>",
                                      class = PrevNextBtnClass,
                                      style='padding:4px; font-size:80%')))
      )
    ),
    hr(),
    br(),br(),br(),
    uiOutput(ns("screens"))
    
    
  )
}
    
# Module Server
    
#' @rdname mod_navigation
#' @export
#' @keywords internal
#' @import shiny shinyjs
#' @importFrom sass sass
    
mod_navigation_server <- function(input, output, session, style=1, pages){
  ns <- session$ns
  
  current <- reactiveValues(
    # This variable is the indice of the current screen
    val = NULL,
    nbSteps = NULL
  )
  
  
  
  output$load_css_style <- renderUI({ uiOutput(ns(paste0('css_style', style)))  })
  
  output$css_style1 <- renderUI({
    req(current$nbSteps)
    
    code <- strsplit(code_sass_timeline_style1,"\n")
    firstLine <- code[[1]][1]
    prefix <- substr(firstLine,1,unlist(gregexpr(pattern =':',firstLine)))
    suffix <- substr(firstLine,unlist(gregexpr(pattern =';',firstLine)), nchar(firstLine))
    
    code[[1]][1] <- paste0(prefix, current$nbSteps, suffix, collapse='')
    code_sass_timeline_style1 <- paste(unlist(code), collapse = '')
    shinyjs::inlineCSS( sass::sass(code_sass_timeline_style1))
  })
  
  
  observeEvent(req(pages),{
    current$nbSteps <- length(pages$stepsNames)
    current$val <- 1
    
    pages$ll.UI[[1]] <- div(id = ns(paste0("screen",1)),  pages$ll.UI[[1]])
    for (i in 2:current$nbSteps){
      pages$ll.UI[[i]] <- shinyjs::hidden(div(id = ns(paste0("screen",i)),  pages$ll.UI[[i]]))
    }
    
    
    current$val <- 1
    
  })

  
  output$timelineStyle <- renderUI({ uiOutput(ns(paste0('timeline', style))) })
  
  #### -----
  ### Three timelines
  output$timeline1 <- renderUI({
    current$val
    status <- rep('',current$nbSteps)
    status[current$val] <- ' active'
    steps <- pages$stepsNames
    txt <- "<div class='flex-parent'> <div class='input-flex-container'>"
    for (i in 1:current$nbSteps){
      txt <- paste0(txt, "<div class='input",status[i], "'><span name='", steps[i],"'></span>  </div>")
     }
    txt <- paste0(txt,"</div></div>")
    HTML(txt)
  })
  
  
  observeEvent(req(input$rstBtn),{
    current$val <- 1
    pages$isDone <- rep(FALSE, current$nbSteps)
    pages$reset <- TRUE
  })
  
  
  navPage <- function(direction) {
    newval <- current$val + direction 
    newval <- max(1, newval)
    newval <- min(newval, current$nbSteps)
   current$val <- newval
  }
  
  
  observeEvent(input$prevBtn,ignoreInit = TRUE,{navPage(-1)})
  observeEvent(input$nextBtn,ignoreInit = TRUE,{navPage(1)})
  
  observeEvent( pages$isDone[current$val],{
    shinyjs::toggleState(id = "nextBtn", condition = isTRUE(pages$isDone[current$val]))
    shinyjs::toggle(id = "nextBtn", condition = current$val< current$nbSteps)
    shinyjs::toggle(id = "prevBtn", condition = current$val > 1)
  })
  
  observeEvent(current$val, {
    lapply(1:current$nbSteps, function(x){shinyjs::toggle(paste0('screen', x), condition = x==current$val)})
    })

  output$screens <- renderUI({
    tagList(pages$ll.UI)
  })


}




#' 
#' 
#' 
#' 
#' timeline_css <- "$red: #e74c3c
#' $blue : #2980b9
#' $midnight : #2c3e50
#' $green: #7b3
#' 
#' .timeline
#'   list-style-type: none
#'   display: flex
#'   align-items: center
#'   justify-content: center
#' .li
#'   transition: all 200ms ease-in
#' 
#' .timestamp
#'   margin-bottom: 20px
#'   padding: 0px 40px
#'   display: flex
#'   flex-direction: column
#'   align-items: center
#'   font-weight: 100
#' .status
#'   padding: 0px 40px
#'   display: flex
#'   justify-content: center
#'   border-top: 2px solid #D6DCE0
#'   position: relative
#'   transition: all 200ms ease-in  
#'   h4
#'     font-weight: 600
#'   &:before
#'     content: ''
#'     width: 25px
#'     height: 25px
#'     background-color: white
#'     border-radius: 25px
#'     border: 1px solid #ddd
#'     position: absolute
#'     top: -15px
#'     left: 42%
#'     transition: all 200ms ease-in 
#'   
#'  
#' .li.complete
#'   .status
#'     border-top: 2px solid $midnight
#'     &:before
#'       background-color: $midnight
#'       border: none
#'       transition: all 200ms ease-in 
#'     h4
#'       color: $midnight
#'       
#'       
#' .li.complete
#'   .status
#'     border-top: 2px solid $green
#'     &:before
#'       background-color: $green
#'       border: none
#'       transition: all 200ms ease-in 
#'     h4
#'       color: $green
#'       
#'       
#' .li.needed
#'   .status
#'     border-top: 2px solid $red
#'     &:before
#'       background-color: $red
#'       border: none
#'       transition: all 200ms ease-in 
#'     h4
#'       color: $red
#'       
#' .li.current
#'   .status
#'     border-top: 2px solid $blue
#'     &:before
#'       background-color: $blue
#'       border: none
#'       transition: all 200ms ease-in 
#'     h4
#'       color: $blue
#'       
#'       
#' @media (min-device-width: 320px) and (max-device-width: 700px)
#'   .timeline
#'     list-style-type: none
#'     display: flex
#'   .li
#'     transition: all 200ms ease-in
#'     display: flex
#'     width: inherit
#'   .timestamp
#'     width: 100px
#'   .status
#'     &:before
#'       left: -8%
#'       top: 30%
#'       transition: all 200ms ease-in "
#' 
#' 
#' ####################
#' progressWizard_CSS <- ".flexer,.progress-indicator{
#'   display:-webkit-box;
#'   display:-moz-box;
#'   display:-ms-flexbox;
#'   display:-webkit-flex;
#'   display:flex
#' }
#' 
#' .no-flexer,.progress-indicator.stacked{
#'   display:block
#' }
#' 
#' .no-flexer-element{
#'   -ms-flex:0;
#'   -webkit-flex:0;
#'   -moz-flex:0;
#'   flex:0;
#' }
#' 
#' .flexer-element,.progress-indicator>li{
#'   -ms-flex:1;
#'   -webkit-flex:1;
#'   -moz-flex:1;
#'   flex:1
#' }
#' 
#' .progress-indicator{
#'   margin:0 0 1em;
#'   padding:0;
#'   font-size:100%;
#' }
#' 
#' .progress-indicator>li{
#'   list-style:none;
#'   text-align:center;
#'   width:auto;
#'   padding:0;
#'   margin:0;
#'   position:relative;
#'   text-overflow:ellipsis;
#'   color:#bbb;
#'   display:block
#' }
#' 
#' .progress-indicator>li:hover{
#'   color:#6f6f6f
#' }
#' 
#' .progress-indicator>li.completed,.progress-indicator>li.completed .bubble{
#'   color:#65d074
#' }
#' 
#' .progress-indicator>li .bubble{
#'   border-radius:1000px;
#'   width:40px;
#'   height:40px;
#'   background-color:#bbb;
#'   display:block;
#'   margin:0 auto .5em;
#'   border-bottom:1px solid #888
#' }
#' 
#' .progress-indicator>li .bubble:after,.progress-indicator>li .bubble:before{
#'   display:block;
#'   position:absolute;
#'   top:19px;
#'   width:100%;
#'   height:3px;
#'   content:'';
#'   background-color:#bbb
#' }
#' 
#' .progress-indicator>li.completed .bubble,.progress-indicator>li.completed .bubble:after,.progress-indicator>li.completed .bubble:before{
#'   background-color:#65d074;
#'   border-color:#247830
#' }
#' 
#' .progress-indicator>li .bubble:before{
#'   left:0
#' }
#' 
#' .progress-indicator>li .bubble:after{
#'   right:0
#' }
#' 
#' .progress-indicator>li:first-child .bubble:after,.progress-indicator>li:first-child .bubble:before{
#'   width:50%;
#'   margin-left:50%
#' }
#' 
#' .progress-indicator>li:last-child .bubble:after,.progress-indicator>li:last-child .bubble:before{
#'   width:50%;
#'   margin-right:50%
#' }
#' 
#' .progress-indicator>li.active,.progress-indicator>li.active .bubble{
#'   color:#337AB7
#' }
#' 
#' .progress-indicator>li.active .bubble,.progress-indicator>li.active .bubble:after,.progress-indicator>li.active .bubble:before{
#'   background-color:#337AB7;
#'   border-color:#122a3f
#' }
#' 
#' .progress-indicator>li a:hover .bubble,.progress-indicator>li a:hover .bubble:after,.progress-indicator>li a:hover .bubble:before{
#'   background-color:#5671d0;
#'   border-color:#1f306e
#' }
#' 
#' .progress-indicator>li a:hover .bubble{
#'   color:#5671d0
#' }
#' 
#' .progress-indicator>li.danger .bubble,.progress-indicator>li.danger .bubble:after,.progress-indicator>li.danger .bubble:before{
#'   background-color:#d3140f;
#'   border-color:#440605
#' }
#' 
#' .progress-indicator>li.danger .bubble{
#'   color:#d3140f
#' }
#' 
#' .progress-indicator>li.warning .bubble,.progress-indicator>li.warning .bubble:after,.progress-indicator>li.warning .bubble:before
#' {
#'   background-color:#edb10a;
#'   border-color:#5a4304
#' }
#' 
#' .progress-indicator>li.warning .bubble{
#'   color:#edb10a
#' }
#' 
#' .progress-indicator>li.info .bubble,.progress-indicator>li.info .bubble:after,.progress-indicator>li.info .bubble:before{
#'   background-color:#5b32d6;
#'   border-color:#25135d
#' }
#' 
#' .progress-indicator>li.info .bubble{
#'   color:#5b32d6
#' }
#' 
#' .progress-indicator.stacked>li{
#'   text-indent:-10px;
#'   text-align:center;
#'   display:block
#' }
#' 
#' .progress-indicator.stacked>li .bubble:after,.progress-indicator.stacked>li .bubble:before{
#'   left:50%;
#'   margin-left:-1.5px;
#'   width:3px;
#'   height:100%
#' }
#' 
#' .progress-indicator.stacked .stacked-text{
#'   position:relative;
#'   z-index:10;
#'   top:0;
#'   margin-left:60%!important;
#'   width:45%!important;
#'   display:inline-block;
#'   text-align:left;
#'   line-height:1.2em
#' }
#' 
#' .progress-indicator.stacked>li a{
#'     border:none}.progress-indicator.stacked.nocenter>li .bubble{
#'       margin-left:0;
#'       margin-right:0
#' }
#' 
#' .progress-indicator.stacked.nocenter>li .bubble:after,.progress-indicator.stacked.nocenter>li .bubble:before{
#'   left:10px
#' }
#' 
#' .progress-indicator.stacked.nocenter .stacked-text{
#'   width:auto!important;
#'   display:block;
#'   margin-left:40px!important
#' }
#' 
#' @media handheld,screen and (max-width:400px){
#'   .progress-indicator{
#'     font-size:60%
#'   }
#' }
#' 
#' 
#' /* A totally custom override */
#'     .progress-indicator.custom-complex {
#'         /*background-color: #f1f1f1;
#'         border: 1px solid #ddd;
#'         border-radius: 10px;*/
#'         padding: 10px 5px;
#'         
#'     }
#'     .progress-indicator.custom-complex > li .bubble {
#'         height: 24px;
#'         width: 99%;
#'         border-radius: 2px;
#'         box-shadow: inset -5px 0 12px rgba(0, 0, 0, 0.2);
#'     }
#'     .progress-indicator.custom-complex > li .bubble:before,
#'     .progress-indicator.custom-complex > li .bubble:after {
#'         display: none;
#'     }"
    
## To be copied in the UI
# mod_navigation_ui("navigation_ui_1")
    
## To be copied in the server
# callModule(mod_navigation_server, "navigation_ui_1")
 
