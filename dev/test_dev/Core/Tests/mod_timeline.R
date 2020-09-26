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
    uiOutput(ns("load_css_style")),
    shinyjs::useShinyjs(),
    fluidRow(
      align= 'center',
      column(width=2,
             div(style = btn_style,
               actionButton(ns("rstBtn"), "reset",
                            class = redBtnClass,
                            style='padding:4px; font-size:80%')
             ),
             div( style = btn_style,
                  shinyjs::disabled(actionButton(ns("prevBtn"), "<<",
                                                 class = PrevNextBtnClass,
                                                 style='padding:4px; font-size:80%')))
      ),
      column(width=8,div( style = btn_style,
                          uiOutput(ns("timelineStyle")))
      ),
      column(width=2,div(style=btn_style,
                         actionButton(ns("nextBtn"), ">>",
                                      class = PrevNextBtnClass,
                                      style='padding:4px; font-size:80%')
      )
      )
    )
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
mod_timeline_server <- function(id, style=1, config, actions){
  stopifnot(!is.reactive(style))
  stopifnot(!is.reactive(config))
  
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$timelineStyle <- renderUI({ uiOutput(ns(paste0('timeline', style))) })
    
    observeEvent(actions,{ 
      print(paste0('action :', unlist(actions)))
      shinyjs::toggleState('nextBtn', condition=actions$nxt)
      shinyjs::toggleState('rstBtn', condition=actions$rst)
      shinyjs::toggleState('prevBtn', condition=actions$prv) 
   })
    
    ## Functions for timeline and styles
    
    output$load_css_style <- renderUI({
      req(length(config$stepsNames))
      style
      if (style==3) return(NULL)
      code <- strsplit(code_sass_timeline[[paste0('style',style)]],"\n")
      firstLine <- code[[1]][1]
      prefix <- substr(firstLine,1,unlist(gregexpr(pattern =':',firstLine)))
      suffix <- substr(firstLine,unlist(gregexpr(pattern =';',firstLine)), nchar(firstLine))
      
      code[[1]][1] <- paste0(prefix, length(config$stepsNames), suffix, collapse='')
      
      shinyjs::inlineCSS( sass::sass(paste(unlist(code), collapse = '')))
      
    })
    
    
    #### -----
    ### Three timelines
    output$timeline1 <- renderUI({
      config$current.pos
      status <- rep('',length(config$stepsNames))
      status[config$current.pos] <- ' active'
      steps <- config$stepsNames
      txt <- "<div class='flex-parent'> <div class='input-flex-container'>"
      for (i in 1:length(config$stepsNames)){
        txt <- paste0(txt, "<div class='input",status[i], "'><span name='", steps[i],"'></span>  </div>")
      }
      txt <- paste0(txt,"</div></div>")
      HTML(txt)
    })
    
    
    output$timeline2 <- renderUI({
      config$current.pos
      config
      status <- rep('', length(config$stepsNames))
      if( !is.null(config$mandatory))
        status[which(config$mandatory)] <- 'mandatory'
      
      #status <- rep('',length(config$stepsNames))
      status[which(config$isDone)] <- 'complete'
      
      active  <- rep('', length(config$stepsNames))
      active[config$current.pos] <- 'active'
      
      steps <- config$stepsNames
      txt <- "<ul class='timeline' id='timeline'>"
      for (i in 1:length(config$stepsNames)){
        txt <- paste0(txt, "<li class='li ",status[i]," ",active[i],"'><div class='timestamp'></div><div class='status'><h4>", steps[i],"</h4></div></li>")
      }
      txt <- paste0(txt,"</ul>")
      
      HTML(txt)
    })
    
    
    output$timeline3 <- renderUI({
      config$current.pos
      color <- rep("lightgrey", length(config$stepsNames))
      colorForCursor <- rep("white", length(config$stepsNames))
      
      
      for (i in 1:length(config$stepsNames)){
        status <- config$isDone[i]
        col <- ifelse(!is.null(config$mandatory) && config$mandatory[i], "red", orangeProstar)
        ifelse(status, color[i] <- "green", color[i] <- col)
      }
      
      colorForCursor[config$current.pos] <- "black"
      
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
    
    
    # return value of the actions
    list(rstBtn = reactive(input$rstBtn),
                   nextBtn = reactive(input$nextBtn),
                   prevBtn = reactive(input$prevBtn)
         )
    
  })
}




code_sass_timeline <- list(
  style1 ="$numDots:3;
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
",
style2 = "$test: test;
$colCompleted: #07910A;
$colMandatory: #D30505;
$colDefault: #B3AFAB;
$radius: 20px;
$lineWidth: 5px;
.timeline{
  list-style-type: none;
  display: flex;
  align-items: center;
  justify-content: center;
}
.li{
  transition: all 200ms ease-in;
}
.timestamp{
  margin-bottom: 20px;
  padding: 0px 40px;
  display: flex;
  flex-direction: column;
  align-items: center;
  font-weight: 100;
}
.status {
  padding: 0px 40px;
  display: flex;
  justify-content: center;
  border-top: $lineWidth solid $colDefault;
  position: relative;
  transition: all 200ms ease-in;
  h4{
    font-weight: 600;
    border-bottom: 3px solid white;
  }
  
  &:before{
    content: '';
    width: $radius;
    height: $radius;
    background-color: white;
    border-radius: 55px;
    border: $lineWidth solid $colDefault;
    position: absolute;
    top: -12px;
    left: 50%;
    transition: all 200ms ease-in;
  }
}
.li.complete{
  .status{
    border-top: $lineWidth solid $colDefault;
    &:before{
      background-color: $colCompleted;
      border: $lineWidth solid $colCompleted;
      transition: all 200ms ease-in;
      }
    h4{
      color: $colCompleted;
      border-bottom: 3px solid white;
      }
    }
}
.li.mandatory{
  .status{
    border-top: $lineWidth solid $colDefault;
    &:before{
      background-color: white;
      border: $lineWidth solid $colMandatory;
      transition: all 200ms ease-in;
      }
    h4{
      color: $colMandatory;
      border-bottom: 3px solid white;
    }
  }
}
           
.li.active{
  .status{
    h4{
      border-bottom: 3px solid currentColor;
    }
  }
}
.li.complete.active{
  .status{
    h4{
      border-bottom: 3px solid currentColor ;
    }
  }
}
      
.li.mandatory.active{
  .status{
    h4{
      border-bottom: 3px solid currentColor;
    }
  }
}", 
style3 = "")