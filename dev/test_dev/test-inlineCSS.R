library(shiny)

# Method 1 - passing a string of valid CSS

## code inspire de  : https://codepen.io/cjl750/pen/mXbMyo

code_sass <-"$numDots:3;
$parentWidthBase: 0.8;
$parentWidth: $parentWidthBase * 100vw;
$parentMaxWidth: 1000px;
$dotWidth: 25px;
$dotWidthSm: 17px;
$active: #2C3E50;
$inactive: #AEB6BF;



html, body{
	height: 100%;
}

body{
	font-family: 'Quicksand', sans-serif;
	font-weight: 500;
	color: #424949;
	background-color: #ECF0F1;
	padding: 0 25px;
	display: flex;
	flex-direction: column;
	position: relative;
}

h1{
	text-align: center;
	height: 38px;
	margin: 60px 0;
	
	span{
		white-space: nowrap;
	}
}
		
		
		
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
	height: 100px;
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
			background-color: $inactive;
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


  ui = fluidPage(
    uiOutput("updateCssCode"),
    selectInput('nSteps', 'n', choices=1:20, selected=5),
    uiOutput("toto")
  )
  
  
  
  server = function(input, output) {
    
    output$updateCssCode <- renderUI({
      input$nSteps
      code <- strsplit(code_sass,"\n")
      firstLine <- code[[1]][1]
      prefix <- substr(firstLine,1,unlist(gregexpr(pattern =':',firstLine)))
      suffix <- substr(firstLine,unlist(gregexpr(pattern =';',firstLine)), nchar(firstLine))
      
      code[[1]][1] <- paste0(prefix, input$nSteps, suffix, collapse='')
      print( code[[1]][1])
      code_sass <- paste(unlist(code), collapse = '')
      inlineCSS( sass(code_sass))
    })
    
    output$toto <- renderUI({
      
      txt <- "<div class='flex-parent'> <div class='input-flex-container'>"
      for (i in 1:input$nSteps){
        txt <- paste0(txt,"<div class='input active'><span name='1910'></span>	</div>")
      }
      txt <- paste0(txt,"</div></div>")
      HTML(txt)
    })
  }




shinyApp(ui, server)