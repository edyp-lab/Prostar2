library(shiny)
library(shinyWidgets)
library(shinyjs)
setwd("~/TELETRAVAIL/github_DAPARforFeatures/Prostar2/dev/prostar_NvCSS/")
#setwd("~/Github/AdaptedForFeatures/Prostar2/dev/prostar_NvCSS/")


ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style_tl4.css")
  ),
  
  
  tags$div(class="box",
           tags$div(class="sub_box",
                    p('Filtration')
           ),
           tags$div(class="sub_box",
                    p('Normalization')
           ),
           tags$div(class="sub_box",
                    p('Imputation')
           ),
           tags$div(class="sub_box",
                    p('Aggregation')
           )
  )
  # tags$div(class="box",
  #          tags$div(class="sub_box",
  #                   p('Filt.')
  #          ),
  #          tags$div(class="sub_box",
  #                   p('Norm.')
  #          ),
  #          tags$div(class="sub_box",
  #                   p('Imp.')
  #          ),
  #          tags$div(class="sub_box",
  #                   p('Aggr.')
  #          )
  # )
  
)


server <- function(input, output){
  
  
  
}


shinyApp(ui, server)
