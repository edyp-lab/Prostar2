library(shiny)
library(shinyWidgets)
library(shinyjs)
setwd("~/TELETRAVAIL/github_DAPARforFeatures/Prostar2/dev/prostar_NvCSS/")
#setwd("~/Github/AdaptedForFeatures/Prostar2/dev/prostar_NvCSS/")


ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style_tl5.css")
  ),
  
  # title = tagList(
  #   tags$span(
  #     class = "logo-mini", style =  "font-size : 14px","Prostar"),
  #   tags$span(
  #     class = "logo-lg", "Prostar")
  # )
  
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
  
)


server <- function(input, output){
  
  
  
}


shinyApp(ui, server)
