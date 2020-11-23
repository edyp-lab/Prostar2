library(shiny)
library(shinyWidgets)
library(shinyjs)
setwd("~/TELETRAVAIL/github_DAPARforFeatures/Prostar2/dev/prostar_NvCSS/")


ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style_tl.css")
  ),
  
  tags$div(class="content",
           p('Filtration'),
           p('Normalization'),
           p('Imputation'),
           p('Aggregation')
  )
  
  
)


server <- function(input, output){
  
  
  
}


shinyApp(ui, server)
