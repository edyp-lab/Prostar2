library(shiny)
library(shinyWidgets)
library(shinyjs)
setwd("~/TELETRAVAIL/github_DAPARforFeatures/Prostar2/dev/prostar_NvCSS/")


ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style_tl.css")
  ),
  
  # first and third
  # tags$div(class="box",
  #          p('Filtration'),
  #          p('Normalization'),
  #          p('Imputation'),
  #          p('Aggregation'))
  
  
  # second
  # tags$ul(class="timeline",
  #         tags$li(class="event",
  #                 p('Filtration')),
  #         tags$li(class="event",
  #                 p('Normalization')),
  #         tags$li(class="event",
  #                 p('Imputation')),
  #         tags$li(class="event",
  #                 p('Aggregation')
  #         ))
  
 
  tags$ul(class="timeline",
                  tags$li(class="event",
                          p('Filtration')),
                  tags$li(class="event",
                          p('Normalization')),
                  tags$li(class="event",
                          p('Imputation')),
                  tags$li(class="event",
                          p('Aggregation')
                  ))
  
  
  
)


server <- function(input, output){
  
  
  
}


shinyApp(ui, server)
