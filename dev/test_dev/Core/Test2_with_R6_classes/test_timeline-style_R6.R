library(R6)

btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"

options(shiny.fullstacktrace = T)
timeline <- TimelineStyle$new('timeline')

ui <- fluidPage(
  timeline$ui()
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  source(file.path('.', 'timeline_Style_R6.R'), local=TRUE)$value
  
  
  #output$timeline <- renderUI({timeline$ui()})
  
  timeline <- TimelineStyle$new('timeline')
  timeline$call(style=2,
                steps = list(Description=T,
                             Step1 = F,
                             Step2 = T),
                status = c(Description=  1,
                              Step1 = -1,
                              Step2 = 1),
                pos = 2
                )
  
  
}


shinyApp(ui, server)
