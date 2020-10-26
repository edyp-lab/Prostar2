library(R6)

source(file.path('.', 'class_TimelineStyle.R'), local=TRUE)$value

btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"

options(shiny.fullstacktrace = T)
timeline <- TimelineStyle$new('timeline',style=2)

ui <- fluidPage(
  tagList(
    timeline$ui(),
    actionButton('btn', 'Simulate change in status')
  )
  #uiOutput('timeline')
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  status <- reactiveVal(
 c(Description=  1, Step1 = -1, Step2 = 1)
    )
  
  steps <- list(Description=T, Step1 = F, Step2 = T)
  pos <- reactiveVal(2)
  output$timeline <- renderUI({timeline$ui()})
  
  timeline$server(steps = steps,
                  status = status,
                pos = pos
                )

  observeEvent(input$btn,{ 
    tmp <- status()
    tmp[2] <- input$btn %% 2
    status(tmp)
    })
}


shinyApp(ui, server)
