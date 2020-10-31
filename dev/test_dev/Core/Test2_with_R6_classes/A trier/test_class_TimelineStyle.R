library(R6)

source(file.path('.', 'class_TimelineStyle.R'), local=TRUE)$value

btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"

options(shiny.fullstacktrace = T)
timeline <- TimelineStyle$new('timeline',style=2)

ui <- fluidPage(
  tagList(
    timeline$ui(),
    actionButton('btn', 'Simulate change in status'),
    actionButton('pos_btn', 'Simulate change in positin')
  )
  #uiOutput('timeline')
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  status <- reactiveVal(c(Description=  1, Step1 = -1, Step2 = 1)  )
  pos <- reactiveVal(2)
  
  output$timeline <- renderUI({timeline$ui()})
  
  timeline$server(steps = list(Description=T, Step1 = F, Step2 = T),
                  status = status,
                  pos =  pos
                  )

  observeEvent(input$btn,{ 
    tmp <- status()
    tmp[2] <- input$btn %% 2
    status(tmp)
    })
  
  observeEvent(input$pos_btn,{ 
    pos(input$pos_btn %% 2)
  })
}


shinyApp(ui, server)
