library(shiny)
library(bootstraplib)
bs_theme_new(bootswatch = "sketchy")
bs_theme_add_variables(`body-bg` = "#2CEC11")
bs_theme_add_variables(`body-bg` = "#2CEC11", `body-color` = "#292123")
bs_theme_add_variables(`body-bg` = "#2CEC11", `body-color` = "#F61D49")
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  bootstrap(),
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))



#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(bootstraplib)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  # uncomment this when you want live previewing / editing of this theme    
  #bootstraplib::bs_themer()
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
})

shinyApp(shinyUI, shinyServer)