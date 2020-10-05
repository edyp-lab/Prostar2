library(highcharter)
library(DAPAR2)
library(shiny)
library(DAPARdata2)


source(file.path("../../R/Plots","mod_plots_corr_matrix.R"), local=TRUE)$value
source(file.path("../../R","global.R"), local=TRUE)$value


ui <- fluidPage(
  tagList(
    uiOutput('choose_data_ui'),
    mod_plots_corr_matrix_ui('plots_corr_matrix')
  )
)


server <- function(input, output, session) {
  
  rv <- reactiveValues(
    current.obj = NULL
  )
  
  output$choose_data_ui <- renderUI({
    selectInput('choose_data', "Dataset", choices = utils::data(package="DAPARdata2")$results[,"Item"]
    )
  })
  
  
  observeEvent(input$choose_data,{
    rv$current.obj <- BiocGenerics::get(input$choose_data)
    print(rv$current.obj)
  })
  
  mod_plots_corr_matrix_server('plots_corr_matrix', 
                               obj = reactive({rv$current.obj[[length(names(rv$current.obj))]]}),
                               names = reactive({NULL}),
                               gradientRate = reactive({0.8}))
}


shinyApp(ui=ui, server=server)