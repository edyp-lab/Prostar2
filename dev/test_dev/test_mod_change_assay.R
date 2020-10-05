library(shiny)

source(file.path('../../R', 'mod_change_assay.R'), local=TRUE)$value

utils::data(Exp1_R25_pept, package='DAPARdata2')

ui <- function() {
  tagList(
    fluidPage(
      mod_change_assay_ui('change_dataset'),
      selectInput('manualChange', 'Manual change of dataset', choices = names(Exp1_R25_pept))
    )
  )
}

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  
  utils::data(Exp1_R25_pept, package='DAPARdata2')
  
  
  mod_change_assay_server('change_dataset',
                          ll.se = reactive({names(Exp1_R25_pept)}),
                          indice = reactive({NULL}))
}


shinyApp(ui, server)
