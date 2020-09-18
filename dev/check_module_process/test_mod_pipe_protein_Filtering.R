
source(file.path('./', 'mod_pipe_protein_Filtering.R'), local=TRUE)$value
source(file.path('../../R', 'mod_navigation.R'), local=TRUE)$value
source(file.path('../../R', 'global.R'), local=TRUE)$value
source(file.path('../../R', 'mod_format_DT.R'), local=TRUE)$value
source(file.path('../../R', 'mod_settings.R'), local=TRUE)$value
source(file.path("../../R", "mod_popover_for_help.R"), local = TRUE)$value
source(file.path('../../R', 'mod_observe_dynamic_colourPicker_input.R'), local=TRUE)$value


library(shiny)
library(tibble)
library(SummarizedExperiment)



ui <- fluidPage(
  tagList(
    selectInput('n_assay', 'Assay', choices = 1:2),
    mod_pipe_protein_Filtering_ui('pipe_filter')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  rv <-reactiveValues(
    ret = NULL,
    current.obj = Exp1_R25_prot
  )
  
  
  
  rv$ret <- callModule(mod_pipe_protein_Filtering_server,
                       'pipe_filter',
                       obj = reactive({rv$current.obj}),
                       indice = reactive({as.numeric(input$n_assay)
                       })
  )
  
  
  observe({
    req(rv$ret())
    rv$current.obj <- rv$ret()
  })
}


shinyApp(ui, server)
