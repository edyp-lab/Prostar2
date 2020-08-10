source(file.path('../../R', 'config.R'), local=TRUE)$value
source(file.path('../../R', 'mod_pipe_pept_norm.R'), local=TRUE)$value
source(file.path('../../R', 'mod_navigation.R'), local=TRUE)$value
source(file.path('../../R', 'mod_plots_density.R'), local=TRUE)$value
source(file.path('../../R', 'mod_plots_intensity.R'), local=TRUE)$value
source(file.path("../../R", "mod_popover_for_help.R"), local = TRUE)$value
source(file.path('../../R', 'mod_format_DT.R'), local=TRUE)$value
source(file.path('../../R', 'mod_infos_dataset.R'), local=TRUE)$value
source(file.path('../../R', 'mod_settings.R'), local=TRUE)$value
source(file.path('../../R', 'global.R'), local=TRUE)$value
source(file.path('../../R', 'mod_observe_dynamic_colourPicker_input.R'), local=TRUE)$value
source(file.path('../../R', 'mod_plots_tracking.R'), local=TRUE)$value


library(highcharter)
library(shinyjs)
library(DAPAR2)
library(DT)
library(tibble)
library(QFeatures)

options(shiny.fullstacktrace = FALSE)


ui <- fluidPage(
  tagList(
    mod_pipe_pept_norm_ui('pipe_norm'),
    mod_infos_dataset_ui('infos')
    # highchartOutput('test')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  utils::data(Exp1_R25_pept, package='DAPARdata2')
  
  rv <-reactiveValues(
    ret = NULL,
    current.obj = Exp1_R25_pept
  )
  
  
  rv$ret <- callModule(mod_pipe_pept_norm_server,
                       'pipe_norm',
                       obj = reactive({Exp1_R25_pept}),
                       ind = reactive({2}))
  
  callModule(mod_infos_dataset_server,'infos',
             obj = reactive({rv$current.obj}))
  
  
  observe({
    req(rv$ret())
    rv$current.obj <- rv$ret()
  })
}


shinyApp(ui, server)
