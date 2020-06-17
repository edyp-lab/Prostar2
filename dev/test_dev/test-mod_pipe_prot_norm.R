source(file.path('../../R', 'config.R'), local=TRUE)$value
source(file.path('../../R', 'mod_pipe_prot_norm.R'), local=TRUE)$value
source(file.path('../../R', 'mod_plots_tracking.R'), local=TRUE)$value
source(file.path('../../R', 'mod_navigation.R'), local=TRUE)$value
source(file.path('../../R', 'mod_plots_density.R'), local=TRUE)$value
source(file.path('../../R', 'mod_plots_intensity.R'), local=TRUE)$value
source(file.path("../../R", "mod_popover_for_help.R"), local = TRUE)$value
source(file.path('../../R', 'mod_format_DT.R'), local=TRUE)$value
source(file.path('../../R', 'mod_infos_dataset.R'), local=TRUE)$value


library(highcharter)
library(shinyjs)
library(DAPAR2)
library(DT)
library(tibble)
library(Features)

options(shiny.fullstacktrace = FALSE)


ui <- fluidPage(
  tagList(
    mod_pipe_prot_norm_ui('pipe_norm'),
    mod_infos_dataset_ui('infos')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  rv <-reactiveValues(
    ret = NULL,
    current.obj = Exp1_R25_prot
  )
  
  rv$ret <- callModule(mod_pipe_prot_norm_server,
             'pipe_norm',
             obj = reactive({Exp1_R25_prot}),
             ind = reactive({2}))

  callModule(mod_infos_dataset_server,'infos',
             obj = reactive({rv$current.obj}))
  
  
  observe({
    req(rv$ret())
    print(rv$ret())
    rv$current.obj <- rv$ret()
  })
}


shinyApp(ui, server)
