source(file.path('../../R', 'config.R'), local=TRUE)$value
source(file.path('../../R', 'mod_pipe_prot_norm.R'), local=TRUE)$value
source(file.path('../../R', 'mod_plots_tracking.R'), local=TRUE)$value
source(file.path('../../R', 'mod_navigation.R'), local=TRUE)$value
source(file.path('../../R', 'mod_plots_density.R'), local=TRUE)$value
source(file.path('../../R', 'mod_plots_intensity.R'), local=TRUE)$value
source(file.path("../../R", "mod_popover_for_help.R"), local = TRUE)$value


options(shiny.fullstacktrace = FALSE)


ui <- fluidPage(
  tagList(
    mod_pipe_prot_norm_ui('pipe_norm')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  callModule(mod_pipe_prot_norm_server,
             'pipe_norm',
             obj = reactive({Exp1_R25_prot}),
             ind = reactive({2}))
}


shinyApp(ui, server)
