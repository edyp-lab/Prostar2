source(file.path('../../R', 'config.R'), local=TRUE)$value
source(file.path('../../R', 'global.R'), local=TRUE)$value
source(file.path('../../R', 'mod_plots_MV_for_impute.R'), local=TRUE)$value
source(file.path('../../R', 'mod_settings.R'), local=TRUE)$value
source(file.path("../../R", "mod_popover_for_help.R"), local = TRUE)$value
source(file.path('../../R', 'mod_observe_dynamic_colourPicker_input.R'), local=TRUE)$value



library(highcharter)
library(shinyjs)
library(DAPAR2)
library(DT)
library(tibble)
library(QFeatures)

options(shiny.fullstacktrace = FALSE)


ui <- fluidPage(
  tagList(
    mod_plots_MV_for_impute_ui('mv')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  rv <-reactiveValues(
    ret = NULL,
    current.obj = Exp1_R25_prot,
    settings = NULL
  )
  
 rv$settings <- callModule(mod_settings_server, "settings", obj=reactive({rv$current.obj}))
 
 
  rv$ret <- callModule(mod_plots_MV_for_impute_server,
                       'mv',
                       obj = reactive({rv$current.obj}),
                       ind = reactive({2}),
                       title = reactive({Title}),
                       palette = reactive({rv$settings()$basePalette}))

  observe({
    req(rv$ret())
    rv$current.obj <- rv$ret()
  })
}


shinyApp(ui, server)
