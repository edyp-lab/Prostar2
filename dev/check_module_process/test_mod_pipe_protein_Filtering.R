
source(file.path('./', 'test/temp_mod_pipe_protein_newModule_shinyTest.R'), local=TRUE)$value
source(file.path('../../R', 'mod_navigation.R'), local=TRUE)$value
source(file.path('../../R', 'global.R'), local=TRUE)$value

source(file.path('../../R', 'mod_settings.R'), local=TRUE)$value
source(file.path("../../R", "mod_popover_for_help.R"), local = TRUE)$value
source(file.path('../../R', 'mod_observe_dynamic_colourPicker_input.R'), local=TRUE)$value


library(shiny)



ui <- fluidPage(
  tagList(
    mod_pipe_protein_newModule_shinyTest_ui('pipe_filter')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  rv <-reactiveValues(
    current.obj = Exp1_R25_prot
  )
  
  
  mod_pipe_protein_newModule_shinyTest_server('pipe_filter',
                                              obj = reactive({rv$current.obj}),
                                              indice = reactive({2})
  )
  
  
}


shinyApp(ui, server)
