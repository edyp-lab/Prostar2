source(file.path('../../R/DataManager', 'mod_choose_pipeline.R'), local=TRUE)$value
source(file.path('../../R', 'mod_infos_dataset.R'), local=TRUE)$value
source(file.path('../../R', 'mod_format_DT.R'), local=TRUE)$value
source(file.path('../../R', 'global.R'), local=TRUE)$value
source(file.path('../../R', 'config.R'), local=TRUE)$value
source(file.path('../../R/DataManager', 'mod_open_demo_dataset.R'), local=TRUE)$value

options(shiny.fullstacktrace = TRUE)

# files <- list.files('../../R', pattern='.R')
# files <- files[-which(files=='app_server.R')]
# files <- files[-which(files=='app_ui.R')]
# 
# for (f in files)
#   source(file.path('../../R',f), local=TRUE)$value

ui <- function(){
  tagList(
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(text = "shinyjs.reset = function() {history.go(0)}",
                           functions = c("reset")),
    fluidPage(
      # fluidRow(class = 'headerrow', column(width = 12, 
      #                                      style = "font-size: 30pt; line-height: 10vh; text-align:left; color:#FFFFFF; width = 100", 
      #                                      tags$strong('Test')), 
      #          tags$head(tags$style('.headerrow{height:10vh;}'))),
      navbarPage("Navbar!",
                 #position = "fixed-top",
                 navbarMenu("Data manager",
                            tabPanel("Open demo dataset",
                                     mod_open_demo_dataset_ui('rl')
                            )
                 )
      )
    )
  )
}

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  
  mod_open_demo_dataset_server('rl', pipeline.def=reactive({pipeline.defs}))
}


shinyApp(ui, server)
