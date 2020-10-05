library(DAPAR2)
library(shiny)
library(DAPARdata2)

# files <- list.files('../../R', pattern='.R')
# files <- files[-which(files=='app_server.R')]
# files <- files[-which(files=='app_ui.R')]
# 
# for (f in files)
#   source(file.path('../../R',f), local=TRUE)$value
#  
source(file.path('../../R', 'mod_change_assay.R'), local=TRUE)$value
source(file.path('../../R/DataManager', 'mod_choose_pipeline.R'), local=TRUE)$value
source(file.path('../../R', 'mod_infos_dataset.R'), local=TRUE)$value
source(file.path('../../R', 'mod_format_DT.R'), local=TRUE)$value

source(file.path('../../R', 'global.R'), local=TRUE)$value
source(file.path('../../R/DataManager', 'mod_open_demo_dataset.R'), local=TRUE)$val
source(file.path('../../R', 'mod_navbar_menu.R'), local=TRUE)$value
source(file.path('../../R', 'mod_homepage.R'), local=TRUE)$value
source(file.path('../../R', 'mod_insert_md.R'), local=TRUE)$value
source(file.path('../../R', 'mod_settings.R'), local=TRUE)$value
source(file.path('../../R', 'mod_popover_for_help.R'), local=TRUE)$value
source(file.path('../../R', 'mod_release_notes.R'), local=TRUE)$value
source(file.path('../../R', 'mod_check_updates.R'), local=TRUE)$value
source(file.path('../../R', 'mod_bug_report.R'), local=TRUE)$value

utils::data('Exp1_R25_pept', package='DAPARdata2')

ui <- function() {
  tagList(
    fluidPage(
      titlePanel("", windowTitle = "toto"),
      
      # div(id = "main_content",
      #     div(
      #       id = "header",
      #       # mod_navbar_menu_ui('mainMenu')
      #       
      #       fluidRow(class = 'headerrow', column(width = 12,
      #                                            style = "font-size: 30pt; line-height: 10vh; text-align:left; color:#FFFFFF; width = 100",
      #                                            tags$strong('Test')),
      #                tags$head(tags$style('.headerrow{height:10vh;}'))
      #       ),
      
      navbarPage("Navbar!",
                 #position = "fixed-top",
                 id = "navPage",
                 inverse = FALSE,
                 
                 navbarMenu("Prostar",
                            tabPanel(title="Home",
                                     value="HomeTab",mod_homepage_ui("homepage")),
                            tabPanel(title="Global settings",
                                     value="GlobalSettingsTab", mod_settings_ui("modSettings")),
                            tabPanel("Release notes",
                                     value="ReleaseNotesTab",mod_release_notes_ui("modReleaseNotes")),
                            tabPanel("Check for updates",
                                     value="CheckUpdatesTab",mod_check_updates_ui("modCheckUpdates"))
                 ),
                 tabPanel('Test change assay',
                          selectInput('manualChange', 'Manual change of dataset', 
                                      choices = names(Exp1_R25_pept))),
                 mod_change_assay_ui('change_dataset')
                 
      )
    )
    # )
  )
}

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  utils::data('Exp1_R25_pept', package='DAPARdata2')
  
  rv <- reactiveValues(
    tmp = NULL,
    indice = NULL
  )
  #callModule(mod_navbar_menu_server,'menu')
  
  #callModule(mod_open_demo_dataset_server, 'mod_OpenDemoDataset', pipeline.def=reactive({pipeline.defs}))
  
  # callModule(mod_bug_report_server, "bugreport")
  #callModule(mod_insert_md_server, "links_MD",URL_links)
  #callModule(mod_insert_md_server, "FAQ_MD", URL_FAQ)
  
  rv$tmp <- mod_change_assay_server('change_dataset',
                                    ll.se = reactive({names(Exp1_R25_pept)}),
                                    indice = reactive({NULL}))
  
  
  observeEvent(rv$tmp, {
    rv.indice <- rv$tmp
  })
}


shinyApp(ui, server)
# library(shiny); runApp('dev/test_dev/test-mod_infos_dataset.R')