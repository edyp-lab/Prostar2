library(DAPAR2)
library(shiny)

# files <- list.files('../../R', pattern='.R')
# files <- files[-which(files=='app_server.R')]
# files <- files[-which(files=='app_ui.R')]
# 
# for (f in files)
#   source(file.path('../../R',f), local=TRUE)$value
# 
source(file.path('../../R', 'mod_choose_pipeline.R'), local=TRUE)$value
source(file.path('../../R', 'mod_infos_dataset.R'), local=TRUE)$value
source(file.path('../../R', 'mod_format_DT.R'), local=TRUE)$value

source(file.path('../../R', 'global.R'), local=TRUE)$value
source(file.path('../../R', 'mod_open_demo_dataset.R'), local=TRUE)$val
source(file.path('../../R', 'mod_navbar_menu.R'), local=TRUE)$value
source(file.path('../../R', 'mod_homepage.R'), local=TRUE)$value
source(file.path('../../R', 'mod_insert_md.R'), local=TRUE)$value
source(file.path('../../R', 'mod_settings.R'), local=TRUE)$value
source(file.path('../../R', 'mod_popover_for_help.R'), local=TRUE)$value
source(file.path('../../R', 'mod_release_notes.R'), local=TRUE)$value
source(file.path('../../R', 'mod_check_updates.R'), local=TRUE)$value
source(file.path('../../R', 'mod_bug_report.R'), local=TRUE)$value

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
                   )
        )
      )
     # )
  )
}

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
 
  callModule(mod_navbar_menu_server,'menu')
  
  #callModule(mod_open_demo_dataset_server, 'mod_OpenDemoDataset', pipeline.def=reactive({pipeline.defs}))
  
  # callModule(mod_bug_report_server, "bugreport")
  #callModule(mod_insert_md_server, "links_MD",URL_links)
  #callModule(mod_insert_md_server, "FAQ_MD", URL_FAQ)
}


shinyApp(ui, server)
# library(shiny); runApp('dev/test_dev/test-mod_infos_dataset.R')