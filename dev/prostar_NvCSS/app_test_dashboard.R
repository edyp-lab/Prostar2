library(shiny)
library(shinydashboard)
setwd("~/TELETRAVAIL/github_DAPARforFeatures/Prostar2/dev/prostar_NvCSS/")

source(file.path('../../R', 'mod_check_updates.R'), local=TRUE)$value
source(file.path('../../R', 'mod_format_DT.R'), local=TRUE)$value
source(file.path('../../R', 'mod_release_notes.R'), local=TRUE)$value
source(file.path('../../R', 'mod_insert_md.R'), local=TRUE)$value
source(file.path('../../R', 'mod_bug_report.R'), local=TRUE)$value
source(file.path('../../R', 'mod_settings.R'), local=TRUE)$value
source(file.path('../../R', 'mod_popover_for_help.R'), local=TRUE)$value
source(file.path('../../R', 'mod_observe_dynamic_colourPicker_input.R'), local=TRUE)$value
source(file.path('../../R', 'global.R'), local=TRUE)$value

ui <- dashboardPage(
  
  
  
  dashboardHeader(title="Prostar",
                  tags$li(class="dropdown",
                          a(href="http://www.prostar-proteomics.org/",
                            img(src="logo.png",
                                title="Prostar website",
                                # Absolute size!
                                height="17px"))),
                  tags$li(class="dropdown",
                          a(href="https://github.com/samWieczorek/Prostar2",
                            icon("github"),
                            title="GitHub"))),
  
  dashboardSidebar(
    sidebarMenu(
      br(),
      menuItem("Data Manager", icon = icon("folder"), startExpanded = TRUE,
               menuSubItem("Import Data", tabName = "import", selected = TRUE), #"Open QFeature file","Convert Data","Demo data"
               menuSubItem("Export Results", tabName = "export"),
               menuSubItem("Reload Prostar", tabName = "reload")),
      hr(),
      menuItem("Global Settings", tabName = "globalSettings", icon = icon("cogs")),
      menuItem("Release Notes", tabName = "releaseNotes", icon = icon("clipboard")),
      menuItem("Check for Updates", tabName = "updates", icon = icon("wrench")),
      menuItem("Help", icon = icon("question-circle"),
               menuSubItem("Useful Links", tabName = "usefulLinks"),
               menuSubItem("FAQ", tabName = "faq"),
               menuSubItem("Bug Report", tabName = "bugReport")),
      hr()
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    tabItems(
      tabItem(tabName = "import",
              h2("Import data")),
      
      tabItem(tabName = "export",
              h2("Export")),
      
      tabItem(tabName = "reload",
              h2("Reload Prostar")),
      
      tabItem(tabName = "globalSettings",
              h2("Global settings"),
              mod_settings_ui('settings')),
      
      tabItem(tabName = "releaseNotes",
              h2("Release notes"),
              mod_release_notes_ui('rl')),
      
      tabItem(tabName = "updates",
              h2("Check for updates"),
              mod_check_updates_ui('test_check')),
      
      tabItem(tabName = "usefulLinks",
              h2("Useful links"),
              mod_insert_md_ui('links_MD')),
      
      tabItem(tabName = "faq",
              h2("FAQ"),
              mod_insert_md_ui('FAQ_MD')),
      
      tabItem(tabName = "bugReport",
              h2("Bug report"),
              mod_bug_report_ui('home'))
    )
  )
  
)

server <- function(input, output,session) {
  
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  mod_settings_server("settings", obj = reactive({Exp1_R25_prot}))
  
  mod_release_notes_server("rl")
  
  mod_check_updates_server('test_check')
  
  mod_insert_md_server('links_MD', URL_links)
  
  mod_insert_md_server("FAQ_MD", URL_FAQ)
  
  mod_bug_report_server('home')
  warning("Test warning message")
}

shinyApp(ui, server)
