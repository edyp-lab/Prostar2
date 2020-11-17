library(shiny)
library(shinydashboard)
library(magrittr)
library(shinyjqui)
library(shinyBS)
library(highcharter)
library(DT)
library(shinyjs)

setwd("~/TELETRAVAIL/github_DAPARforFeatures/Prostar2/dev/prostar_NvCSS/")

source(file.path('../../R', 'mod_check_updates.R'), local=TRUE)$value
source(file.path('../../R', 'mod_format_DT.R'), local=TRUE)$value
source(file.path('../../R', 'mod_release_notes.R'), local=TRUE)$value
source(file.path('../../R', 'mod_insert_md.R'), local=TRUE)$value
source(file.path('../../R', 'mod_bug_report.R'), local=TRUE)$value
source(file.path('../../R', 'mod_settings.R'), local=TRUE)$value
source(file.path('../../R', 'mod_popover_for_help.R'), local=TRUE)$value
source(file.path('../../R', 'mod_observe_dynamic_colourPicker_input.R'), local=TRUE)$value
source(file.path('../../R', 'mod_navigation.R'), local=TRUE)$value
source(file.path('../../R', 'global.R'), local=TRUE)$value
source(file.path('../../R', 'config.R'), local=TRUE)$value

source(file.path('../../R/DataManager', 'mod_import_file_from.R'), local=TRUE)$value
source(file.path('../../R/DataManager', 'mod_convert_ms_file.R'), local=TRUE)$value
source(file.path('../../R/DataManager', 'mod_select_keyID.R'), local=TRUE)$value
source(file.path('../../R/DataManager', 'mod_build_design.R'), local=TRUE)$value
source(file.path('../../R/DataManager', 'mod_build_design_example.R'), local=TRUE)$value
source(file.path('../../R/DataManager', 'mod_choose_pipeline.R'), local=TRUE)$value
source(file.path('../../R/DataManager', 'mod_open_demo_dataset.R'), local=TRUE)$value

source(file.path('../../R', 'mod_bsmodal.R'), local=TRUE)$value
source(file.path('../../R/Plots', 'mod_all_plots.R'), local=TRUE)$value
source(file.path("../../R/Plots", "mod_all_plots.R"), local=TRUE)$value
source(file.path("../../R/Plots", "mod_plots_intensity.R"), local = TRUE)$value
source(file.path("../../R/Plots", "mod_plots_tracking.R"), local = TRUE)$value
source(file.path("../../R/Plots", "mod_plots_legend_colored_exprs.R"), local = TRUE)$value
source(file.path("../../R/Plots", "mod_plots_corr_matrix.R"), local = TRUE)$value
source(file.path("../../R/Plots", "mod_plots_heatmap.R"), local = TRUE)$value
source(file.path("../../R/Plots", "mod_plots_group_mv.R"),  local = TRUE)$value
source(file.path("../../R/Plots", "mod_plots_se_explorer.R"),  local = TRUE)$value
source(file.path("../../R/Plots", "mod_plots_var_dist.R"), local = TRUE)$value
source(file.path("../../R/Plots", "mod_plots_pca.R"), local = TRUE)$value


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
               menuSubItem("Import Data", tabName = "import", selected = TRUE),
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
    
    fluidPage(
      
      
      tags$div(column(2,
                      id = "v_timeline",
                      style = paste0("display: ","none",";"), # "block" if loaded data else "none"
                      br(),
                      h4('Statistic Descriptive'),
                      mod_bsmodal_ui('statsDescriptive'),
                      br(),
                      h4('Timeline'))),
      
      column(10,
             tabItems(
               tabItem(tabName = "import",
                       h2("Import data"),
                       tabsetPanel(type = "tabs",
                                   tabPanel("Open QFeature file",
                                            h3("Tab1"),
                                            mod_import_file_from_ui("openFile")
                                   ),
                                   tabPanel("Convert Data",
                                            h3("Tab2"),
                                            mod_convert_ms_file_ui("convert")
                                   ),
                                   tabPanel("Demo data",
                                            h3("Tab3"),
                                            mod_open_demo_dataset_ui("demoData")))
               ),
               
               
               tabItem(tabName = "export",
                       h2("Export")),
               
               tabItem(tabName = "reload",
                       h2("Reload Prostar")),
               
               tabItem(tabName = "globalSettings",
                       h2("Global settings"),
                       mod_settings_ui("settingsOptions")),
               
               tabItem(tabName = "releaseNotes",
                       h2("Release notes"),
                       mod_release_notes_ui("rl")),
               
               tabItem(tabName = "updates",
                       h2("Check for updates"),
                       mod_check_updates_ui("test_check")),
               
               tabItem(tabName = "usefulLinks",
                       h2("Useful links"),
                       mod_insert_md_ui("links_MD")),
               
               tabItem(tabName = "faq",
                       h2("FAQ"),
                       mod_insert_md_ui("FAQ_MD")),
               
               tabItem(tabName = "bugReport",
                       h2("Bug report"),
                       mod_bug_report_ui('bugReport'))
             )),
    )
  )
  
)

server <- function(input, output,session) {
  
  utils::data(Exp1_R25_prot, package="DAPARdata2")
  
  mod_import_file_from_server("openFile")
  mod_convert_ms_file_server("convert")
  mod_open_demo_dataset_server("demoData", pipeline.def=reactive({pipeline.defs}))
  
  mod_settings_server("settingsOptions", obj = reactive({Exp1_R25_prot}))
  
  mod_release_notes_server("rl")
  
  mod_check_updates_server("test_check")
  
  mod_insert_md_server("links_MD", URL_links)
  
  mod_insert_md_server("FAQ_MD", URL_FAQ)
  
  mod_bug_report_server("bugReport")
  warning("Test warning message")
  
  #------------------------------------------------------------------------------#
  r <- reactiveValues(
    settings = NULL
  )
  
  r$settings <- mod_settings_server("settings", obj=reactive({Exp1_R25_prot}))
  
  mod_all_plots_server("exemple_plot",
                       dataIn = reactive({Exp1_R25_prot}),
                       indice = reactive({2}),
                       settings = reactive({r$settings()}) )
  
  mod_UI <- mod_all_plots_ui("exemple_plot")
  mod_bsmodal_server("statsDescriptive",
                     title = "Plots",
                     mod_UI = mod_UI,
                     width="75%"
  )
  #------------------------------------------------------------------------------#
  
}

shinyApp(ui, server)
