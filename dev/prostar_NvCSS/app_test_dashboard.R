library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(magrittr)
library(shinyjqui)
library(shinyBS)
library(highcharter)
library(DT)
library(shinyjs)

#.libPaths("C:/Users/EF249002/Documents/R/win-library/4.0.3/")
#setwd("~/TELETRAVAIL/github_DAPARforFeatures/Prostar2/dev/prostar_NvCSS/")
setwd("~/Github/AdaptedForFeatures/Prostar2/dev/prostar_NvCSS/")

source(file.path('../../R', 'mod_check_updates.R'), local=TRUE)$value
source(file.path('../../R', 'mod_format_DT.R'), local=TRUE)$value
source(file.path('../../R', 'mod_release_notes.R'), local=TRUE)$value
source(file.path('../../R', 'mod_insert_md.R'), local=TRUE)$value
source(file.path('../../R', 'mod_bug_report.R'), local=TRUE)$value
source(file.path('../../R', 'mod_settings.R'), local=TRUE)$value
source(file.path('../../R', 'mod_popover_for_help.R'), local=TRUE)$value
source(file.path('../../R', 'mod_observe_dynamic_colourPicker_input.R'), local=TRUE)$value
source(file.path('../../R', 'mod_homepage.R'), local=TRUE)$value
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


ui <- dashboardPagePlus(
  skin="blue",
  #orangeProstar <- "#E97D5E"
  #https://stackoverflow.com/questions/31711307/how-to-change-color-in-shiny-dashboard
  
  dashboardHeaderPlus(
    title = tagList(
      tags$span(
        class = "logo-mini", style =  "font-size : 14px","Prostar"),
      tags$span(
        class = "logo-lg", "Prostar")
    ),
    
    tags$li(class="dropdown",
            checkboxInput(inputId = 'data', label = 'Data Loaded?', value = FALSE)
    ),
    tags$li(class="dropdown",
            a(href="http://www.prostar-proteomics.org/",
              img(src="logo.png",
                  title="Prostar website",
                  height="17px"))),
    tags$li(class="dropdown",
            a(href="https://github.com/samWieczorek/Prostar2",
              icon("github"),
              title="GitHub"))
  ),
  
  
  dashboardSidebar(
    sidebarMenu(
      br(),
      menuItem("Home", tabName = "ProstarHome", icon = icon("home"),selected = TRUE
      ),
      hr(),
      menuItem("Data Manager", icon = icon("folder"), startExpanded = TRUE,
               menuSubItem("Open QFeature file", tabName = "openFile"),
               menuSubItem("Convert Data", tabName = "convert"),
               menuSubItem("Demo data", tabName = "demoData"),
               menuSubItem("Export Results", tabName = "export")),
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
    
    useShinyjs(),
    
    uiOutput('contenu_dashboardBody')
    
  )
)




server <- function(input, output,session) {
  

  # hide/show sidebar/button sidebar
  observeEvent(input$data, {
    print(input$data)
    if(isFALSE(input$data)){
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
      shinyjs::runjs("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'visible';")}
    else{
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
      shinyjs::runjs("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")}
  })
  
  
  mod_homepage_server('home')
  
  output$contenu_dashboardBody <- renderUI({
    
    
    if (isFALSE(input$data)){
      col_left <- 1
      col_right <- 12
      display <- "none"
    } else{
      col_left <- 2
      col_right <- 10
      display <- "block"
    }
    
    fluidPage(
      
      column(col_left, id = "v_timeline", style=paste0("display: ",display," ;"),
             br(),
             h4('Statistic Descriptive'),
             mod_bsmodal_ui('statsDescriptive'),
             br(),
             h4('Timeline'),
             tags$img(src="timeline_v.PNG",
                      title="General timeline",
                      style="display:block ; height: 500px; margin: auto;")
             
      ),
      column(col_right, tabItems(
        tabItem(tabName = "ProstarHome", class="active", h2("Home Prostar"),mod_homepage_ui('home')
        ),
        tabItem(tabName = "openFile", h2("Open QFeature file"), mod_import_file_from_ui("openFile")
                ),
        tabItem(tabName = "convert", h2("Convert Data"), mod_convert_ms_file_ui("convert")
                ),
        tabItem(tabName = "demoData", h2("Demo data"), mod_open_demo_dataset_ui("demoData")
                ),
        tabItem(tabName = "export", h2("Export")
                ),
        tabItem(tabName = "globalSettings", h2("Global settings")
                ),
        tabItem(tabName = "releaseNotes", h2("Release notes")
                ),
        tabItem(tabName = "updates", h2("Check for updates")
                ),
        tabItem(tabName = "usefulLinks", h2("Useful links")
                ),
        tabItem(tabName = "faq", h2("FAQ")
                ),
        tabItem(tabName = "bugReport", h2("Bug report"), mod_bug_report_ui("bugReport")
                )
      )
      )
    )
    
    
  })
  
  
  
  
  
  #------------------------------------------------------------------------------#
  
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

  # #------------------------------------------------------------------------------#
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
  # #------------------------------------------------------------------------------#
  
}

shinyApp(ui, server)
