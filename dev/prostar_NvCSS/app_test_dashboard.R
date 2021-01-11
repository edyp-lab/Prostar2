library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(magrittr)
library(shinyjqui)
library(shinyBS)
library(highcharter)
library(DT)
library(shinyjs)
library(shinythemes)


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

# source(file.path('../../R', 'mod_bsmodal.R'), local=TRUE)$value
# source(file.path('../../R/Plots', 'mod_all_plots.R'), local=TRUE)$value
# source(file.path('../../R/Plots', "mod_all_plots.R"), local=TRUE)$value
# source(file.path('../../R/Plots', 'mod_plots_intensity.R'), local = TRUE)$value
# source(file.path('../../R/Plots', 'mod_plots_tracking.R'), local = TRUE)$value
# source(file.path('../../R/Plots', 'mod_plots_legend_colored_exprs.R'), local = TRUE)$value
# source(file.path('../../R/Plots', 'mod_plots_corr_matrix.R'), local = TRUE)$value
# source(file.path('../../R/Plots', 'mod_plots_heatmap.R'), local = TRUE)$value
# source(file.path('../../R/Plots', 'mod_plots_group_mv.R'),  local = TRUE)$value
# source(file.path('../../R/Plots', 'mod_plots_se_explorer.R'),  local = TRUE)$value
# source(file.path('../../R/Plots', 'mod_plots_var_dist.R'), local = TRUE)$value
# source(file.path('../../R/Plots', 'mod_plots_pca.R'), local = TRUE)$value


ui <- dashboardPagePlus(
  #skin="blue",
  
  # https://stackoverflow.com/questions/31711307/how-to-change-color-in-shiny-dashboard
  # orangeProstar <- "#E97D5E"
  # gradient greenblue header
  # greenblue links <- #2fa4e7
  # darker greenblue hover links <- #157ab5
  # darker greenblue titles <- #317eac
  # small titles <- #9999
  # darkest greenblue button reset+next+selected menu
  # color background arrow : #88b7d5 (bleu gris clair)
  # lightgrey #dddd
  # grey #ccc
  # bleu ceruleen #2EA8B1
  # jaune clair 'mark' #FCF8E3
  # green #468847
  # darker green #356635
  
  
  dashboardHeaderPlus(
    # title on top right, shrinks when sidebar collapsed
    title = tagList(
      tags$span(
        class = "logo-mini", style =  "font-size : 14px","Prostar"),
      tags$span(
        class = "logo-lg", "Prostar")
    ),
    # button to mimic data loaded
    tags$li(class="dropdown",
            checkboxInput(inputId = 'data', label = 'Data Loaded?', value = FALSE)
    ),
    # links Prostar website and github
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
      # inactiveClass for import menus inactivation 
      tags$head(tags$style(".inactiveLink {
                           pointer-events: none;
                           background-color: grey;
                           }")),
      # Menus and submenus in sidebar
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
      menuItem("Check for Updates", tabName = "checkUpdates", icon = icon("wrench")),
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
  
  
  output$contenu_dashboardBody <- renderUI({
    
    # "data loading" button > body content extra vertical navigation timeline + stats descr modal button
    if (isFALSE(input$data)){
      col_left <- 1
      col_right <- 12
      display <- "none"
    } else{
      col_left <- 2
      col_right <- 10
      display <- "block"
    }
    
    # body content
    fluidPage(
      theme = shinythemes::shinytheme("cerulean"),
      
      # first column, showed if "data loaded"
      column(col_left, id = "v_timeline", style=paste0("display: ",display," ;"),
             br(),
             h4('Statistic Descriptive'),
             #mod_bsmodal_ui('statsDescriptive'),
             br(),
             # h4('Timeline')
             # , tags$img(src="timeline_v.PNG",
             #            title="General timeline",
             #            style="display:block ; height: 500px; margin: auto;")
      ),
      
      column(col_right,
             tabItems(
               tabItem(tabName = "ProstarHome", class="active", mod_homepage_ui('home')),
               tabItem(tabName = "openFile", h3("Open QFeature file"), mod_import_file_from_ui("open_file")),
               tabItem(tabName = "convert", h3("Convert data"), mod_convert_ms_file_ui("convert_data")),
               tabItem(tabName = "demoData", h3("Charge a demo dataset"), mod_open_demo_dataset_ui("demo_data")),
               tabItem(tabName = "export", h3("Export")), # export module not yet
               tabItem(tabName = "globalSettings", h3('Global settings'), mod_settings_ui('global_settings')),
               tabItem(tabName = "releaseNotes", h3('Release notes'), mod_release_notes_ui('rl')),
               tabItem(tabName = "checkUpdates", h3('Check for updates'), mod_check_updates_ui('check_updates')),
               tabItem(tabName = "usefulLinks", mod_insert_md_ui('links_MD')),
               tabItem(tabName = "faq", mod_insert_md_ui('FAQ_MD')),
               tabItem(tabName = "bugReport", h3('Bug report'), mod_bug_report_ui("bug_report"))
             )
      )
    )
    
  })
  
  # mimics loading data > body content and inactivation of import menus in sidebar
  observeEvent(input$data, { #https://stackoverflow.com/questions/48278111/disable-enable-click-on-dashboard-sidebar-in-shiny
    
    if(isFALSE(input$data)){
      # show sidebar and button sidebar
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
      shinyjs::runjs("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'visible';")
      
      # enable import menus
      shinyjs::removeCssClass(selector = "a[data-value='openFile']", class = "inactiveLink")
      shinyjs::removeCssClass(selector = "a[data-value='convert']", class = "inactiveLink")
      shinyjs::removeCssClass(selector = "a[data-value='demoData']", class = "inactiveLink")
    }
    else{ # "after data loaded"
      # hide sidebar/button sidebar
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
      shinyjs::runjs("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")
      
      # disable import menus 
      shinyjs::addCssClass(selector = "a[data-value='openFile']", class = "inactiveLink")
      shinyjs::addCssClass(selector = "a[data-value='convert']", class = "inactiveLink")
      shinyjs::addCssClass(selector = "a[data-value='demoData']", class = "inactiveLink")
    }
  })
  
  
  
  utils::data(Exp1_R25_prot, package="DAPARdata2")
  
  #---------------------------Server modules calls---------------------------------------------------#
  
  mod_homepage_server('home')
  
  mod_import_file_from_server("open_file")
  mod_convert_ms_file_server("convert_data")
  mod_open_demo_dataset_server("demo_data", pipeline.def=reactive({pipeline.defs}))
  
  mod_settings_server("global_settings", obj = reactive({Exp1_R25_prot}))
  
  mod_release_notes_server("rl")
  
  mod_check_updates_server("check_updates")
  
  mod_insert_md_server("links_MD", URL_links)
  
  mod_insert_md_server("FAQ_MD", URL_FAQ)
  
  mod_bug_report_server("bug_report")
  
  
  # #---------------------------Modules for Stats Desc in Modal---------------------------------------------------#
  # r <- reactiveValues(
  #   settings = NULL
  # )
  # 
  # r$settings <- mod_settings_server("settings", obj=reactive({Exp1_R25_prot}))
  # 
  # mod_all_plots_server("exemple_plot",
  #                      dataIn = reactive({Exp1_R25_prot}),
  #                      indice = reactive({2}),
  #                      settings = reactive({r$settings()}) )
  # 
  # mod_UI <- mod_all_plots_ui("exemple_plot")
  # mod_bsmodal_server("statsDescriptive",
  #                    title = "Plots",
  #                    mod_UI = mod_UI,
  #                    width="75%"
  # )
  
  
}

shinyApp(ui, server)
