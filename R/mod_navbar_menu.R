# Module UI
  
#' @title   mod_navbar_menu_ui and mod_navbar_menu_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_navbar_menu
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' @importFrom shinyjs extendShinyjs 
mod_navbar_menu_ui <- function(id){
  ns <- NS(id)
  tagList(
    extendShinyjs(text = "shinyjs.reset = function() {history.go(0)}",
                  functions = c("reset")),
    navbarPage(
      position = "fixed-top",
      id = "navPage",
      inverse = TRUE,
      
      absolutePanel(
        id  = "#AbsolutePanel",
        top = 0, right = 50, width = "500px",height = "50px",
        draggable = FALSE,
        fixed = FALSE,
        cursor = "default"
        ,uiOutput("datasetAbsPanel" )
      ),
      #modulePlotsUI('showPlots')
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
      navbarMenu("Data manager",
                 #            tabPanel("Open MSnset",value = 'openMSnsetTab',moduleOpenMSnSetUI("moduleOpenMSnSet")),
                 #            tabPanel("Convert",value = "convertTab",moduleConvertDataUI("moduleProcess_Convert")),
                 #            tabPanel("Demo data",  value='demoTab', moduleOpenDemoDatasetUI("mod_OpenDemoDataset")),
                 tabPanel(title="ReloadProstar",
                          value="ReloadTab",
                          p("Due to some instability of cache memory when successively opening several datasets in a Prostar session, data management has been simplified. 
                          To work on another dataset than the current one, reloading Prostar first is now necessary (with the button above).  It will restart Prostar
                          with a fresh R session where import menus are enabled 'Dataset manager' menu."),
                          actionButton(ns("ReloadProstar"), "Reload Prostar",class = actionBtnClass)
                 )
      ),
      navbarMenu("Help",
                 tabPanel("Links",value="usefulLinksTab",  mod_insert_md_ui('links_MD')),
                 tabPanel("FAQ", value="faqTab",  mod_insert_md_ui('FAQ_MD')),
                 tabPanel("Bug report",value="bugReportTab",  mod_bug_report_ui('bugreport'))
      )
      
    ) ## end navbarPage
  )
}
    
# Module Server
    
#' @rdname mod_navbar_menu
#' @export
#' @keywords internal
    
mod_navbar_menu_server <- function(input, output, session){
  ns <- session$ns
  observeEvent(input$ReloadProstar, { js$reset()})
}
    
## To be copied in the UI
# mod_navbar_menu_ui("navbar_menu_ui_1")
    
## To be copied in the server
# callModule(mod_navbar_menu_server, "navbar_menu_ui_1")
 
