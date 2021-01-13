library(fresh)
library(DAPAR2)
library(QFeatures)
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


#---------------------------------------------------------------------------------------------------------
timeoutSeconds <- 30*60

inactivity <- sprintf("function idleTimer() {
var t = setTimeout(logout, %s);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions
function logout() {
Shiny.setInputValue('timeOut', '%ss')
}
function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, %s);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();", timeoutSeconds*1000, timeoutSeconds, timeoutSeconds*1000)


lapply(list.files('R/Prostar_UI/', pattern='.R'), 
       function(x) {source(file.path('R/Prostar_UI', x), local=TRUE)$value })

lapply(list.files('R/Tools/', pattern='.R'), 
       function(x) {source(file.path('R/Tools', x), local=TRUE)$value })

lapply(list.files('R/DataManager/', pattern='.R'), 
       function(x) {source(file.path('R/DataManager',x), local=FALSE)$value })



#' @importFrom shinyjs hidden extendShinyjs inlineCSS
#' @importFrom shiny includeCSS
#' 
app_ui <- function() {
 
tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    #use_theme(dreamRs),
    launchGA(),
    tags$script(inactivity),
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(text = "shinyjs.reset = function() {history.go(0)}",
                           functions = c("reset")),
   
     #theme = "css/ceruleanProstar.css",
      #theme = shinythemes::shinytheme("cerulean"),

     titlePanel("", windowTitle = "Prostar"),
        ###### DIV LOADING PAGE  #######
         div(id = "loading_page", mod_loading_page_ui('loadPage') ),

        ###### DIV MAIN CONTENT  #######
        shinyjs::hidden(
          div(id = "main_content",
              div(
                id = "header",
                
                dashboardPagePlus(
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
                            actionButton('browser', 'Browser()')
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
                # mod_navbar_menu_ui('mainMenu')

#                   fluidPage(
#                     navbarPage("Prostar",
#                     position = "fixed-top",
#                     id = "navPage",
#                     inverse = FALSE,
# 
#                     
#                     
#                     #modulePlotsUI('showPlots')
#                     navbarMenu("Prostar",
#                                tabPanel(title="Home",
#                                         value="HomeTab",mod_homepage_ui("homepage")),
#                                tabPanel(title="Global settings",
#                                         value="GlobalSettingsTab", mod_settings_ui("modSettings")),
#                                tabPanel("Release notes",
#                                         value="ReleaseNotesTab",mod_release_notes_ui("modReleaseNotes")),
#                                tabPanel("Check for updates",
#                                         value="CheckUpdatesTab",mod_check_updates_ui("modCheckUpdates"))
#                     ),
#                     navbarMenu("Data manager",
#                                tabPanel("Open MSnset",value = 'openMSnsetTab',
#                                         mod_open_dataset_ui('moduleOpenDataset'),
#                                         mod_infos_dataset_ui("infos_openFile")
#                                         ),
#                                tabPanel("Convert",value = "convertTab",
#                                         mod_convert_ms_file_ui('moduleProcess_Convert')
#                                         ),
#                                tabPanel("Demo data",  value='demoTab', 
#                                         mod_open_demo_dataset_ui('mod_OpenDemoDataset'),
#                                         mod_infos_dataset_ui("infos_demoDataset")
#                                         ),
#                                tabPanel(title="ReloadProstar",
#                                          value="ReloadTab",
#                                          p("Due to some instability of cache memory when successively opening several datasets in a Prostar session, data management has been simplified.
#                                           To work on another dataset than the current one, reloading Prostar first is now necessary (with the button above).  It will restart Prostar
#                                           with a fresh R session where import menus are enabled 'Dataset manager' menu."),
#                                          actionButton("ReloadProstar", "Reload Prostar",class = actionBtnClass)
#                                           )
#                     ),
#                     # navbarMenu("Data mining",
#                     #            tabPanel("Descriptive statistics", value='descriptiveStats', mod_all_plots_ui('modAllPlots'))
#                     # ),
#                     navbarMenu("Help",
#                                tabPanel("Links",value="usefulLinksTab",  mod_insert_md_ui('links_MD')),
#                                tabPanel("FAQ", value="faqTab",  mod_insert_md_ui('FAQ_MD')),
#                                tabPanel("Bug report",value="bugReportTab",  mod_bug_report_ui('bugreport')
# 
#                     )
#                     )
#                   ) ## end navbarPage
# )
                 )
               )  ## end div for main content 2
           ) ## end div for main content 1
# 
         ) ## end hidden
    #) ## end fluid

}



#' @import shiny shinyjs
golem_add_external_resources <- function(){

  addResourcePath(
    'www', system.file('app/www', package = 'Prostar2')
  )

  tags$head(
    golem::activate_js(),
    golem::favicon()
    
    ,tags$link(rel="stylesheet", type="text/css", href="www/css/custom_sass.css")
    
    #,tags$head(tags$style(sass::sass(input = sass::sass_file("inst/app/www/css/_variables-Prostar.scss"),
    #                           options = sass::sass_options(output_style = "expanded"))))
    
    # ,tags$head(tags$style(sass::sass(input = sass::sass_file("inst/app/www/css/_variables-cerulean.scss"),
    #                                  options = sass::sass_options(output_style = "expanded"))))
    
    ,tags$link(rel="stylesheet", type="text/css", href="www/css/prostar.css")
    #,tags$link(rel="stylesheet", type="text/css", href="www/css/loading_page.css")
    ,HTML("<script type='text/javascript' src='sbs/shinyBS.js'></script>")


    ,shiny::includeCSS('http://netdna.bootstrapcdn.com/font-awesome/4.1.0/css/font-awesome.min.css')

    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here

  )
}

