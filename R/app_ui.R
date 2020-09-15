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



#' @importFrom shinyjs hidden extendShinyjs inlineCSS
#' @importFrom shiny includeCSS
#' 
app_ui <- function() {
  
tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

      launchGA(),
      tags$script(inactivity),
      shinyjs::useShinyjs(),
   shinyjs::extendShinyjs(text = "shinyjs.reset = function() {history.go(0)}",
                           functions = c("reset")),
   
     #theme = "css/ceruleanProstar.css",
      #theme = shinythemes::shinytheme("cerulean"),

        titlePanel("", windowTitle = "Prostar"),
        ###### DIV LOADING PAGE  #######
         div(id = "loading_page",
             mod_loading_page_ui('loadPage')
         ),
         #
        ###### DIV MAIN CONTENT  #######
        shinyjs::hidden(
          div(id = "main_content",
              div(
                id = "header",
                # mod_navbar_menu_ui('mainMenu')
                
                  fluidRow(class = 'headerrow', column(width = 12,
                                                       style = "font-size: 30pt; line-height: 10vh; text-align:left; color:#FFFFFF; width = 100",
                                                       tags$strong('Test')),
                           tags$head(tags$style('.headerrow{height:10vh;}'))
                           ),
                  fluidPage(
                    navbarPage("Prostar",
                    position = "fixed-top",
                    id = "navPage",
                    inverse = FALSE,

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
                               tabPanel("Open MSnset",value = 'openMSnsetTab',
                                        mod_open_dataset_ui('moduleOpenDataset'),
                                        mod_infos_dataset_ui("infos_openFile")
                                        ),
                               tabPanel("Convert",value = "convertTab",
                                        mod_convert_ms_file_ui('moduleProcess_Convert')
                                        ),
                               tabPanel("Demo data",  value='demoTab', 
                                        mod_open_demo_dataset_ui('mod_OpenDemoDataset'),
                                        mod_infos_dataset_ui("infos_demoDataset")
                                        ),
                               tabPanel(title="ReloadProstar",
                                         value="ReloadTab",
                                         p("Due to some instability of cache memory when successively opening several datasets in a Prostar session, data management has been simplified.
                                          To work on another dataset than the current one, reloading Prostar first is now necessary (with the button above).  It will restart Prostar
                                          with a fresh R session where import menus are enabled 'Dataset manager' menu."),
                                         actionButton("ReloadProstar", "Reload Prostar",class = actionBtnClass)
                                          )
                    ),
                    navbarMenu("Data processing",
                               tabPanel("Descriptive statistics", value='descriptiveStats', mod_all_plots_ui('modAllPlots'))
                    ),
                    navbarMenu("Help",
                               tabPanel("Links",value="usefulLinksTab",  mod_insert_md_ui('links_MD')),
                               tabPanel("FAQ", value="faqTab",  mod_insert_md_ui('FAQ_MD')),
                               tabPanel("Bug report",value="bugReportTab",  mod_bug_report_ui('bugreport')

                    )
                    )
                  ) ## end navbarPage
)
                )
              )  ## end div for main content 2
          ) ## end div for main content 1

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
    ,tags$link(rel="stylesheet", type="text/css", href="www/css/prostar.css")
    #,tags$link(rel="stylesheet", type="text/css", href="www/css/loading_page.css")
    ,HTML("<script type='text/javascript' src='sbs/shinyBS.js'></script>")


    ,shiny::includeCSS('http://netdna.bootstrapcdn.com/font-awesome/4.1.0/css/font-awesome.min.css')

    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here

  )
}

