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



#' @import shiny sass
#' @importFrom shinyjs hidden extendShinyjs inlineCSS
#' @importFrom shiny includeCSS
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    fluidPage(
      launchGA(),
      tags$script(inactivity),
      shinyjs::useShinyjs(),
      
      #theme = "css/ceruleanProstar.css",
      #theme = shinythemes::shinytheme("cerulean"),
      tagList(
        titlePanel("", windowTitle = "Prostar"),
        ###### DIV LOADING PAGE  #######
        div(id = "loading_page",
            mod_loading_page_ui('loadingPage')
        ),
        
        ###### DIV MAIN CONTENT  #######
        shinyjs::hidden(
          div(id = "main_content",
              div(
                id = "header",
                mod_navbar_menu_ui('mainMenu')
              )  ## end div for main content 2
          ) ## end div for main content 1
          
        ) ## end hidden
      )
    ) ## end fluid
  )
}



#' @import shiny shinyjs
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('inst/app/www', package = 'Prostar2')
  )
 
  tags$head(
    golem::activate_js(),
    golem::favicon()
    , tags$style(".modal-dialog{ width:200px}")
    ,HTML("<script type='text/javascript' src='sbs/shinyBS.js'></script>")
    ,HTML(".tab-content {padding-top: 40px; }")
    ,tags$style(HTML("hr {border-top: 1px solid #000000;}"))
    ,tags$link(rel="stylesheet", type="text/css", href="www/css/prostar.css")
    
    ,tags$style(sass(sass_file("inst/app/www/css/sass-size.scss"),
                     sass_options(output_style = "expanded")))
    ,shiny::includeCSS('http://netdna.bootstrapcdn.com/font-awesome/4.1.0/css/font-awesome.min.css')
    
    ,shinyjs::inlineCSS(".body { font-size:14px;}")
    ,shinyjs::inlineCSS(".rect {float: left;
            width: 100px;
            height: 20px;
            margin: 2px;
            border: 1px solid rgba(0, 0, 0, .2);}")
    # ,inlineCSS(".green {background: #06AB27}")
    # ,inlineCSS(".red {background: #C90404}")
    # ,inlineCSS(".grey {background:lightgrey;}"),
    # tags$head(includeCSS("www/css/arrow.css")),
    # includeCSS("www/progressBar/progressBar.css"),
    
    
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    
  )
}
  