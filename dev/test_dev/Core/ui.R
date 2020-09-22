
# Laucnh minimal necessary code to start the ui server
source(file.path('.', 'mod_change_dataset.R'), local=FALSE)$value
dir <- './DataManager'
lapply(list.files(dir, pattern='.R'), 
       function(x) {source(file.path(dir,x), local=FALSE)$value })

# List available values for workflows
ll.wf <- paste0('wf', 1)

ui <- function() {
  tagList(
    fluidPage(
      titlePanel("", windowTitle = "Foo"),
       
      # this panel is only to view running variables.
      # It does not take reallly part of core
      absolutePanel(
        style= "z-index: 100;",
        top = 150, 
        right = 50, 
        width = "300px",
        height = "50px",
        draggable = TRUE,
        fixed = FALSE,
        cursor = "default",
        wellPanel(
          style= " background-color: lightblue; padding: 2px",
          tags$h3('Prostar core'),
          uiOutput('activeTab'),
          uiOutput('currentIndice'),
          uiOutput('currentObj'),
          #uiOutput('names_Input'),
          width='300px'
          )
        ),
      
      navbarPage("Navbar!",
                 id = "navPage",
                 # This menu is hardcoded because it is static and there are only few
                 # data manager modules. But it is certainly possible to
                 # implement a generic module of data manager which
                 # contains those 3 modules. The problem is the display of tabPanels
                 # in the UI which does not work
                 navbarMenu('data_manager', 
                            tabPanel(title = 'Data manager 1', value='mod_datamanager_1',
                                     mod_datamanager_1_ui('mod_datamanager_1', ll.wf)),
                            tabPanel(title = 'Data manager 2', value='mod_datamanager_2',
                                     mod_datamanager_2_ui('mod_datamanager_2', ll.wf)),
                            tabPanel(title = 'Data manager 3', value='mod_datamanager_3',
                                     mod_datamanager_3_ui('mod_datamanager_3', ll.wf))
                 )        
      )
    )
  )
}
