
# Laucnh minimal necessary code to start the ui server
source(file.path('.', 'mod_change_dataset.R'), local=FALSE)$value

ui <- function() {
  tagList(
    fluidPage(
      titlePanel("", windowTitle = "Foo"),
      
      mod_change_dataset_ui('mod_change_dataset'), 
      
      # this panel is only to view running variables.
      # It does not take reallly part of core
      absolutePanel(
        style= "text-align: center; background-color: #EEEEEE; z-index: 100;",
        top = 150, 
        right = 50, 
        width = "300px",
        height = "50px",
        draggable = TRUE,
        fixed = FALSE,
        cursor = "default",
        wellPanel(
          style= "text-align: center; background-color: lightblue;",
          uiOutput('activeTab'),
          uiOutput('currentObj'),
          uiOutput('names_Input'),
          width='300px'
          )
        ),
      
      navbarPage("Navbar!",
                 id = "navPage",
                 navbarMenu('Data manager', 
                             tabPanel(title = 'Source 1', value='mod_source_1',
                                     mod_source_1_ui('mod_source_1')),
                            tabPanel(title = 'Source 2', value='mod_source_2',
                                     mod_source_2_ui('mod_source_2')),
                            tabPanel(title = 'Source 3', value='mod_source_3',
                                     mod_source_3_ui('mod_source_3'))
                 )        
      )
    )
  )
}
