ui <- function() {
  tagList(
    fluidPage(
      titlePanel("", windowTitle = "Foo"),
      wellPanel(
        uiOutput('activeTab'),
        uiOutput('currentObj')
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
                 ),
                 tabPanel(title='Manage modules', value='manage_modules',
                          tags$p('Add modules'),
                          actionButton('addButton', '', icon = icon('plus'))
                 )
                 
      )
    )
  )
}
