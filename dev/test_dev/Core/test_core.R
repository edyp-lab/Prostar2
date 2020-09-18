library(DAPAR2)
library(shiny)


source(file.path('../../../R', 'mod_navbar_menu.R'), local=TRUE)$value
source(file.path('.', 'mod_A.R'), local=TRUE)$value
source(file.path('.', 'mod_B.R'), local=TRUE)$value
source(file.path('.', 'mod_C.R'), local=TRUE)$value


remove_shiny_inputs <- function(id, .input) {
  invisible(
    lapply(grep(id, names(.input), value = TRUE), function(i) {
      .subset2(.input, "impl")$.values$remove(i)
    })
  )
}


remove_all_module_observers <- function(session){
  cnames <- names(session$userData)
  ind <- grep('_observer_', names(session$userData))
  lapply(cnames, function(x) {session$userData[[x]]$destroy()})
} 




ui <- function() {
  tagList(
    fluidPage(
      titlePanel("", windowTitle = "toto"),
      uiOutput('activeTab'),
      uiOutput('currentObj'),
      selectInput('n', 'N', choices=1:5, width='70px'),
      navbarPage("Navbar!",
                 id = "navPage",
                 
                 tabPanel(title='Manage modules', value='manage_modules',
                          tags$p('Add modules'),
                          actionButton('addButton', '', icon = icon('plus'))
                          )

      )
    )
  )
}

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  rv <- reactiveValues(
    current.obj = NULL,
    tmp = NULL
  )
   
  output$activeTab <- renderUI({
    input$navPage
  })
  
  output$currentObj <- renderUI({
    p(paste0('rv$current.obj = ', rv$current.obj))
  })
  
  observeEvent(input$addButton, {
    
    tabs <- list(
      tabPanel("mod_A", value='mod_A', mod_A_ui('mod_A')),
      tabPanel("mod_B", value='mod_B', mod_B_ui('mod_B')),
      tabPanel("mod_C", value='mod_C', mod_C_ui('mod_C'))
    )
    
    insertTab(inputId = "navPage",
              do.call(navbarMenu, c('Process' ,tabs)),
              target="manage_modules",
              position="after")
    
    })
  

  
  
  
  observeEvent(input$navPage,{
    
    # Delete the server part of all modules
    #browser()
    remove_all_module_observers(session)
    
    if(input$navPage == "mod_A")
      source(file.path('.', 'watch_mod_A.R'), local=TRUE)$value
    if(input$navPage == "mod_B")
      source(file.path('.', 'watch_mod_B.R'), local=TRUE)$value
    if(input$navPage == "mod_C")
      source(file.path('.', 'watch_mod_C.R'), local=TRUE)$value
  })
  
}


shinyApp(ui, server)
