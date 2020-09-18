## Sources
# https://www.r-bloggers.com/shiny-add-removing-modules-dynamically/
# https://shiny.rstudio.com/articles/modules.html#migrating-from-callmodule-to-moduleserver
# https://appsilon.com/how-to-safely-remove-a-dynamic-shiny-module/

unsuspendAll <- function(session = getDefaultReactiveDomain()) {
  observe({
    pattern <- "^output_(.*?)_hidden$"
    
    ids <- names(session$clientData) %>%
      grep(pattern, ., value = TRUE) %>%
      sub(pattern, "\\1", .)
    for (id in ids) {
      print(id)
      outputOptions(session$output, id, suspendWhenHidden = FALSE)
    }
  })
} 




library(shiny)
data(mtcars)
cols <- sort(unique(names(mtcars)[names(mtcars) != 'mpg']))


##############
remove_shiny_inputs <- function(id, .input) {
  
  invisible(
    lapply(grep(id, names(.input), value = TRUE), function(i) {
      .subset2(.input, "impl")$.values$remove(i)
    })
  )
}


remove_all_module_observers <- function(session){
  browser()
  cnames <- names(session$userData)
  ind <- grep('_observer_', names(session$userData))
  for (i in cnames)
    session$userData[[i]]$destroy()
} 

remove_module_observers <- function(session, id){
  cnames <- names(session$userData)
  ind <- grep(id, names(session$userData))
  session$userData[[cnames[ind]]]$destroy()
} 


###--------------------------------------------------------------
mod_test_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("lmModel"))
}


mod_test_server <- function(id, val){
  moduleServer(
    id,
    function(input, output, session) {
      
      
      session$userData[[paste0(id,'_observer_1')]] <- observeEvent(val(), {
        print(paste0('In module ', id, ', new value = ', val()))
      })
      
      
      output$lmModel <- renderUI({
        ns <- session$ns
        tags$div(id = environment(ns)$namespace,
                 tagList(
                   wellPanel(
                     tags$p(paste0('Module ', id, ' created'))
                     )
                   )
        )
      })
    }
  )
}


#-----------------------------------


ui <- fluidPage(
  actionButton('addButton', '', icon = icon('plus')),
  selectInput('test', '', choices=1:15, width='70px'),
  uiOutput('mod2delete_ui'),
  actionButton('delButton', 'Delete selected module', icon = icon('times')),
  actionButton('delButtonAll', 'Delete ALL modules', icon = icon('times'))
)


server <- function(input, output, session) {
  
  
  rv <- reactiveValues(activeMods=NULL)
  
  output$mod2delete_ui <- renderUI({
    rv$activeMods
    selectInput('mod2delete', 'Module to delete', choices = rv$activeMods, width='100px')
    
  })
  
 
  
  observeEvent(input$addButton, {
    id <- paste0('mod_', input$addButton)
    
    insertUI(
      selector = '#addButton',
      where = "beforeBegin",
      ui = mod_test_ui(id)
    )
    
    mod_test_server(id, val = reactive(input$test))
    rv$activeMods <- c(rv$activeMods, id)
  })
  
  
  observeEvent(input$delButtonAll, {
    #removeUI(selector = sprintf('#%s', input$mod2delete))
    #remove_shiny_inputs(input$mod2delete, input)
    remove_all_module_observers(session)
    
  })
  
  observeEvent(input$delButton, {
    removeUI(selector = sprintf('#%s', input$mod2delete))
    remove_shiny_inputs(input$mod2delete, input)
    remove_module_observers(session, input$mod2delete)
    rv$activeMods <- rv$activeMods[-which(rv$activeMods==input$mod2delete)]
  })
}
shinyApp(ui = ui, server = server)