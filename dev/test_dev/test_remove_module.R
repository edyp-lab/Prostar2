## Sources
# https://www.r-bloggers.com/shiny-add-removing-modules-dynamically/




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


lmUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("lmModel"))
}


lmModelModule <- function(input, output, session, param, val) {

  
  observeEvent(val(), {
    print(paste0('In module ', param(), ', new value = ', val()))
  })
  output[['lmModel']] <- renderUI({
    ns <- session$ns
    tags$div(id = environment(ns)[['namespace']],
             tagList(
               wellPanel(
                 tags$div( style="display:inline-block;",
                           tags$p(paste0('Module ', param(), ' created')),
                          actionButton(ns('deleteButton'),  '', icon = shiny::icon('times'))
                 )
                   )
                 )
               )
  })
}


ui <- fluidPage(
  br(),
  actionButton('addButton', '', icon = icon('plus')),
  selectInput('test', '', choices=1:15, width='70px')
)
server <- function(input, output, session) {
  observeEvent(input$addButton, {
    id <- paste0('mod_', input$addButton)

    insertUI(
      selector = '#addButton',
      where = "beforeBegin",
      ui = lmUI(id)
    )
    
    callModule(lmModelModule, id, param=reactive(id), val = reactive(input$test))
    
    observeEvent(input[[paste0(id, '-deleteButton')]], {
      cat('Delete a module')
      removeUI(selector = sprintf('#%s', id))
      remove_shiny_inputs(id, input)
    })
  })
}
shinyApp(ui = ui, server = server)