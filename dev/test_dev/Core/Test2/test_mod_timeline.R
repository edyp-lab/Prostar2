source(file.path('../../../../R', 'global.R'), local=TRUE)$value
source(file.path('./Timelines', 'mod_timeline.R'), local=TRUE)$value

options(shiny.fullstacktrace = T)
ui <- fluidPage(
  tagList(
    mod_timeline_ui("timeline")
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {

  
  config <- reactiveValues(
    stepsNames = c("Description", "Step 1", "Step 2", "Step 3"),
    mandatory = c(FALSE, FALSE, TRUE, TRUE),
    isDone = c(TRUE, FALSE, FALSE, FALSE)
  )
  
  rv <- reactiveValues(
    current.pos = 1, 
    screens = NULL)
  
    actions <- reactiveValues(rst = TRUE,
                   nxt = TRUE,
                   prv = TRUE)

    
    InitScreens <- reactive({
      # initialisation of the screens
      for (i in 1:length(config$stepsNames))
        rv$screens[[i]] <- if (i == rv$current.pos) 
          div(id = paste0("screen", i),  rv$screens[[i]])
      else  
        shinyjs::hidden(div(id = paste0("screen", i),  rv$screens[[i]]))
      rv$screens
    })
    
    CreateScreens <- reactive({
      rv$screens <- lapply(1:length(config$stepsNames), function(x){
        do.call(uiOutput, list(outputId=paste0("screen", x)))})
      rv$screens
    })
    
    
    observeEvent(rv$screens, ignoreNULL=F,{
      CreateScreens()
      InitScreens()
 print(rv$screens)
    })
  
  mod_timeline_server("timeline", 
                      style = 2, 
                      screens = rv$screens, 
                      process_config=config, 
                      actions=actions)
}


shinyApp(ui, server)
