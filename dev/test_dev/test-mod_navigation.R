library(rhandsontable)
#source(file.path('../../R', 'mod_navigation.R'), local=TRUE)$value
source(file.path('../../R', 'global.R'), local=TRUE)$value
source(file.path('Core/Tests', 'mod_timeline.R'), local=TRUE)$value

ui <- fluidPage(
  tagList(
    mod_timeline_ui("test_nav"),
    uiOutput('show')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  rv.timeline <- reactiveValues(
    stepsNames = c("Description", "Step 1", "Step 2", "Step 3"),
    isDone =  c(TRUE, FALSE, FALSE, FALSE),
    mandatory =  c(FALSE, FALSE, TRUE, TRUE),
    current.pos = 1
  )
  
  
  ll.UI <- reactiveValues(
    screens = NULL
  )
  
  
  
  pos <- mod_timeline_server("test_nav",style = 2, pages = rv.timeline  )
  
  
  
  #observeEvent(pos()$current.pos, {
  #  lapply(1:length(rv.timeline$stepsNames), function(x){shinyjs::toggle(paste0('screen', x) )})
  #  })
  
  #observeEvent(pos()$rstBtn, { xx})
  
  #observeEvent(pos()$nextBtn, { })
  
  #observeEvent(pos()$prevBtn, { })
  
  #-------------------------------------------
  
  
  observeEvent(pos()$current.pos, {
    
   # browser()
    ll.UI$screens <- lapply(1:length(rv.timeline$stepsNames), function(x){
      do.call(uiOutput, list(outputId=paste0("screen", x)))})
      
      
    # initialisation fo the screens
    ll.UI$screens[[1]] <- div(id = paste0("screen", 1),  ll.UI$screens[[1]])
    for (i in 2:length(rv.timeline$stepsNames)){
      ll.UI$screens[[i]] <- shinyjs::hidden(div(id = paste0("screen", i),  ll.UI$screens[[i]]))
    }
  })
  
  observeEvent(req(rv.timeline$isDone[pos()$current.pos]), {
    # Disable all previous screensbut
      lapply(1:length(rv.timeline$stepsNames), function(x){ shinyjs::disable(paste0('screen', x))})
  })
  
  #------------------------------------------------
  
  output$show <- renderUI({
    tagList(
      ll.UI$screens
    )
  })

  
  reset <- reactive({
    rv.timeline$isDone <- c(TRUE,rep(FALSE, 3))
    for (i in 1:length(rv.timeline$stepsNames))
      shinyjs::reset(paste0('screen', i))
  })
  
  
  observeEvent(req(rv.timeline$reset),{
    rv.timeline$reset  <- FALSE
    print('reset activated from navigation module')
    reset()
  })

  
  ##------------------------------------------------------------
  #default values for the widgets
  r.params <- reactiveValues(
    select1 = 1,
    select2 = 10,
    select3 = 100
  )
  
  rv.data <- reactiveValues(
    data = list(original = 'original')
  )
  
  #*observeEvent(input$done1,{rv.timeline$isDone[1] <- TRUE})
  observeEvent(input$done2,{
    rv.timeline$isDone[2] <- TRUE
    rv.data$data <- append(rv.data$data, input$select1)})
  
  observeEvent(input$done3,{rv.timeline$isDone[3] <- TRUE
  rv.data$data <- append(rv.data$data, input$select2)})
  
  observeEvent(input$done4,{rv.timeline$isDone[4] <- TRUE
  rv.data$data <- append(rv.data$data, input$select3)})
  
  
  output$screen1 <- renderUI({
    tagList(
      tags$h1('Description of the module')
    )
      
  })
  
  
  
  output$screen2 <- renderUI({
 
        tagList(
          div(id='screen2',
              tags$h2('Step 1'),
      actionButton('done2', 'Set done 1'),
      selectInput('select1', 'Select 1', 
                  choices = 1:5, 
                  selected = 1,
                  width = '150px')
        )
    )
  })
  
  
  output$screen3 <- renderUI({
    
        tagList(
          div(id='screen3',
              tags$h3('Step 2'),
      actionButton('done3', 'Set done 2'),
      selectInput('select2', 'Select 2',
                  choices = 1:5,
                  selected = r.params$select2,
                  width = '150px')
        )
    )
  })
  
  output$screen4 <- renderUI({
    
        tagList(
          div(id='screen4',
              tags$h1('Step 4'),
      actionButton('done4', 'Validate')
    )
    )
  })
  
}


shinyApp(ui, server)
