config <- list(
  process.name = 'Filtering',
  mandatory = setNames(c(F,F,F), c('Description', 'Step1', 'Step2')),
  status = setNames(c(F,F,F), c('Description', 'Step1', 'Step2'))
)

##################################################################################################

ProcessLogics = function(self, input, output){
  ns <- NS(self$id)
  output$Description <- renderUI({
    tagList(
      actionButton(ns('start_btn'), 
                   paste0('Start ', self$config$process.name),
                   class = btn_success_color),
      mod_insert_md_ui(ns(paste0(self$config$process.name, "_md")))
    )
  })
  
  
  observe({
    mod_insert_md_server(paste0(self$config$process.name, "_md"), 
                         paste0('./md/', self$config$process.name, '.md'))
  })
  
  # observeEvent(input$start_btn, {
  #   self$InitializeDataIn()
  #   ValidateCurrentPos()
  # })
  
  ############### SCREEN 2 ######################################
  
  output$Step1 <- renderUI({
    name <- 'Step1'
    
    tagList(
      div(id=ns(name),
          div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
              tags$h2(name)),
          div(style="display:inline-block; vertical-align: middle; padding-right: 40px;",
              selectInput(ns('select1'), 'Select step 1', 
                          choices = 1:5, 
                          selected = 1,
                          width = '150px')
          ),
          div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
              actionButton(ns(paste0('perform_', name, '_btn')), 'Perform'))
      )
    )
  })
  
  
  observeEvent(input$perform_Step1_btn, {
    self$ValidateCurrentPos()
  })
  
  ############### SCREEN 3 ######################################
  output$Step2 <- renderUI({
    name <- 'Step2'
    tagList(
      div(id=ns(name),
          div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
              tags$h3(name)),
          div(style="display:inline-block; vertical-align: middle;padding-right: 40px;",
              selectInput(ns('select2'), 'Select step 2',
                          choices = 1:5,
                          selected = 1,
                          width = '150px')),
          div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
              actionButton(ns(paste0('perform_', name, '_btn')), 'Perform'))
      )
    )
  })
  
  ## Logics to implement: here, we must take the last data not null
  # in previous datas. The objective is to take account
  # of skipped steps
  observeEvent(input$perform_Step2_btn, {
    self$ValidateCurrentPos()
  })
  
  
  
  
  ############### SCREEN 4 ######################################
  output$Step3 <- renderUI({
    name <- 'Step3'
    
    tagList(
      div(id=ns(name),
          div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
              tags$h3(name)),
          div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
              actionButton(ns('validate_btn'), 'Validate'))
      )
    )
    
  })
  
  
  observeEvent(input$validate_btn, {
    # rv$dataIn <- AddItemToDataset(self$rv$dataIn, self$config$process.name)
    self$ValidateCurrentPos()
  })
}