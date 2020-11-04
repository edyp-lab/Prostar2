list(
  config = list(process.name = 'ProcessA',
                steps = c('Description', 'Step1', 'Step2', 'Step3'),
                mandatory = setNames(c(F,F,F,F), c('Description', 'Step1', 'Step2', 'Step3'))
  ),
  RenderUIs_Definitions = function(input, output, config){
      ns <- NS(private$id)
      output$Description <- renderUI({
        tagList(
          actionButton(ns('btn_validate_Description'), 
                       paste0('Start ', config$process.name),
                       class = btn_success_color),
          mod_insert_md_ui(ns(paste0(config$process.name, "_md")))
        )
      })
      
      observe({
        mod_insert_md_server(paste0(config$process.name, "_md"), 
                             paste0('./md/', config$process.name, '.md'))
      })
      
      observeEvent(input$btn_validate_Description, {
        self$InitializeDataIn()
        ValidateCurrentPos()
      })
      
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
                  actionButton(ns(paste0('btn_validate_', name)), 'Perform'))
          )
        )
      })
      
      
      observeEvent(input$btn_validate_Step1, {
        ValidateCurrentPos()
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
                  actionButton(ns(paste0('btn_validate_', name)), 'Perform'))
          )
        )
      })
      
      ## Logics to implement: here, we must take the last data not null
      # in previous datas. The objective is to take account
      # of skipped steps
      observeEvent(input$btn_validate_Step2, {
        ValidateCurrentPos()
      })

      ############### SCREEN 4 ######################################
      output$Step3 <- renderUI({
        name <- 'Step3'
        
        tagList(
          div(id=ns(name),
              div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  tags$h3(name)),
              div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  actionButton(ns(paste0('btn_validate_', name)), 'Validate'))
          )
        )
        
      })
      
      
      observeEvent(input$btn_validate_Step3, {
        # rv$dataIn <- AddItemToDataset(private$rv$dataIn, private$config$process.name)
        ValidateCurrentPos()
      })
      
  }
)
    