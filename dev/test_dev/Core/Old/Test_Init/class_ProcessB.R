#Timeline_R6.R
ProcessB = R6Class(
  "ProcessB",
  inherit = Process,
  private = list(
    .config = list(name = 'ProcessB',
                   steps = c('Description', 'Step1', 'Step2'),
                   mandatory = c(T, F, F)
    )
  ),
  
  public = list(
    
    Add_RenderUIs_Definitions = function(input, output){
      cat(paste0(class(self)[1], '::', 'Add_RenderUIs_Definitions()\n'))
      ns <- NS(self$id)
      output$Description <- renderUI({
        tagList(
          actionButton(ns('btn_validate_Description'), 
                       paste0('Start ', self$config$name),
                       class = btn_success_color),
          mod_insert_md_ui(ns(paste0(self$config$name, "_md")))
        )
      })
      
      observe({
        mod_insert_md_server(paste0(self$config$name, "_md"), 
                             paste0('./md/', self$config$name, '.md'))
      })
      
      observeEvent(input$btn_validate_Description, {
        self$InitializeDataIn()
        self$ValidateCurrentPos()
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
                  actionButton(ns(paste0('btn_validate_', name)), 'Perform'))
          )
        )
      })
      
      ## Logics to implement: here, we must take the last data not null
      # in previous datas. The objective is to take account
      # of skipped steps
      observeEvent(input$btn_validate_Step2, {
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
                  actionButton(ns(paste0('btn_validate_', name)), 'Validate'))
          )
        )
        
      })
      
      
      observeEvent(input$btn_validate_Step3, {
        # rv$dataIn <- AddItemToDataset(self$rv$dataIn, self$config$name)
        self$ValidateCurrentPos()
      })
      
    }
    
  )
)