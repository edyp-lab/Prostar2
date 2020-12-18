#Timeline_R6.R
ProcessC = R6Class(
  "ProcessC",
  inherit = Process,
  private = list(
    .config = list(name = 'ProcessC',
                   steps = c('Description', 'Step1', 'Step2', 'Step3'),
                   mandatory = c(T,F,T,F)
    )
  ),
  
  public = list(
    
    Description_server = function(input, output){
      observeEvent(input$btn_validate_Description, ignoreInit = T, ignoreNULL=T, {
        cat(paste0(class(self)[1], "::observeEvent(input$btn_validate_Description from - ", self$id, '\n'))
        private$InitializeDataIn()
        private$ValidateCurrentPos()
      })
      
      output$datasetDescription <- renderUI({
        tagList(
          p(paste0('Dataset description: ', paste0(names(self$rv$temp.dataIn), collapse=", ")))
        )
      })
    },
    
    
    Description_ui = function(){
      
      
      wellPanel(
        tagList(
          actionButton(self$ns('btn_validate_Description'), 
                       paste0('Start ', self$config$name),
                       class = btn_success_color),
          includeMarkdown(paste0('./md/',self$config$name, ".md")),
          uiOutput(self$ns('datasetDescription'))
        )
      )
    },
    
    ############### SCREEN 2 ######################################
    
    Step1_server = function(input, output){
      observeEvent(input$btn_validate_Step1, ignoreInit = T, {
        print("Action on btn_validate_Step1")
        private$ValidateCurrentPos()
      })
    },
    
    Step1_ui = function(){
      name <- 'Step1'
      wellPanel(
        tagList(
          div(id=self$ns(name),
              div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  tags$h2(name)),
              div(style="display:inline-block; vertical-align: middle; padding-right: 40px;",
                  selectInput(self$ns('select1'), 'Select step 1', 
                              choices = 1:5, 
                              selected = 1,
                              width = '150px')
              ),
              div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  actionButton(self$ns(paste0('btn_validate_', name)), 'Perform'))
          )
        )
      )
    },
    
    Step2_server = function(input, output){
      ## Logics to implement: here, we must take the last data not null
      # in previous datas. The objective is to take account
      # of skipped steps
      observeEvent(input$btn_validate_Step2, ignoreInit = T, {
        private$ValidateCurrentPos()
      })
    },
    
    Step2_ui = function(){
      name <- 'Step2'
      wellPanel(
        tagList(
          div(id=self$ns(name),
              div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  tags$h3(name)),
              div(style="display:inline-block; vertical-align: middle;padding-right: 40px;",
                  selectInput(self$ns('select2'), 'Select step 2',
                              choices = 1:5,
                              selected = 1,
                              width = '150px')),
              div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  actionButton(self$ns(paste0('btn_validate_', name)), 'Perform'))
          )
        )
      )
    },
    
    Step3_server = function(input, output){
      
      observeEvent(input$btn_validate_Step3, ignoreInit = T, {
        self$rv$dataIn <- AddItemToDataset(self$rv$dataIn, self$config$name)
        private$ValidateCurrentPos()
      })
    },
    
    Step3_ui = function(){
      name <- 'Step3'
      wellPanel(
        tagList(
          div(id = self$ns(name),
              div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  tags$h3(name)),
              div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  actionButton(self$ns(paste0('btn_validate_', name)), 'Validate'))
          )
        )
      )
    }
  )
)