#Timeline_R6.R
ProcessB = R6Class(
  "ProcessB",
  inherit = Process,
  private = list(
    .config = list(name = 'ProcessB',
                   steps = c('Description', 'Step1', 'Step2', 'Step3'),
                   mandatory = c(T,F,T,F)
    )
  ),
  
  public = list(
    
    Description = function(){
      observeEvent(self$input$btn_validate_Description, ignoreInit = T, {
        cat(paste0(class(self)[1], '::observeEvent(self$input$btn_validate_Description from - ', self$id, '\n'))
        self$InitializeDataIn()
        self$ValidateCurrentPos()
      })
      
      
      tagList(
        actionButton(self$ns('btn_validate_Description'), 
                     paste0('Start ', self$config$name),
                     class = btn_success_color),
        includeMarkdown(paste0('./md/',self$config$name, ".md"))
      )
    },
    
    ############### SCREEN 2 ######################################
    
    Step1 = function(){
      name <- 'Step1'
      
      observeEvent(self$input$btn_validate_Step1, ignoreInit = T, {
        self$ValidateCurrentPos()
      })
      
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
      
    },
    
    ############### SCREEN 3 ######################################
    Step2 = function(){
      name <- 'Step2'
      
      
      ## Logics to implement: here, we must take the last data not null
      # in previous datas. The objective is to take account
      # of skipped steps
      observeEvent(self$input$btn_validate_Step2, ignoreInit = T, {
        self$ValidateCurrentPos()
      })
      
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
    },
    
    ############### SCREEN 4 ######################################
    Step3 = function(){
      name <- 'Step3'
      
      observeEvent(self$input$btn_validate_Step3, ignoreInit = T, {
        self$rv$dataIn <- AddItemToDataset(self$rv$dataIn, self$config$name)
        self$ValidateCurrentPos()
      })
      
      tagList(
        div(id = self$ns(name),
            div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                tags$h3(name)),
            div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                actionButton(self$ns(paste0('btn_validate_', name)), 'Validate'))
        )
      )
      
    }
    
  )
)