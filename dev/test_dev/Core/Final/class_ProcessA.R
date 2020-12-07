#Timeline_R6.R
ProcessA = R6Class(
  "ProcessA",
  inherit = Process,
  private = list(
    .config = list(name = 'ProcessA',
                   steps = c('Description', 'Step1', 'Step2', 'Step3'),
                   mandatory = c(T,F,F,F)
                   )
  ),
  
  public = list(

   Description = function(){
     
      observe({
        mod_insert_md_server(paste0(self$name, "_md"), 
                             paste0('./md/', self$name, '.md'))
      })
      
      observeEvent(self$input$btn_validate_Description, {
        self$InitializeDataIn()
        self$ValidateCurrentPos()
      })
      
      tagList(
        actionButton(self$ns('btn_validate_Description'), 
                     paste0('Start ', self$name),
                     class = btn_success_color),
        mod_insert_md_ui(self$ns(paste0(self$name, "_md")))
      )
  },
      
      ############### SCREEN 2 ######################################
      
      Step1 = function(){
        name <- 'Step1'
        
        observeEvent(self$input$btn_validate_Step1, {
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
      observeEvent(self$input$btn_validate_Step2, {
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

      observeEvent(self$input$btn_validate_Step3, {
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