#Timeline_R6.R
ProcessA = R6Class(
  "ProcessA",
  inherit = Process,
  private = list(
    .config = list(name = 'ProcessA',
                   steps = c('Description', 'Step1', 'Step2', 'Step3'),
                   mandatory = c(T,F,T,F)
                   )
  ),
  
  public = list(

    Description_listeners = function(){
      observeEvent(self$input$btn_validate_Description, ignoreInit = T, ignoreNULL=T, {
        cat(paste0(class(self)[1], "::observeEvent(self$input$btn_validate_Description from - ", self$id, '\n'))
      })
    },
    
    
   Description_ui = function(){
     tagList(
        actionButton(self$ns('btn_validate_Description'), 
                     paste0('Start ', self$config$name),
                     class = btn_success_color),
        includeMarkdown(paste0('./md/',self$config$name, ".md"))
      )
  },
      
      ############### SCREEN 2 ######################################
      
  Step1_listeners = function(){
    observeEvent(self$input$btn_validate_Step1, ignoreInit = T, {
      print("Action on btn_validate_Step1")
    })
  },
  
  Step1_ui = function(){
        name <- 'Step1'
        
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
  
  Step2_listeners = function(){
    ## Logics to implement: here, we must take the last data not null
    # in previous datas. The objective is to take account
    # of skipped steps
    observeEvent(self$input$btn_validate_Step2, ignoreInit = T, {
      self$ValidateCurrentPos()
    })
        },
  
  Step2_ui = function(){
    name <- 'Step2'
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
  
  Step3_listeners = function(){
    
    observeEvent(self$input$btn_validate_Step3, ignoreInit = T, {
      self$rv$dataIn <- AddItemToDataset(self$rv$dataIn, self$config$name)
      self$ValidateCurrentPos()
    })
    },
  
  Step3_ui = function(){
    name <- 'Step3'
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