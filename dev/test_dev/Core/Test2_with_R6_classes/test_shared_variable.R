library(R6)


Test <- R6Class(
  "Test",
  inherit = General,
  public = list(
    initialize = function(id,  dataIn){
      self$id <- id
      observe({
        self$SetConfig(list(name = 'Description', 
                            steps = LETTERS[1:sample.int(10,1)]))
        self$SetDataIn(dataIn())
      })
    }
   
  )
)


General <- R6Class(
  "General",
  public = list(
    id = NULL,

    dataOut = reactiveValues(),
    config = reactiveValues(),
    dataIn = reactiveValues(),
    rv = reactiveValues(
      current.pos = 1
    ),
    initialize = function(){ },
    
   
    ui = function(){
      ns <- NS(self$id)
      tagList(
        actionButton(ns('change'), 'Simulate change config'),
        actionButton(ns('send'), 'Send value'),
        actionButton(ns('curpos'), 'Change current position'),
        uiOutput(ns('show_config'))
      )
    },
    
    GetConfig = function(){self$config[[self$id]]},
    SetConfig = function(value){self$config[[self$id]] <- value},
    GetSteps = function(){self$config[[self$id]]$steps},
    SetSteps = function(pos, value){self$config[[self$id]]$steps[pos] <- value},
    GetStatus = function(){self$config[[self$id]]$status},
    SetStatus = function(pos, value){self$config[[self$id]]$status[pos] <- value},
    GetMandatory = function(){self$config[[self$id]]$mandatory},
    SetMandatory = function(pos, value){self$config[[self$id]]$mandatory[pos] <- value},
    GetName = function(){self$config[[self$id]]$name},
    SetName = function(value){self$config[[self$id]]$name <- value},
    GetDataOut = function() {self$dataOut[[self$id]]},
    SetDataOut = function(data){self$dataOut[[self$id]] <- list(value = data,
                                                                trigger = runif(1,0,1)
                                                                )},
    SetDataIn = function(data){
      if(is.na(data)) self$SetDataOut(data)
      self$dataIn[[self$id]] <- data},
    GetDataIn = function(){self$dataIn[[self$id]]},
    
    GetCurrentPos = function(){self$rv[[self$id]]$current.pos},
    SetCurrentPos = function(value){self$rv[[self$id]]$current.pos <- value},
    
    server = function(dataIn){
      ns <- NS(self$id)
     
      moduleServer(self$id, function(input, output, session) {
        ns <- NS(self$id)
        
        output$show_config <- renderUI({ 
          self$GetDataIn()
          tagList(
            p(paste0("steps = ", paste0(self$GetSteps(), collapse=' '))),
            p(paste0(', dataIn = ', self$GetDataIn())),
            p(paste0('Current.pos = ', self$GetCurrentPos()))
            )
          })
        
        
        observeEvent(input$curpos, {self$SetCurrentPos(input$curpos)})
        
        observeEvent(input$change,{
          self$SetSteps(1, input$change%%2 ==0)
        })
        
        observeEvent(input$send,{
          self$SetDataOut(self$GetSteps())
        })
        
      }
      
      )
      reactive({self$GetDataOut()})
      
    }
  )
)


Super <- R6Class(
  "Super",
  public = list(
    id = NULL,
    ll.process = list(
      Description = NULL,
      processA = NULL
    ),
    rv = reactiveValues(
      dataIn = 3),
  steps = setNames(c('Description', 'processA'),c('Description', 'processA')),

initialize = function(id){
  self$id <- id
},
res = reactiveValues(),
tmp.return = reactiveValues(),
ui = function(){
  ns <- NS(self$id)
  fluidPage(
    tagList(
      actionButton(ns('sendToProcesses'), "Send dataIn"),
      uiOutput(ns('screen')),
      uiOutput(ns('res'))
    )
  )
  },
server = function(input, output, session) {
  ns <- NS(self$id)
  
lapply(names(self$ll.process), function(x){
  self$ll.process[[x]] <- Test$new(ns(x), dataIn = reactive({self$rv$dataIn}))
})
  
  
  observeEvent(self$tmp.return[['Description']]()$trigger, {
    print("change in Description")
    print(paste0("self$rv$tmp.return[['Description']]()= ", paste0(self$tmp.return[['Description']]()$value, collapse=' ')))
  })
  
  observeEvent(self$tmp.return[['processA']]()$trigger, {
    print("change in processA")
    print(paste0("self$rv$tmp.return[['processA']]() = ", paste0(self$tmp.return[['processA']]()$value, collapse=' ')))
  })
  
  # rv$description <- processDescription$server()
  # rv$processA <- processA$server()
  self$tmp.return[['Description']] <- self$ll.process[['Description']]$server()
  self$tmp.return[['processA']] <- self$ll.process[['processA']]$server()
  
  moduleServer(self$id, function(input, output, session) {
    ns <- NS(self$id)
    

  
  output$screen <- renderUI({
    tagList(
      wellPanel(h3('Description'), self$ll.process[['Description']]$ui()),
      wellPanel(h3('process A'), self$ll.process[['processA']]$ui())
      )
    })
  
  observeEvent(input$sendToProcesses, {
    self$rv$dataIn <- input$sendToProcesses
  })

  output$res <- renderUI({
    tagList(
      lapply(names(reactiveValuesToList(self$tmp.return)), function(x){p(paste0(x, ' = ', paste0(self$tmp.return[[x]]()$value, collapse=' ')))})
    )
  })

  })
}
)
)


super <- Super$new('App')
ui = fluidPage(super$ui())
server = function(input, output){super$server()}
shiny::shinyApp(ui, server)

