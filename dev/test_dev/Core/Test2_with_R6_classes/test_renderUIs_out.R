Child <- R6Class(
  "Child",
  inherit=Mother,
  private = list( ),
  public = list(
    id = NULL,
    RenderUIs = NULL,
    config = NULL,
    
    initialize = function(id, config){
      self$id <- id
      self$config <- config
      self$RenderUIs <- RenderUIs
    }

  )
)

Mother <- R6Class(
  "Mother",
  private = list( ),
  public = list(
    id = NULL,
    initialize = function(id){},
    
    ui = function(){
      ns <- NS(self$id)
      tagList(
        uiOutput(ns('screen'))
      )
    },
    
    
    PrintChange = function(){
      print(paste0("Event caught on input$change : ", paste0(config$steps, collapse=' ')))
            },
    
    server = function(fun){
      ns <- NS(self$id)
      inner = 3
      config = reactiveValues(
        steps = "toto"
      )
      moduleServer(self$id, function(input, output, session) {
        ns <- NS(self$id)
        output$screen <- renderUI({ uiOutput(ns('Description'))})
        
        fun(input, output, self)

      }
      
      )

    }
  )
)


######
config = list(process.name = 'ProcessA',
              steps = c('Description', 'Step1', 'Step2', 'Step3'),
              mandatory = setNames(c(F,F,F,F), c('Description', 'Step1', 'Step2', 'Step3'))
)

RenderUIs = function(input, output, self){
  ns <- NS(self$id)
  output$Description <- renderUI({
    tagList(
      p(self$id),
      actionButton(ns('change'), "change")
    )})
  
  observeEvent(input$change,{
    print("change")
    self$PrintChange()})
}




app1 <- Child$new('test', config1)
app1 <- Child$new('test', config)

ui = fluidPage(app$ui())

server = function(input, output){
  app$server(RenderUIs)
}

shiny::shinyApp(ui, server)