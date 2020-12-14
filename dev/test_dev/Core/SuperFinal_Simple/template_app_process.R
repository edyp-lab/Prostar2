

Pipeline <- R6Class(
  "Pipeline",
  public = list(

    initialize = function(id){
      self$id <- id
    },
    

    Launch_Servers = function(data){
      self$child.process <- setNames(lapply(names(self$child.process),
                                            function(x){
                                              assign(x, get(x))$new(x)
                                            }),
                                     names(self$child.process)
      )
      
      lapply(names(self$child.process), function(x){
        self$tmp.return[[x]] <- self$child.process[[x]]$server(dataIn = reactive({data()}))
      })
      
    },
    
    ui = function() {
      uiOutput(ns('show_ui'))
    },
    
    server = function(dataIn ) {
      
      moduleServer(self$id, function(input, output, session) {
        ns <- NS(self$id)
        
        output$show_ui <- renderUI({
         lapply(names(self$child.process), function(x){
              wellPanel(h3(x), self$child.process[[x]]$ui())
            })
        })
        
        
        
      })
    }
  )
)


Pipeline <- Pipeline$new('App')
ui = fluidPage(
   Pipeline$ui()
)

server = function(input, output){
  Pipeline$Launch_Servers(data = reactive({rv$dataIn}))
  Pipeline$server(dataIn = reactive({rv$dataIn}))

}
shiny::shinyApp(ui, server)