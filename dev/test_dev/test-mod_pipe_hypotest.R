source(file.path('../../R', 'global.R'), local=TRUE)$value
source(file.path('../../R', 'mod_navigation.R'), local=TRUE)$value
source(file.path('../../R', 'mod_pipe_hypotest.R'), local=TRUE)$value


library(QFeatures)
library(shiny)
library(shinyjs)
library(highcharter)
library(DAPAR2)




ui <- fluidPage(
  mod_pipe_hypotest_ui('pipe_hypothesis_test')
)


server <- function(input, output, session) {
  
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  obj<-filterNA(Exp1_R25_prot,pNA=0,i=2)
  
  
  rv <- reactiveValues(
    test = obj
  )
  
  
  rv$test <- callModule(mod_pipe_hypotest_server,'pipe_hypothesis_test',
                        obj=reactive({obj}),
                        ind = reactive({2})
                        )
  
  
}

shinyApp(ui, server)                    
