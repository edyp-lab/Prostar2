##
##
## This example try to use a module to generate the UI interface for the parameters of a method
## called in a pipeline. Tho goal is to simplify the code of a pipeline module by extracting
## the management of variables of the different widgets
##
##



source(file.path('../../R', 'config.R'), local=TRUE)$value
source(file.path('../../R', 'global.R'), local=TRUE)$value
source(file.path('../../R', 'mod_pipe_prot_norm.R'), local=TRUE)$value


library(shinyjs)
library(DAPAR2)
library(tibble)
library(QFeatures)
library(shiny)

##
##
## Module which create a little ui for the widgets of a function
## 
##
mod_params_norm_ui <- function(id){
  ns <- NS(id)
  tagList(
    
  )
}


mod_params_norm_ui <- function(input, output, session, paramsIn){
  ns <- session$ns
  
  rv <- reactiveValues(
    paramsIn = NULL,
    paramsOut = NULL
  )
  
}




##
##
## Light module of normalisation
## 
##

mod_pipe_prot_norm_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('Screen_Prot_norm_1'))
  )
}



mod_pipe_prot_norm_server <- function(input, output, session, obj, ind){
  ns <- session$ns
  
  
  ## reactive values for variables in the module
  rv.norm <- reactiveValues(
    name = "processProtNorm",
    dataIn = NULL,
    i = NULL,
    dwidgets = list(method = "None",
                   type = "None",
                   varReduction = FALSE,
                   quantile = 0.15,
                   spanLOESS = 0.7)
  )
  
  
  # observeEvent(input$btn_reset,{
  #   
  #   rv.norm$widgets$method <- "None"
  #   rv.norm$widgets$type <- "None"
  #   rv.norm$widgets$varReduction <- FALSE
  #   rv.norm$widgets$quantile <- 0.15
  #   rv.norm$widgets$spanLOESS <- 0.7
  # 
  #   rv.norm$dataIn <- obj()
  #   rv.norm$i <- ind()
  # })
  # 
  
  
  observe({
    ## instanciation of the RV in the module with parameters
    req(obj())
    rv.norm$dataIn <- obj()
    rv.norm$i <- ind()
  })
  
  
  ###---------------------------------------------------------------------------------###
  ###                                 Screen 1                                        ###
  ###---------------------------------------------------------------------------------###
  output$Screen_Prot_norm_1 <- renderUI({
    isolate({
      tagList(
        div(
          div(
            style="display:inline-block; vertical-align: middle; padding-right: 20px;",
            selectInput(ns("normalization.method"),"Normalization method", 
                        choices = DAPAR2::normalizeMethods.dapar(), 
                        selected = rv.norm$widgets$method,
                        width='200px')
          ),
          div(
            style="display:inline-block; vertical-align: middle; padding-right: 20px;",
            hidden(selectInput(ns("normalization.type"), "Normalization type",  
                               choices = c("overall", "within conditions"), 
                               selected = rv.norm$widgets$type,
                               width='150px'))
          ),
          div(
            style="display:inline-block; vertical-align: middle; padding-right: 20px;",
            hidden(textInput(ns("spanLOESS"), "Span",value = rv.norm$widgets$spanLOESS, width='100px')),
            uiOutput(ns("test_spanLOESS")),
            uiOutput(ns("choose_normalizationQuantile")),
            uiOutput(ns("choose_normalizationScaling"))
          ),
          div(
            style="display:inline-block; vertical-align: middle; padding-right: 20px;",
            hidden(actionButton(ns("perform.normalization"), "Perform normalization", class = actionBtnClass, width="170px"))
          )
        )
        )
    })
    
  })
  
  
  
  observeEvent(input$normalization.method, ignoreInit=TRUE,{
    rv.norm$widgets$method <- input$normalization.method
  })
  observeEvent(input$normalization.type, ignoreInit=TRUE,{
    rv.norm$widgets$type <- input$normalization.type
  })
  observeEvent(input$normalization.variance.reduction, ignoreInit=TRUE,{
    rv.norm$widgets$varReduction <- input$normalization.variance.reduction
  })
  observeEvent(input$normalization.quantile, ignoreInit=TRUE,{
    rv.norm$widgets$quantile <- input$normalization.quantile
  })
  observeEvent(input$spanLOESS, ignoreInit=TRUE,{
    rv.norm$widgets$spanLOESS <- input$spanLOESS
  })
  

  output$test_spanLOESS <- renderUI({
    req(rv.norm$widgets$spanLOESS)
    if (!is.numeric(rv.norm$widgets$spanLOESS)){
      tags$p("Please choose a number.")
    }
  })
  
  
  output$test_normQuant <- renderUI({
    req(rv.norm$widgets$quantile)
    if (!is.numeric(rv.norm$widgets$quantile)){
      tags$p("Please choose a number.")
    }
  })
  
 
  
  output$choose_normalizationQuantile <- renderUI({
    req(rv.norm$widgets$method)
    if (rv.norm$widgets$method != "QuantileCentering") { return (NULL)}
    
    tagList(
      textInput(ns("normalization.quantile"), NULL,
                value = rv.norm$widgets$quantile,width='150px'),
      uiOutput(ns("test_normQuant"))
    )
    
  })
  
  
  
  
  
  output$choose_normalizationScaling <- renderUI({
    req(rv.norm$widgets$method)
    
    if (rv.norm$widgets$method == "MeanCentering"){
      
      checkboxInput(ns("normalization.variance.reduction"), 
                    "Include variance reduction",  
                    value = rv.norm$widgets$varReduction)
    }
    
  })
  
  
  
  ##' Reactive behavior : Normalization of data
  ##' @author Samuel Wieczorek
  observeEvent(input$perform.normalization,{
    rv.norm$widgets$method
    rv.norm$dataIn
    # isolate({
    conds <- colData(rv.norm$dataIn)$Condition
    
    ## the dataset whihc will be normalized is always the original one
    rv.norm$dataIn <- obj()
    rv.norm$i <- ind()
    
    switch(rv.norm$widgets$method, 
           None = rv.norm$dataIn <- obj(),
           
           GlobalQuantileAlignment = {
             rv.norm$dataIn <- normalizeD(object = rv.norm$dataIn,
                                          i = rv.norm$i,
                                          name = "proteins_norm",
                                          method='GlobalQuantileAlignment'
             )
           },
           
           QuantileCentering = {
             quant <-NA
             if (!is.null(rv.norm$widgets$quantile))
               quant <- as.numeric(rv.norm$widgets$quantile)
             
             rv.norm$dataIn <- normalizeD(object = rv.norm$dataIn, 
                                          i = rv.norm$i, 
                                          name = "proteins_norm",
                                          method = 'QuantileCentering', 
                                          conds = conds, 
                                          type = rv.norm$widgets$type,
                                          quantile = quant
             )
             
           } ,
           
           MeanCentering = {
             rv.norm$dataIn <- normalizeD(object =rv.norm$dataIn,
                                          i = rv.norm$i, 
                                          name ="proteins_norm",
                                          method = 'MeanCentering', 
                                          conds = conds, 
                                          type = rv.norm$widgets$type,
                                          scaling = rv.norm$widgets$varReduction
             )
           }, 
           
           SumByColumns = {
             rv.norm$dataIn <- normalizeD(object = rv.norm$dataIn,
                                          i =rv.norm$i,
                                          name = "proteins_norm",
                                          method = 'SumByColumns', 
                                          conds = conds, 
                                          type = rv.norm$widgets$type
             )
           },
           
           LOESS = { 
             rv.norm$dataIn <- normalizeD(object = rv.norm$dataIn,
                                          i = rv.norm$i,
                                          name = "proteins_norm",
                                          method = 'LOESS', 
                                          conds = conds, 
                                          type = rv.norm$widgets$type,
                                          span = as.numeric(rv.norm$widgets$spanLOESS)
             )
           },
           
           vsn = {
             rv.norm$dataIn <- normalizeD(object = rv.norm$dataIn,
                                          i = rv.norm$i, 
                                          name = "proteins_norm",
                                          method = 'vsn', 
                                          conds = conds, 
                                          type = rv.norm$widgets$type)
           }
    )
    # })
    
    rv.norm$i <- ind() + 1
    rv.norm$dataOut <- rv.norm$dataIn
  })
  
  
  return({reactive(rv.norm$dataOut)})
  
}









##
##
## Code for the shiny App example
## 
##


ui <- fluidPage(
  tagList(
    mod_pipe_prot_norm_ui('view')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  
  callModule(mod_pipe_prot_norm_server,id = "view",
             obj= reactive({Exp1_R25_prot}),
             ind = reactive({2})
             )
  
  
}


shinyApp(ui, server)