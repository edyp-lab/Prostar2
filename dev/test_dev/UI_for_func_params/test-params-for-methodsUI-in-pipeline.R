##
##
## This example try to use a module to generate the UI interface for the parameters of a method
## called in a pipeline. Tho goal is to simplify the code of a pipeline module by extracting
## the management of variables of the different widgets
##
##


library(shinyjs)
library(DAPAR2)
library(tibble)
library(QFeatures)
library(shiny)



source(file.path('.', 'mod_norm_QuantileCentering_UI.R'), local=TRUE)$value
source(file.path('.', 'mod_norm_vsn_UI.R'), local=TRUE)$value
source(file.path('.', 'mod_norm_loess_UI.R'), local=TRUE)$value



###############################################################################
##
## Light module of normalisation
## 
###############################################################################

mod_norm_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    uiOutput(ns('Screen_Prot_norm_1')),
    actionButton(ns('reset_btn'), 'Reset'),
    actionButton(ns('set_btn'), 'Set')
  )
}



mod_norm_server <- function(input, output, session, obj, ind){
  ns <- session$ns
  
  
  ## reactive values for variables in the module
  rv.norm <- reactiveValues(
    name = "processProtNorm",
    dataIn = NULL,
    dataOut = NULL,
    tmp = NULL,
    widgets = list(method = "None"),
    params_loess = list(),
    params_vsn = list(),
    params_QuantileCentering = list()
  )
  

  observe({
    ## instanciation of the RV in the module with parameters
    req(obj())
    rv.norm$dataIn <- obj()
    rv.norm$dataOut <- obj()
  })
  
 
  ###---------------------------------------------------------------------------------###
  ###                                 Screen 1                                        ###
  ###---------------------------------------------------------------------------------###
  output$Screen_Prot_norm_1 <- renderUI({

      tagList(
        selectInput(ns("normalization.method"),"Normalization method", 
                    choices = c('None'='None', DAPAR2::normalizeMethods.dapar())
                    ),
        
        hidden(div(id=ns("div_params_loess"),mod_params_LOESS_ui(ns("params_loess")))),
        hidden(div(id=ns("div_params_vsn"),mod_params_vsn_ui(ns("params_vsn")))),
        hidden(div(id=ns("div_params_QuantileCentering"),mod_params_QuantileCentering_ui(ns("params_QuantileCentering")))),
        
        actionButton(ns("perform.normalization"), "Perform normalization"))
  })
  
  observeEvent(input$normalization.method,{
    rv.norm$widgets$method <- input$normalization.method
    toggle('div_params_loess', condition=rv.norm$widgets$method=='LOESS')
    toggle('div_params_vsn', condition=rv.norm$widgets$method=='vsn')
    toggle('div_params_QuantileCentering', condition=rv.norm$widgets$method=='QuantileCentering')
  })
  
  
  
  observeEvent(input$reset_btn,{
    rv.norm$params <- NULL
  })
  
  observeEvent(input$set_btn,{
    rv.norm$params <- list(fun='LOESS', args=list(span=10, type='within conditions'))
  })
  
  
  rv.norm$tmp1 <- callModule(mod_params_LOESS_server, 'params_loess', 
                             obj=reactive({rv.norm$dataIn}),
                             paramsIn = reactive({rv.norm$params_loess})  
                             )
  rv.norm$tmp2 <- callModule(mod_params_vsn_server, 'params_vsn', 
                             obj=reactive({rv.norm$dataIn}),
                             paramsIn = reactive({rv.norm$params_vsn})
                             )
  rv.norm$tmp3 <- callModule(mod_params_QuantileCentering_server, 'params_QuantileCentering', 
                             obj=reactive({rv.norm$dataIn}),
                             paramsIn = reactive({rv.norm$params_QuantileCentering})
                             )
  
  observeEvent(rv.norm$tmp1,{ rv.norm$params_loess <- rv.norm$tmp1()})
  observeEvent(rv.norm$tmp2,{ rv.norm$params_vsn <- rv.norm$tmp2()})
  observeEvent(rv.norm$tmp3,{ rv.norm$params_QuantileCentering <- rv.norm$tmp3()})
  
  
  ##' Reactive behavior : Normalization of data
  ##' @author Samuel Wieczorek
  observeEvent(input$perform.normalization,{
    rv.norm$widgets$method
    rv.norm$dataIn
    
    
  # browser()
    ll <- list(object = rv.norm$dataIn,
               i = length(names(rv.norm$dataIn)),
               name = "proteins_norm",
               method = rv.norm$widgets$method
    )
    
    switch(rv.norm$widgets$method, 
           None = rv.norm$dataIn <- obj(),
           
           GlobalQuantileAlignment = {
             
           },
           
           QuantileCentering = {
             ll <- append(ll, list(subset.norm = NULL))
             
             rv.norm$dataOut <- do.call(normalizeD, append(ll,  
                                                           rv.norm$params_QuantileCentering$args)
             )
           },
           LOESS = {
             ll <- append(ll, rv.norm$params_loess$args)
             rv.norm$dataOut <- do.call(normalizeD, ll)
           },
           vsn = {
             browser()
             ll <- append(ll, rv.norm$params_vsn$args)
             rv.norm$dataOut <- do.call(normalizeD, ll)
           }
    )
     
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
    uiOutput('QF'),
    br(),
    uiOutput('metadata'),
    hr(),
    mod_norm_ui('view')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  rv <- reactiveValues(
    res = Exp1_R25_prot
  )
  
  rv$res <- callModule(mod_norm_server,id = "view",
             obj= reactive({Exp1_R25_prot}),
             ind = reactive({2})
             )
  
  
  output$QF <- renderUI({
    rv$res
    print(paste0(names(rv$res()), collapse=', '))
  })
  
  output$metadata <- renderUI({
    req(metadata(rv$res()[[length(names(rv$res()))]])$Params)
     print(metadata(rv$res()[[length(names(rv$res()))]])$Params)
  })
  
}


shinyApp(ui, server)