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


###############################################################################
##
## Module which create a little ui for the widgets of a function
## 
###############################################################################

mod_params_QuantileCentering_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('QuantileCentering_ui'))
  )
}


mod_params_QuantileCentering_server <- function(input, output, session, paramsIn){
  ns <- session$ns
  
  params <- reactiveValues(
    fun = 'QuantileCentering',
    args = list(
      type = "overall",
      quantile = xxx,
      subset.norm = NULL
    )
  )
  
  
  observeEvent(paramsIn(), ignoreNULL = FALSE, {
    
    if (is.null(paramsIn()) || length(paramsIn())==0){
      params$fun = 'QuantileCentering'
      params$args$type = "overall",
      quantile = xxx,
      params$args$subset.norm = NULL
    } else {
      params$args$type = paramsIn()$args$type
      params$args$quantile = paramsIn()$args$quantile
      params$args$subset.norm = paramsIn()$args$subset.norm
    }
  })
  
  
  
  observeEvent(input$normalization.type, ignoreInit=TRUE,{params$args$type <- input$normalization.type})
  
  
  output$loess_ui <- renderUI({
    
    tagList(
      selectInput(ns("normalization.type"), "Normalization type",  
                  choices = c("None", "overall", "within conditions"), 
                  selected = params$args$type)
    )
  })
  
  return(reactive({params}))
}









###############################################################################
##
## Module which create a little ui for the widgets of a function
## 
###############################################################################

mod_params_vsn_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('vsn_ui'))
  )
}


mod_params_vsn_server <- function(input, output, session, paramsIn){
  ns <- session$ns
  
  params <- reactiveValues(
    fun = 'vsn',
    args = list(
      type = "overall"
    )
  )
  
  
  observeEvent(paramsIn(), ignoreNULL = FALSE, {
    
    if (is.null(paramsIn()) || length(paramsIn())==0){
      params$fun = 'vsn'
      params$args$type = "overall"
    } else {
      params$args$type = paramsIn()$args$type
    }
  })
  
  
  
   observeEvent(input$normalization.type, ignoreInit=TRUE,{params$args$type <- input$normalization.type})
  
  
  output$loess_ui <- renderUI({
    
    tagList(
      selectInput(ns("normalization.type"), "Normalization type",  
                  choices = c("None", "overall", "within conditions"), 
                  selected = params$args$type)
    )
  })

  return(reactive({params}))
}






###############################################################################
##
## Module which create a little ui for the widgets of a function
## 
###############################################################################

mod_params_LOESS_ui <- function(id){
  ns <- NS(id)
   tagList(
     uiOutput(ns('loess_ui'))
   )
}


mod_params_LOESS_server <- function(input, output, session, paramsIn){
  ns <- session$ns
  
  params <- reactiveValues(
    fun = 'loess',
    args = list(
      span = 0.7,
      type = "overall"
    )
  )
  
  
  observeEvent(paramsIn(), ignoreNULL = FALSE, {

    if (is.null(paramsIn()) || length(paramsIn())==0){
      params$fun = 'loess'
      params$args$span=0.7
      params$args$type = "overall"
    } else {
      params$args$span= paramsIn()$args$span
      params$args$type = paramsIn()$args$type
    }
  })
  

  
  observeEvent(input$spanLOESS, ignoreInit=TRUE,{ params$args$span <- input$spanLOESS })
  observeEvent(input$normalization.type, ignoreInit=TRUE,{params$args$type <- input$normalization.type})
    
    
  output$loess_ui <- renderUI({

    tagList(
      selectInput(ns("normalization.type"), "Normalization type",  
                choices = c("None", "overall", "within conditions"), 
                selected = params$args$type),
    textInput(ns("spanLOESS"), "Span",value = params$args$span)
    )
  })
    
  
  
  return(reactive({params}))
}




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
    params = list()
  )
  

  observe({
    ## instanciation of the RV in the module with parameters
    req(obj())
    rv.norm$dataIn <- obj()
    rv.norm$dataOut <- obj()
  })
  
  rv.norm$tmp <- callModule(mod_params_LOESS_server, 'params_loess', paramsIn = reactive({rv.norm$params})  )
  rv.norm$tmp <- callModule(mod_params_LOESS_server, 'params_vsn', paramsIn = reactive({rv.norm$params})  )
  observeEvent(rv.norm$tmp,{ rv.norm$params <- rv.norm$tmp()})
  
  ###---------------------------------------------------------------------------------###
  ###                                 Screen 1                                        ###
  ###---------------------------------------------------------------------------------###
  output$Screen_Prot_norm_1 <- renderUI({

      tagList(
        selectInput(ns("normalization.method"),"Normalization method", 
                    choices = c('None'='None', DAPAR2::normalizeMethods.dapar())
                    ),
        
        hidden(div(id=ns("div_params_loess"),mod_params_LOESS_ui(ns("params_loess")))),
        hidden(div(id=ns("div_params_vsn"),mod_params_LOESS_ui(ns("params_vsn")))),
        
        actionButton(ns("perform.normalization"), "Perform normalization"))
  })
  
 
  observeEvent(input$reset_btn,{
    rv.norm$params <- NULL
  })
  
  observeEvent(input$set_btn,{
    rv.norm$params <- list(fun='LOESS', args=list(span=10, type='within conditions'))
  })
  
  observeEvent(input$normalization.method,{
    rv.norm$widgets$method <- input$normalization.method
   toggle('div_params_loess', condition=rv.norm$widgets$method=='LOESS')
   toggle('div_params_vsn', condition=rv.norm$widgets$method=='vsn')
  })
 
  
  ##' Reactive behavior : Normalization of data
  ##' @author Samuel Wieczorek
  observeEvent(input$perform.normalization,{
    rv.norm$widgets$method
    rv.norm$dataIn
    
    
  # browser()
     ll <- list(object = rv.norm$dataIn,
                i = length(names(rv.norm$dataIn)),
                name = "proteins_norm",
                method = rv.norm$widgets$method,
                conds = colData(rv.norm$dataIn)$Condition
                )
     
     rv.norm$dataOut <- do.call(normalizeD, append(ll,  rv.norm$params$args) )

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