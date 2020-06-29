#' pipe_prot_norm UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_pipe_process_ui <- function(id){
  ns <- NS(id)
  tagList(
    dataTableOutput(ns('iris_before')),
    mod_navigation_ui(ns('nav_pipe_process')),
    dataTableOutput(ns('iris_after'))
  )
}

#' pipe_process Server Function
#'
#' @noRd
#' 
#' @param input,output,session
#' 
#' @param obj
#' 
#' @param samplesTab
#' 
mod_pipe_process_server <- function(input, output, session, obj, ind){
  ns <- session$ns
  
  
  ## Section navigation module
  # Variable to manage the different screens of the module
  r.nav <- reactiveValues(
    name = "Foo",
    stepsNames = c("Normalization", "Save"),
    ll.UI = list( screenStep1 = uiOutput(ns("Screen_Process_1")),
                  screenStep2 = uiOutput(ns("Screen_Process_2")),
                  screenStep3 = uiOutput(ns("Screen_Process_3"))
    ),
    isDone =  rep(FALSE,3),
    mandatory =  rep(TRUE,3),
    reset = FALSE
  )
  
  ## reactive values for variables in the module
  rv.process <- reactiveValues(
    name = "processProtNorm",
    i = NULL,
    dataIn = NULL,
    dataOut = NULL,
    widgets = list(column = "None"),
    settings = NULL,
    
    )
  
  
  observeEvent(req(r.nav$reset),{
    
    rv.process$widgets$column <- "None"

    ## do not modify this part
    rv.process$dataIn <- obj()
    rv.process$i <- ind()
    r.nav$isDone <- rep(FALSE, 3)
    r.nav$reset <- FALSE
    ## end of no modifiable part
  })
  
  
  callModule(mod_navigation_server, 'nav_pipe_prot_norm', style=2, pages=r.nav)
  
  #### END of template part of the module
  
  
  
  ##
  ##  
  ## Calls to other modules
  ##
  ##
  
  # Add new modules via 'callModule'
  # for example, the module for settings: mod_settings
  
  rv.process$settings <- callModule(mod_settings_server,
                                 "settings", 
                                 obj = reactive({obj()}))
  
  
 
  
  observe({
    ## instanciation of the RV in the module with parameters
    req(obj())
    rv.process$dataIn <- obj()
    rv.process$i <- ind()
  })
  
  
  
  
  ##
  ## Definitions of the screens
  ##
  
  ###---------------------------------------------------------------------------------###
  ###                                 Screen 1                                        ###
  ###---------------------------------------------------------------------------------###
  output$Screen_Process_1 <- renderUI({
    
    
  })
  
  
  
  observeEvent(input$column, ignoreInit=TRUE,{
    rv.process$widgets$column <- input$column
  })
  
  
  ##' Reactive behavior : Normalization of data
  ##' @author Samuel Wieczorek
  observeEvent(input$perform.normalization,{
    rv.process$widgets$method
    rv.process$dataIn
    # isolate({
    conds <- colData(rv.process$dataIn)$Condition
    
    ## the dataset whihc will be normalized is always the original one
    rv.process$dataIn <- obj()
    rv.process$i <- ind()
    
    switch(rv.process$widgets$method, 
           None = rv.process$dataIn <- obj(),
           
           GlobalQuantileAlignment = {
             rv.process$dataIn <- normalizeD(object = rv.process$dataIn,
                                          i = rv.process$i,
                                          name = "protein_norm",
                                          method='GlobalQuantileAlignment'
             )
           },
           
           QuantileCentering = {
             quant <-NA
             if (!is.null(rv.process$widgets$quantile))
               quant <- as.numeric(rv.process$widgets$quantile)
             
             rv.process$dataIn <- normalizeD(object = rv.process$dataIn, 
                                          i = rv.process$i, 
                                          name = "proteins_norm",
                                          method = 'QuantileCentering', 
                                          conds = conds, 
                                          type = rv.process$widgets$type,
                                          subset.norm = GetIndicesOfSelectedProteins(), 
                                          quantile = quant
             )
             
           } ,
           
           MeanCentering = {
             rv.process$dataIn <- normalizeD(object =rv.process$dataIn,
                                          i = rv.process$i, 
                                          name ="proteins_norm",
                                          method = 'MeanCentering', 
                                          conds = conds, 
                                          type = rv.process$widgets$type,
                                          subset.norm = GetIndicesOfSelectedProteins(), 
                                          scaling = rv.process$widgets$varReduction
             )
           }, 
           
           SumByColumns = {
             rv.process$dataIn <- normalizeD(object = rv.process$dataIn,
                                          i =rv.process$i,
                                          name = "proteins_norm",
                                          method = 'SumByColumns', 
                                          conds = conds, 
                                          type = rv.process$widgets$type,
                                          subset.norm = GetIndicesOfSelectedProteins()
             )
           },
           
           LOESS = { 
             rv.process$dataIn <- normalizeD(object = rv.process$dataIn,
                                          i = rv.process$i,
                                          name = "proteins_norm",
                                          method = 'LOESS', 
                                          conds = conds, 
                                          type = rv.process$widgets$type,
                                          span = as.numeric(rv.process$widgets$spanLOESS)
             )
           },
           
           vsn = {
             rv.process$dataIn <- normalizeD(object = rv.process$dataIn,
                                          i = rv.process$i, 
                                          name = "proteins_norm",
                                          method = 'vsn', 
                                          conds = conds, 
                                          type = rv.process$widgets$type)
           }
    )
    # })
    
    rv.process$i <- ind() + 1
    r.nav$isDone[1] <- TRUE
    #shinyjs::hide("perform.normalization")
  })
  
  
  
  
  
  
  
  #######################
  output$viewComparisonNorm_UI <- renderHighchart({
    rv.process$settings()$basePalette
    req(rv.process$dataIn)
    obj()
    GetIndicesOfSelectedProteins()
    print(GetIndicesOfSelectedProteins())
    
    hc <- DAPAR2::compareNormalizationD_HC(qDataBefore = assay(obj()[[ind()]]),
                                           qDataAfter = assay(rv.process$dataIn[[rv.process$i]]),
                                           conds= colData(obj())$Condition,
                                           palette = rv.process$settings()$basePalette,
                                           subset.view= GetIndicesOfSelectedProteins(),
                                           n = 50)
    hc
  })
  
  
  
  
  #######################
  
  #viewComparisonNorm2 <- reactive({
  #  rv$PlotParams$paletteConditions
  #  leg <- NULL
  #  grp <- NULL
  #  
  #  labelsNorm <- NULL
  #  labelsToShowNorm <- NULL
  #  gToColorNorm <- NULL
  #  
  #  labelsToShowNorm <- c(1:nrow(Biobase::pData(rv$current.obj)))
  #  
  #  
  #  
  #  if (is.null(rv$whichGroup2Color) 
  #      || (rv$whichGroup2Color == "Condition")){
  #    labelsNorm <- Biobase::pData(rv$current.obj)[,"Condition"]
  #  }else {
  #    labelsNorm <- paste(Biobase::pData(rv$current.obj)[,"Condition"],
  #                        Biobase::pData(rv$current.obj)[,"Bio.Rep"],
  #                        Biobase::pData(rv$current.obj)[,"Tech.Rep"],
  #                        Biobase::pData(rv$current.obj)[,"Analyt.Rep"],
  #                        sep= "_")
  #  }
  #  
  #  
  #  if (input$datasets == paste0("Normalized.", rv$typeOfDataset)){
  #    obj1 <- rv$dataset[[(which(names(rv$dataset)==dname) - 1)]]
  #    obj2 <- rv$dataset[[input$datasets]]
  #  }
  #  else {
  #    obj1 <-rv$dataset[[input$datasets]]
  #    obj2 <- rv$current.obj
  #    
  #  }
  #  
  #  wrapper.compareNormalizationD(obj1, obj2,
  #                                labelsNorm,
  #                                as.numeric(labelsToShowNorm),
  #                                palette = rv$PlotParams$paletteConditions)
  #  
  #})
  
  
  
  
  
  
  ###---------------------------------------------------------------------------------###
  ###                                 Screen 2                                        ###
  ###---------------------------------------------------------------------------------###
  
  output$Screen_Prot_norm_2 <- renderUI({
    print('screen 2')
    tagList(
      actionButton(ns("valid.normalization"),
                   "Save normalization", 
                   class = actionBtnClass, 
                   width="170px")
    )
    
  })
  
  
  
  ##' -- Validate and save the normalization ---------------------------------------
  ##' @author Samuel Wieczorek
  observeEvent(input$valid.normalization,{ 
    
    if (rv.process$widgets$method != "None") {
      metadata(rv.process$dataIn[[rv.process$i]])$Params <- list(
        method = rv.process$widgets$method,
        type = rv.process$widgets$type,
        varReduction = rv.process$widgets$varReduction,
        quantile = rv.process$widgets$quantile,
        spanLOESS =rv.process$widgets$spanLOESS
      )
      
      rv.process$dataOut <- rv.process$dataIn
      r.nav$isDone[2] <- TRUE
      # UpdateDatasetWidget(rv$current.obj, name)
    }
    
  })
  
  return({reactive(rv.process$dataOut)})
  
}

## To be copied in the UI
# mod_pipe_prot_norm_ui("pipe_prot_norm_ui_1")

## To be copied in the server
# callModule(mod_pipe_prot_norm_server, "pipe_prot_norm_ui_1")

