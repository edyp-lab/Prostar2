#' pipe_prot_norm UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_pipe_prot_norm_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_navigation_ui(ns('nav_pipe_prot_norm'))
  )
}
    
#' pipe_prot_norm Server Function
#'
#' @noRd
#' 
#' @param input,output,session
#' 
#' @param obj
#' 
#' @param samplesTab
#' 
mod_pipe_prot_norm_server <- function(input, output, session, obj, ind){
  ns <- session$ns
  
  
  
  # Variable to manage the different screens of the module
  r.nav <- reactiveValues(
    name = "Normalization",
    stepsNames = c("Normalization", "Save"),
    ll.UI = list( screenStep1 = uiOutput(ns("Screen_Prot_norm_1")),
                  screenStep2 = uiOutput(ns("ScreenProt_norm_2"))
                ),
    isDone =  rep(FALSE,2),
    mandatory =  rep(TRUE,2),
    reset = FALSE
  )
  
  ## reactive values for variables in the module
  rv.norm <- reactiveValues(
    name = "processProtNorm",
    dataIn = NULL,
    settings = NULL,
    # contient l'objet de sortie du module (ie. a MAE instance)
    dataOut = NULL,
    widgets = list(method = "None",
                   type = "None",
                   varReduction = FALSE,
                   quantile = 0.15,
                   spanLOESS = 0.7),
    trackFromBoxplot = NULL,
    selectProt = NULL, 
    resetTracking = FALSE
  )
  
  
  observeEvent(req(r.nav$reset),{
    ## update widgets whose names are in r.widgets with the value in this list
    ## This part must be before the reinitialization of r.nav$isDone
    # updateCheckboxInput(session,'selectIdent', value=NULL)
    # updateSelectInput(session,'convert_proteinId', selected=NULL)
    # updateSelectInput(session,'idBox', selected=NULL)
    # updateRadioButtons(session, "typeOfData", selected=NULL)
    # updateRadioButtons(session, "checkDataLogged", selected=NULL)
    # updateCheckboxInput(session,"replaceAllZeros", value=NULL)
    
    
    rv.norm$widgets$method <- "None"
    rv.norm$widgets$type <- "None"
    rv.norm$widgets$varReduction <- FALSE
    rv.norm$widgets$quantile <- 0.15
    rv.norm$widgets$spanLOESS <- 0.7
    rv.norm$resetTracking <- TRUE
    
    rv.norm$dataIn <- obj()
    
    
    ## do not modify this part
    r.nav$isDone <- rep(FALSE, 2)
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
  callModule(mod_plots_density_server,
             "densityPlot_Norm",
             obj = reactive({rv.norm$dataIn[[ind()]]}),
             conds = reactive({colData(rv.norm$dataIn)[["Condition"]]}),
             legend = reactive({colData(rv.norm$dataIn)[["Sample.name"]]}),
             base_palette = reactive({rv.norm$settings()$basePalette}))
  
  
  
  callModule(mod_popover_for_help_server,
             "modulePopover_normQuanti", 
             data = reactive(list(title = HTML(paste0("<strong>Normalization quantile</strong>")), 
                                  content="lower limit/noise (quantile = 0.15), median (quantile = 0.5). Min value=0, max value=1")))
  
  rv.norm$selectProt <- callModule(mod_plots_tracking_server, 
                                   "ProtSelection", 
                                   obj = reactive({rv.norm$dataIn}),
                                   params=reactive({NULL}),
                                   keyId = reactive({metadata(obj())[['keyId']]}),
                                   reset = reactive({rv.norm$resetTracking}))

  rv.norm$settings <- callModule(mod_settings_server,
                                 "settings", 
                                 obj = reactive({obj()}))
  
  rv.norm$trackFromBoxplot <- callModule(mod_plots_intensity_server,
                                         "boxPlot_Norm",
                                         dataIn = reactive({rv.norm$dataIn[[ind()]]}),
                                         meta = reactive({metadata(obj())}),
                                         conds = reactive({colData(obj())[['Condition']]}),
                                         params = reactive({if (isTRUE(input$SyncForNorm)) 
                                                                {rv.norm$selectProt()} else {NULL}}),
                                         reset = reactive({rv.norm$resetTracking}),
                                         base_palette = reactive({rv.norm$settings()$basePalette})
                                          )
                                         
                                         
  
  
  ##
  ## Definitions of the screens
  ##
  
  
  observe({
    req(obj())
    rv.norm$dataIn <- obj()
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
            hidden(div(id='DivProtSelection',mod_plots_tracking_ui(ns('ProtSelection'))))
          ),
          div(
            style="display:inline-block; vertical-align: middle; padding-right: 20px;",
            hidden(actionButton(ns("perform.normalization"), "Perform normalization", class = actionBtnClass, width="170px"))
          )
        ),
        uiOutput(ns("helpForNormalizationMethods")),
        tags$hr(),
        fluidRow(
          column(width=4, mod_plots_density_ui(ns("densityPlot_Norm"))),
          column(width=4,
                 hidden(checkboxInput(ns("SyncForNorm"), "Synchronise with selection above", value=FALSE)),
                 withProgress(message = 'Building plot',detail = '', value = 0, {
                   mod_plots_intensity_ui(ns("boxPlot_Norm"))
                 }))
          # column(width=4,withProgress(message = 'Building plot',detail = '', value = 0, {
          #   imageOutput(ns("viewComparisonNorm_DS"))
          # })
          # )
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
  
  output$helpForNormalizationMethods <- renderUI({
    req(rv.norm$widgets$method)
    if (rv.norm$widgets$method == "None") {return(NULL)}
    
    
    switch(rv.norm$widgets$method,
           GlobalQuantileAlignment= txt <- "This method proposes a normalization of important
         magnitude that should be cautiously used. It proposes to align the quantiles of all 
         the replicates as described in [Other ref. 1]; practically it amounts to replace 
         abundances by order statistics.",
           QuantileCentering = txt <- "These methods propose to shift the sample distributions 
         (either all of them at once, or within each condition at a time) to align a specific 
         quantile: the median (under the assumption that up-regulations and down-regulations 
         are equally frequent), the 15% quantile (under the assumption that the signal/noise ratio is 
         roughly the same in all the samples), or any other user's choice.",
           MeanCentering = txt <- "These methods propose to shift the sample distributions (either all 
         of them at once, or within each condition at a time) to align their means. It is also possible 
         to force unit variance (or not).",
           SumByColumns = txt <- "These methods propose normalizations of important magnitude that should be cautiously used.
         It operates on the original scale (not the log2 one) and propose to normalize each abundance by the 
         total abundance of the sample (so as to focus on the analyte proportions among each sample).",
           LOESS = txt <- "This method proposes to apply a cyclic LOESS [Other ref. 4, 5] normalization to the data 
         (either all of them at once, or on each condition independently). It relates to  a 
         combination of multiple regression models. The user can tune the regression span (an higher span smooths
         the fit more, while a lower span captures more trends).",
           vsn = txt <- "This method proposes to apply the Variance Stabilization Normalization [Other ref. 6] to the 
         data (either all of them at once, or on each condition independently). No specific parameters required."
    )
    
    tags$p(txt)
  })
  
  
  
  
  output$choose_normalizationQuantile <- renderUI({
    req(rv.norm$widgets$method)
    if (rv.norm$widgets$method != "QuantileCentering") { return (NULL)}
    
    tagList(
      mod_popover_for_help_ui(ns("modulePopover_normQuanti")),
      textInput(ns("normalization.quantile"), NULL,
                value = rv.norm$widgets$quantile,width='150px'),
      uiOutput(ns("test_normQuant"))
    )
    
  })
  
  
  
  
  
  output$choose_normalizationScaling <- renderUI({
    req(rv.norm$widgets$method)
    
    if (rv.norm$widgets$method == "MeanCentering"){
      # check if the normalisation has already been performed
      
      checkboxInput(ns("normalization.variance.reduction"), 
                    "Include variance reduction",  
                    value = rv.norm$widgets$varReduction)
    }
    
  })
  
  
  observeEvent(rv.norm$widgets$method,{
    req(obj())
    if (rv.norm$widgets$method == "None"){
      rv.norm$dataIn <- obj()
      rv.norm$resetTracking <- TRUE
    }
    
    shinyjs::toggle("perform.normalization", condition = rv.norm$widgets$method != "None")
    shinyjs::toggle("spanLOESS", condition = rv.norm$widgets$method == "LOESS")
    
    shinyjs::toggle("normalization.type", 
                    condition=( rv.norm$widgets$method %in% DAPAR2::normalizeMethods.dapar()))
    
    cond <- metadata(rv.norm$dataIn[[ind()]])$typeOfData == 'protein'
    shinyjs::toggle('DivProtSelection', condition= cond)
    shinyjs::toggle('SyncForNorm', condition= cond)
  })
  
  
  GetIndicesOfSelectedProteins <- reactive({
    rv.norm$trackFromBoxplot()
    if(is.null(rv.norm$trackFromBoxplot()$type)){return(NULL)}
    
    ind <- NULL
    ll <- rowData(rv.norm$dataIn[[ind()]])[,metadata(rv.norm$dataIn)$keyId]
    tt <- rv.norm$trackFromBoxplot()$type
    switch(tt,
           ProteinList = ind <- rv.norm$trackFromBoxplot()$list.indices,
           Random = ind <- rv.norm$trackFromBoxplot()$rand.indices,
           Column = ind <- rv.norm$trackFromBoxplot()$col.indices
    )
    if (length(ind)==0)
      ind <- NULL
    ind
  })
  
  
  ##' Reactive behavior : Normalization of data
  ##' @author Samuel Wieczorek
  observeEvent(input$perform.normalization,{
    rv.norm$widgets$method
    rv.norm$dataIn
    # isolate({
    conds <- colData(rv.norm$dataIn)$Condition
    
    switch(rv.norm$widgets$method, 
           G_noneStr = rv.norm$dataIn <- obj(),
           
           GlobalQuantileAlignment = {
             rv.norm$dataIn <- addAssay(rv.norm$dataIn, 
                                        normalizeD(rv.norm$dataIn[[ind()]], method='GlobalQuantileAlignment'), 
                                        "protein_norm")
           },
           
           QuantileCentering = {
             quant <-NA
             if (!is.null(rv.norm$widgets$quantile))
                quant <- as.numeric(rv.norm$widgets$quantile)
      
             rv.norm$dataIn <- addAssay(rv.norm$dataIn, 
                                        normalizeD(rv.norm$dataIn[[ind()]], 
                                                   method = 'QuantileCentering', 
                                                   conds = conds, 
                                                   type = rv.norm$widgets$type,
                                                   subset.norm = GetIndicesOfSelectedProteins(), 
                                                   quantile = quant),
                                        "proteins_norm")
             
           } ,
           
           MeanCentering = {
             rv.norm$dataIn <- addAssay(rv.norm$dataIn, 
                                        normalizeD(rv.norm$dataIn[[ind()]], 
                                                   method = 'MeanCentering', 
                                                   conds = conds, 
                                                   type = rv.norm$widgets$type,
                                                   subset.norm = GetIndicesOfSelectedProteins(), 
                                                   scaling = rv.norm$widgets$varReduction),
                                        "proteins_norm")
           }, 
           
           SumByColumns = {
              rv.norm$dataIn <- addAssay(rv.norm$dataIn, 
                                        normalizeD(rv.norm$dataIn[[ind()]], 
                                                   method = 'SumByColumns', 
                                                   conds = conds, 
                                                   type = rv.norm$widgets$type,
                                                   subset.norm = GetIndicesOfSelectedProteins()),
                                        "proteins_norm")
             
           },
           
           LOESS = { 
             rv.norm$dataIn <- addAssay(rv.norm$dataIn, 
                                        normalizeD(rv.norm$dataIn[[ind()]], 
                                                   method = 'LOESS', 
                                                   conds = conds, 
                                                   type = rv.norm$widgets$type,
                                                   span = as.numeric(rv.norm$widgets$spanLOESS)
                                                   ),
                                        "proteins_norm")
           },
           
           vsn = {
             rv.norm$dataIn <- addAssay(rv.norm$dataIn, 
                                        normalizeD(rv.norm$dataIn[[ind()]], 
                                                   method = 'vsn', 
                                                   conds = conds, 
                                                   type = rv.norm$widgets$type),
                                        "proteins_norm")
           }
    )
    # })
    rvModProcess$moduleNormalizationDone[1] <- TRUE
    shinyjs::toggle("valid.normalization", condition=input$perform.normalization >= 1)
  })
  
  
 
 
  
  ################
  # viewComparisonNorm <- reactive({
  #   rv.norm$settings()$examplePalette
  #   req(rv.norm$dataIn)
  #   GetIndicesOfSelectedProteins()
  #   leg <- NULL
  #   grp <- NULL
  #   
  #   labelsNorm <- NULL
  #   labelsToShowNorm <- NULL
  #   gToColorNorm <- NULL
  #   if (is.null(input$lab2Show)) { 
  #     labelsToShowNorm <- c(1:nrow(colData(rv.norm$dataIn)))
  #   } else { 
  #     labelsToShowNorm <- input$lab2Show
  #     }
  #   
  #   # if (is.null(rv$whichGroup2Color)){
  #   #   gToColorNorm <- "Condition"
  #   # } else {
  #   #   gToColorNorm <- rv$whichGroup2Color
  #   #   }
  #   # 
  #   
  #   #if (is.null(rv$whichGroup2Color)){
  #     labelsNorm <- colData(rv.norm$dataIn)[,"Condition"]
  #   # } else {
  #   #   labelsNorm <- apply(colData(rv.norm$dataIn), 1, function(x){paste0(x, collapse='_')})
  #   #   names(labelsNorm)<- NULL
  #   #   labelsNorm <- setNames(as.list(c(1:length(labs))),labs)
  #   # }
  #   
  #   
  #   dname <- paste0("Normalized.", metadata(rv.norm$dataIn[[ind()]])$typeOfData)
  #   #if (input$datasets == dname){
  #   #  obj1 <- rv$dataset[[(which(names(rv$dataset)==dname) - 1)]]
  #   #  obj2 <- obj()[[ind()]]
  #   #}
  #   #else {
  #   #  obj1 <- obj()[[ind()]]
  #   #  obj2 <- rv.norm$dataIn
  #   #  
  #   #}
  #   
  #   obj1 <- obj2 <- NULL
  #   
  #   if (length(GetIndicesOfSelectedProteins())==0) {
  #     wrapper.compareNormalizationD(obj1, 
  #                                   obj2,
  #                                   labelsNorm,
  #                                   as.numeric(labelsToShowNorm),
  #                                   palette = rv.norm$settings()$examplePalette) }
  #   else {
  #     ll <- rowData(rv.norm$dataIn[[ind()]])[,metadata(rv.norm$dataIn[[ind()]])$keyId]
  #     wrapper.compareNormalizationDSubset(obj1, 
  #                                         obj2,
  #                                         labelsNorm,
  #                                         as.numeric(labelsToShowNorm),
  #                                         idsForLegend = ll,
  #                                         palette = NULL,
  #                                         subset.view= GetIndicesOfSelectedProteins()
  #     )
  #     
  #   }
  #   
  # })
  
  
  #######################
  # output$viewComparisonNorm_DS <- renderImage({
  #   outfile <- tempfile(fileext='.png')
  #   # Generate a png
  #   png(outfile)
  #   viewComparisonNorm()
  #   dev.off()
  #   
  #   # Return a list
  #   list(src = outfile,
  #        alt = "This is alternate text")
  # }, deleteFile = FALSE)
  
  
  
  
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
    req(input$perform.normalization)
    
    isolate({
      if (rv.norm$widgets$method != G_noneStr) {
        typeOfDataset <- metadata(rv.norm$dataIn[[ind()]])$typeOfData
        name <- paste0("Normalized", ".", typeOfDataset)
        metadata(rv.norm$dataIn[[ind()]])$Params <- list(
                                method = rv.norm$widgets$method,
                                type = rv.norm$widgets$type,
                                varReduction = rv.norm$widgets$varReduction,
                                quantile = rv.norm$widgets$quantile,
                                spanLOESS =rv.norm$widgets$spanLOESS
                                 )
        
        rv.norm$dataOut <- rv.norm$dataIn
        r.nav$isDone[2] <- TRUE
       # UpdateDatasetWidget(rv$current.obj, name)
        
      }
      
    } )
    
    
    return({reactive(rv.norm$dataOut)})
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
 
}
    
## To be copied in the UI
# mod_pipe_prot_norm_ui("pipe_prot_norm_ui_1")
    
## To be copied in the server
# callModule(mod_pipe_prot_norm_server, "pipe_prot_norm_ui_1")
 
