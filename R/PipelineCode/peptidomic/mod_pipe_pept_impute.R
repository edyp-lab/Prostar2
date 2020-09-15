#' @title pipe_pept_impute UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' 
mod_pipe_pept_impute_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    mod_navigation_ui(ns('nav_pipe_process'))
  )
}
    
#' pipe_pept_impute Server Function
#'
#' @noRd 
#' 
mod_pipe_pept_impute_server <- function(input, output, session, obj){
  ns <- session$ns
  
  
  ## Section navigation module
  # Variable to manage the different screens of the module
  r.nav <- reactiveValues(
    name = "Imputation",
    stepsNames = c("Imputation", "Save"),
    ll.UI = list( screenStep1 = uiOutput(ns("Screen_Impute_1")),
                  screenStep2 = uiOutput(ns("Screen_Impute_2"))
    ),
    isDone =  rep(FALSE,2),
    mandatory =  rep(FALSE,2),
    reset = FALSE
  )
  
  ## reactive values for variables in the module
  rv.impute <- reactiveValues(
    name = "processPeptImpute",
    dataIn = NULL,
    dataOut = NULL,
    i = NULL,
    settings = NULL,
    tmp = NULL,
    
    widgets = list(algorithm = "None",
                   basicAlgorithm = "None",
                   detQuantile = 2.5,
                   detQuant_factor = 1,
                   imp4p_nbiter = 10,
                   imp4p_withLapala = FALSE,
                   imp4p_qmin = 2.5,
                   imp4pLAPALA_distrib = "beta",
                   KNN_n = 10
                  ),
    imputePlotsSteps = list(step0 = NULL,
                            step1 = NULL,
                            step2 = NULL
    )
  )
  
  
  
  observeEvent(req(r.nav$reset),{
    
    rv.impute$widgets <- list(algorithm = "None",
                              basicAlgorithm = "None",
                              detQuantile = 2.5,
                              detQuant_factor = 1,
                              imp4p_nbiter = 10,
                              imp4p_withLapala = FALSE,
                              imp4p_qmin = 2.5,
                              imp4pLAPALA_distrib = "beta",
                              KNN_n = 10
                               )

    
    ## do not modify this part
    rv.impute$dataIn <- obj()
    rv.impute$dataOut <- NULL
    rv.impute$i <- length(names(obj()))
    r.nav$isDone <- rep(FALSE, 2)
    r.nav$reset <- FALSE
    ## end of no modifiable part
  })
  
  callModule(mod_navigation_server, 'nav_pipe_process', style=2, pages=r.nav)
  
  #### END of template part of the module
  
  
  
  
  ##
  ##  
  ## Global variables for the module
  ##
  ##
  imputationAlgorithms <- c("None" = "None",
                            "imp4p" = "imp4p",
                            "Basic methods" = "BasicMethods")
  
  basicMethodsImputationAlgos <- c("None" = "None",
                                   "Det. quantile" = "detQuantile",
                                   "KNN" = "KNN",
                                   "MLE" = "MLE"
  )
  
  ##
  ##  
  ## Calls to other modules
  ##
  ##
  rv.impute$settings <- callModule(mod_settings_server, "settings", obj=reactive({rv.impute$dataIn}))
  
  callModule(mod_plots_mv_for_imputation_server,"mvImputationPlots_PeptideLevel", 
             obj = reactive({rv.impute$dataIn}),
             ind = reactive({length(names(rv.impute$dataIn))}),
             title = reactive("POV distribution"),
             palette =reactive({rv.impute$PlotParams$paletteConditions}))
  
  
  callModule(mod_det_quant_impute_Values_server, "DetQuantValues_DT",
             qData = reactive({req(rv.impute$dataIn)
               assay(rv.impute$dataIn,length(names(rv.impute$dataIn)))
             }),
             quant = reactive({rv.impute$widgets$detQuantile}), 
             factor = reactive({rv.impute$widgets$detQuant_factor})
             )

  
  
  callModule(mod_popover_for_help_server,"modulePopover_HelpImputationPeptide", 
             data = list(title = HTML("<strong>Algorithm</strong>"),
                                  content= HTML(paste0("<ul><li><strong>imp4p [Ref. 7]</strong> a proteomic-specific multiple 
                                                       imputation method that operates on peptide-level datasets and which 
                                                       proposes to impute each missing value according to its nature 
                                                       (left-censored  or random). To tune the number of iterations, 
                                                       let us keep in mind that, the more iterations, the more accurate the 
                                                       results, yet the more time-consuming the computation.</li> 
                                                       <li><strong>Dummy censored:</strong> each missing value is supposed 
                                                       to be a censored value and is replaced by the XXX quantile of the 
                                                       corresponding sample abundance distribution <ul><li><strong>KNN 
                                                       </strong>see [Other ref. 2].</li><li><strong>MLE </strong>Imputation 
                                                       with the maximum likelihood estimate of the expected intensity 
                                                       (see the norm R package).</li></ul></ul>")))
             )
  
  ##
  ##  
  ## Initialisation of the reactive values in the module
  ##
  ##
  observeEvent(obj(),{
    cat('Initialisation of rv.impute$dataIn')
    rv.impute$dataIn <- obj()
  })
  
  
  ##
  ## Definitions of the screens
  ##
  
  ###---------------------------------------------------------------------------------###
  ###                                 Screen 1                                        ###
  ###---------------------------------------------------------------------------------###
  
  output$Screen_Impute_1 <- renderUI({
    
  nbEmptyLines <- nEmptyLines(assay(rv.impute$dataIn,length(names(rv.impute$dataIn))))
  
  if (nbEmptyLines > 0) {
    tags$p("Your dataset contains empty lines (fully filled with missing values). In order to use
             the imputation tool, you must delete them by using the filter tool.")
    
  }
  else { 
    tabPanel("Miss. values imputation",
             id = "tabPanelImputation",
             value = "imputation",
             tags$div(
               tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                         mod_popover_for_help_ui(ns("modulePopover_HelpImputationPeptide")),
                         selectInput(ns("missing.value.algorithm"),
                                     NULL,
                                     choices = imputationAlgorithms, 
                                     selected = rv.impute$widgets$algorithm,
                                     width='150px')
               ),
               
               tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                         uiOutput(ns("basicAlgoUI"))),
               tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                         uiOutput(ns("detQuantOptsUI")),
                         uiOutput(ns("KNNOptsUI")),
                         uiOutput(ns("imp4pOptsUI"))),
               tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                         uiOutput(ns("imp4pOpts2UI"))),
               tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                         uiOutput(ns("detQuant_impValues")))
               
             ),
             tags$div(
               tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                         actionButton(ns("perform.imputation.button"), "Perform imputation", class = actionBtnClass))
             ),
             br(), br(), br(),
             uiOutput(ns("warningImputationMethod")),
             
             ## progress bar
             #br(),
             #br(),
             #uiOutput(outputId = "progressOne")
             tagList(
               tags$hr(),
               withProgress(message = '',detail = '', value = 0, {
                 incProgress(0.5, detail = 'Aggregation in progress')
                 mod_plots_mv_for_imputation_ui(ns("mvImputationPlots_PeptideLevel"))
               })
             )      
             
    )
    
  }
  
  })
  
  observeEvent(input$missing.value.algorithm,{
    rv.impute$widgets$algorithm <- input$missing.value.algorithm})
  
  observeEvent(input$missing.value.basic.algorithm,{
    rv.impute$widgets$basicAlgorithm <- input$missing.value.basic.algorithm})
  
  observeEvent(input$detQuant_quantile,{
    rv.impute$widgets$detQuantile <- input$detQuant_quantile})
  
  observeEvent(input$detQuant_factor,{
    rv.impute$widgets$detQuant_factor <- input$detQuant_factor})
  
  observeEvent(input$KNN_n,{
    rv.impute$widgets$KNN_n <- input$KNN_n})
  
  observeEvent(input$imp4p_nbiter,{
    rv.impute$widgets$imp4p_nbiter <- input$imp4p_nbiter})
  
  
  observeEvent(input$imp4p_withLapala,{
    rv.impute$widgets$imp4p_withLapala <- input$imp4p_withLapala})
  
  observeEvent(input$imp4p_qmin,{
    rv.impute$widgets$imp4p_qmin <- input$imp4p_qmin})
  
  observeEvent(input$imp4pLAPALA_distrib,{
    rv.impute$widgets$imp4pLAPALA_distrib <- input$imp4pLAPALA_distrib})
  
  
  
  
  
  
  
  
  output$basicAlgoUI <- renderUI({
    if (rv.impute$widgets$algorithm != "BasicMethods"){return(NULL)}
    
    selectInput(ns("missing.value.basic.algorithm"), 
                "Methods", 
                width='150px',
                choices = basicMethodsImputationAlgos,
                selected = rv.impute$widgets$basicAlgorithm)
    
  })
  
  
  output$detQuantOptsUI <- renderUI({
    req(rv.impute$widgets$basicAlgorithm)
    req(rv.impute$widgets$algorithm)
    if ((rv.impute$widgets$basicAlgorithm != "detQuantile") || 
        (rv.impute$widgets$algorithm != "BasicMethods")){return(NULL)}
    
    tagList(
      tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                numericInput(ns("detQuant_quantile"), "Quantile", 
                             value = rv.impute$widgets$detQuantile
                             , step=1, min=0, max=100,
                             width='100px')),
      tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                numericInput(ns("detQuant_factor"), "Factor", 
                             value = rv.impute$widgets$detQuant_factor,
                             step=1, min=0, max=10,
                             width='100px')
      )
    )
    
  })
  
  
  output$KNNOptsUI <- renderUI({
    req(rv.impute$widgets$basicAlgorithm)
    req(rv.impute$widgets$algorithm)
    if ((rv.impute$widgets$basicAlgorithm != "KNN") || 
        (rv.impute$widgets$algorithm != "BasicMethods")){return(NULL)}
    
    isolate({
      numericInput(ns("KNN_n"), "Neighbors", 
                   value = rv.impute$widgets$KNN_n, 
                   step=1, min=0, 
                   max = max(rv.impute$widgets$KNN_n, nrow(rv.impute$dataIn)),
                   width ='100px')
    })
  })
  
  
  output$imp4pOptsUI <- renderUI({
    if (rv.impute$widgets$algorithm != "imp4p"){return(NULL)}
    
    #updateSelectInput(session,"missing.value.basic.algorithm", selected="None")
    tagList(
      tags$div( style="display:inline-block; vertical-align: top; padding-right: 40px;",
                numericInput(ns("imp4p_nbiter"), "Iterations", 
                             value = rv.impute$widgets$imp4p_nbiter,
                             step=1, min=1, width='100px')),
      
      tags$div( style="display:inline-block; vertical-align: bottom; padding-right: 20px;",
                checkboxInput(ns("imp4p_withLapala"), "Impute MEC also", 
                              value = rv.impute$widgets$imp4p_withLapala ))
    )
  })
  
  
  output$imp4pOpts2UI <- renderUI({
    if (!isTRUE(rv.impute$widgets$imp4p_withLapala)){return(NULL)}
    
    
    tagList(
      tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                numericInput(ns("imp4p_qmin"), "Upper lapala bound", 
                             value = rv.impute$widgets$imp4p_qmin,
                             step=0.1, min=0, max=100,
                             width='100px')),
      tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                radioButtons(ns("imp4pLAPALA_distrib"), "Distribution type", 
                             choices = c("uniform" = "unif", "beta" = "beta"),
                             selected = rv.impute$widgets$imp4pLAPALA_distrib) 
      )
    )
  })
  
  
  
  output$detQuant_impValues <- renderUI({
    req(rv.impute$widgets$basicAlgorithm)
    req(rv.impute$widgets$algorithm)
    if ((rv.impute$widgets$basicAlgorithm != "detQuantile") || 
        (rv.impute$widgets$algorithm != "BasicMethods")){return(NULL)}
    
    
    mod_det_quant_impute_Values_ui("DetQuantValues_DT")
    
  })
  
  
  
  output$warningImputationMethod <- renderText({
    req(rv.impute$widgets$algorithm)
    req(rv.impute$widgets$imp4p_withLapala)
    
    
    if (rv.impute$widgets$imp4p_withLapala == FALSE){return(NULL)}
    
    var <- ((rv.impute$widgets$algorithm == "imp4p") && (isTRUE(rv.impute$widgets$imp4p_withLapala))) ||
      (rv.impute$widgets$basicAlgorithm ==  "BasicMethods")
    
    if (var){
      t <- "<br> 
    <font color=\"red\"><strong>Warning:</strong> Warning: Imputed MEC (Missing on the Entire Condition) 
    values must be very cautiously interpreted <br>[see the User manual, Section 6.3.1].</font color=\"red\">"
      HTML(t)}
    
  })
  
  
 
  
  # 
  #------------------------------------------
  ##' Missing values imputation - reactivity behavior
  ##' @author Samuel Wieczorek
  observeEvent(input$perform.imputation.button,{
    if (rv.impute$widgets$algorithm == 'None'){return(NULL)}
    
    nbMV_Before <- length(which(is.na(assay(rv.impute$dataIn, length(names(rv.impute$dataIn))))))
    
    
       withProgress(message = '',detail = '', value = 0, {
        incProgress(0.5, detail = 'Imputation in progress')
        
        switch (rv.impute$widgets$algorithm,
                imp4p = {
                    if (rv.impute$widgets$imp4p_withLapala) {
                      rv.impute$dataIn <- impute_dapar(object = rv.impute$dataIn,
                                                      i = length(names(rv.impute$dataIn)),
                                                      name = 'impute',
                                                      method = 'mi',
                                                      sampleTab = colData(rv.impute$dataIn),
                                                      nb.iter = rv.impute$widgets$imp4p_nbiter,
                                                      lapala = rv.impute$widgets$imp4p_withLapala,
                                                      q.min = rv.impute$widgets$imp4p_qmin / 100,
                                                      distribution = as.character(rv.impute$imp4pLAPALA_distrib))
            
            
                    } else {
                      rv.impute$dataIn <- impute_dapar(object = rv.impute$dataIn,
                                                       i = length(names(rv.impute$dataIn)),
                                                       name = 'impute',
                                                       method = 'mi',
                                                       sampleTab = colData(rv.impute$dataIn),
                                                       nb.iter = rv.impute$widgets$imp4p_nbiter,
                                                       lapala = rv.impute$widgets$imp4p_withLapala)
            
                       }
          
          },
          BasicMethods ={
            switch(rv.impute$widgets$basicAlgorithm,
                 KNN={  
                   rv.impute$dataIn <- impute_dapar(object = rv.impute$dataIn,
                                                    i = length(names(rv.impute$dataIn)),
                                                    name = 'impute',
                                                    method = 'knn_by_conds',
                                                    conds = colData(rv.impute$dataIn)$Condition,
                                                    k = rv.impute$widgets$KNN_n)
                 },
                 MLE={
                   rv.impute$dataIn <- impute_dapar(object = rv.impute$dataIn,
                                                    i = length(names(rv.impute$dataIn)),
                                                    name = 'impute',
                                                    method = 'mle_dapar',
                                                    sampleTab = colData(rv.impute$dataIn))
                 },
                 detQuantile=
                   {
                     rv.impute$dataIn <- impute_dapar(object = rv.impute$dataIn,
                                                      i = length(names(rv.impute$dataIn)),
                                                      name = 'impute',
                                                      method = 'det_quant',
                                                      qval = (rv.impute$widgets$detQuantile/100),
                                                      factor = rv.impute$widgets$detQuant_factor)
                   }
          )
          }
        )
        incProgress(1, detail = 'Finalize imputation')
        
      })

    
    
    nbMV_After <- length(which(is.na(assay(rv.impute$dataIn, length(names(rv.impute$dataIn))))))
    rv.impute$nb_MV_imputed <- nbMV_After - nbMV_Before
    r.nav$isDone[1] <- TRUE
    
  })
  
  
  
  
  # output$helpForImputation <- renderText({
  #   req(input$missing.value.algorithm)
  #   input$missing.value.basic.algorithm
  #   rv_impute$typeOfDataset
  #   
  #   if ((input$missing.value.algorithm == "None")) {return(NULL)}
  #   if ((input$missing.value.algorithm == "Basic methods") && is.null(input$missing.value.basic.algorithm == "None")) {return(NULL)}
  #   
  #   name <- NULL
  #   
  #   helpTextImputation <- list("imp4p" = "<strong>imp4p [5]</strong> is a proteomic-specific multiple imputation
  #                              method that operates on peptide-level datasets and which proposes <br>
  #                              to impute each missing value according to its nature (censored
  #                              or random). <br> The more iterations, the more accurate the results,
  #                              yet the more time-consuming.",
  #                              "dummy censored" = "Dummy censored: each missing value is supposed to be a censored value and
  #                              is replaced by the XXX quantile <br> of the corresponding sample
  #                              abundance distribution",
  #                              "KNN" = "<strong>K- nearest neighbors</strong>, see [7]",
  #                              "MLE" = "<strong>Maximum likelihood estimation</strong>, see [8]")
  #   
  #   
  #   if (input$missing.value.algorithm == "Basic methods") {
  #     name <- input$missing.value.basic.algorithm}
  #   else {name <- input$missing.value.algorithm}
  #   
  #   if (!is.null(name)) {
  #     HTML(helpTextImputation[[name]])
  #     
  #   }
  # })
  
  
  
  ###---------------------------------------------------------------------------------###
  ###                                 Screen 2                                        ###
  ###---------------------------------------------------------------------------------###
  
  output$Screen_Impute_2 <- renderUI({
    
    tagList(
      actionButton(ns("ValidImputation"), "Save imputation", class = actionBtnClass))
  })
}


##' -- Validate the imputation ---------------------------------------
##' @author Samuel Wieczorek
observeEvent(input$ValidImputation,{ 

    rv.impute$dataOut <- rv.impute$dataIn
    r.nav$isDone[2] <- TRUE
    
})


    
## To be copied in the UI
# mod_pipe_pept_impute_ui("pipe_pept_impute_ui_1")
    
## To be copied in the server
# callModule(mod_pipe_pept_impute_server, "pipe_pept_impute_ui_1")
 
