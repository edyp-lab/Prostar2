#' pipe_prot_impute UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_pipe_prot_impute_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    mod_navigation_ui(ns('nav_pipe_process'))
  )
}
    
#' pipe_prot_impute Server Function
#'
#' @noRd 
mod_pipe_prot_impute_server <- function(input, output, session, obj){
  ns <- session$ns
 
  
  ## Section navigation module
  # Variable to manage the different screens of the module
  r.nav <- reactiveValues(
    name = "Imputation",
    stepsNames = c("Partially Observed Values", "Missing on Entire Condition", "Save"),
    ll.UI = list( screenStep1 = uiOutput(ns("Screen_Impute_1")),
                  screenStep2 = uiOutput(ns("Screen_Impute_2")),
                  screenStep3 = uiOutput(ns("Screen_Impute_3"))
    ),
    isDone =  rep(FALSE,3),
    mandatory =  rep(FALSE,3),
    reset = FALSE
  )
  
  ## reactive values for variables in the module
  rv.impute <- reactiveValues(
    name = "processProtImpute",
    dataIn = NULL,
    dataOut = NULL,
    i = NULL,
    settings = NULL,
    tmp = NULL,
    
    widgets = list(POV_algorithm = "None",
                   POV_detQuant_quantile = 0,
                   POV_detQuant_factor = 1,
                   POV_KNN_n = 10,
                   MEC_algorithm = "None",
                   MEC_detQuant_quantile = 2.5,
                   MEC_detQuant_factor = 1,
                   MEC_fixedValue = 0
                  ),
    imputePlotsSteps = list(step0 = NULL,
                            step1 = NULL,
                            step2 = NULL
                            )
  )
  
  
  observeEvent(req(r.nav$reset),{
    
    rv.impute$widgets <- list(POV_algorithm = "None",
                              POV_detQuant_quantile = 0,
                              POV_detQuant_factor = 1,
                              POV_KNN_n = 10,
                              MEC_algorithm = "None",
                              MEC_detQuant_quantile = 2.5,
                              MEC_detQuant_factor = 1,
                              MEC_fixedValue = 0
                              )
    
    rv.impute$imputePlotsSteps <- list(step0 = NULL,
                                       step1 = NULL,
                                       step2 = NULL)
    
    ## do not modify this part
    rv.impute$dataIn <- obj()
    rv.impute$i <- length(names(obj()))
    r.nav$isDone <- rep(FALSE, 3)
    r.nav$reset <- FALSE
    ## end of no modifiable part
  })
  
  callModule(mod_navigation_server, 'nav_pipe_process', style=2, pages=r.nav)
  
  #### END of template part of the module
  
  
  #global variables for the module
  imputationAlgorithmsProteins_POV <- list("None" = "None",
                                           "slsa" = "slsa",
                                           "Det quantile" = "detQuantile",
                                           "KNN" = "KNN")

  imputationAlgorithmsProteins_MEC <- list("None" = "None",
                                           "Det quantile" = "detQuantile",
                                           "Fixed value" = "fixedValue")
  
  
  
  ##
  ##  
  ## Calls to other modules
  ##
  ##
  rv.impute$settings <- callModule(mod_settings_server, "settings", obj=reactive({rv.impute$dataIn}))
  
  callModule(mod_plots_mv_for_imputation_server,"mvImputationPlots_MV", 
             obj = reactive({obj()}),
             ind = reactive({length(names(obj()))}),
             title = reactive({"POV distribution"}),
             palette = reactive({rv.impute$settings()$basePalette}))
  
  callModule(mod_plots_mv_for_imputation_server,"mvImputationPlots_MEC", 
             obj = reactive({rv.impute$imputePlotsSteps$step1}),
             ind = reactive({length(names(rv.impute$dataIn))}),
             title = reactive({"Distribution after POV imputation"}),
             palette = reactive({rv.impute$settings()$basePalette}))
  
  callModule(mod_plots_mv_for_imputation_server,"mvImputationPlots_Valid", 
             obj = reactive({rv.impute$imputePlotsSteps$step2}),
             ind = reactive({length(names(rv.impute$dataIn))}),
             title = reactive({"Distribution after POV and MEC imputation"}),
             palette = reactive({rv.impute$settings()$basePalette}))
  
  callModule(mod_det_quant_impute_Values_server, "POV_DetQuantValues_DT", 
             qData = reactive({req(rv.impute$dataIn)
                                   assay(rv.impute$dataIn,length(names(rv.impute$dataIn)))
                                   }),
             quant = reactive({rv.impute$widgets$POV_detQuant_quantile}), 
             factor = reactive({rv.impute$widgets$POV_detQuant_factor}))
  
  callModule(mod_det_quant_impute_Values_server, "MEC_DetQuantValues_DT", 
             qData = reactive({req(rv.impute$dataIn)
                              assay(rv.impute$dataIn,length(names(rv.impute$dataIn)))
                              }),
             quant = reactive({rv.impute$widgets$MEC_detQuant_quantile}), 
             factor = reactive({rv.impute$widgets$MEC_detQuant_factor}))

  
  
  ########
  observeEvent(obj(),{
    cat('initialisation of rv.impute$dataIn')
    rv.impute$dataIn <- obj()
   # rv.impute$i <- length(names(obj()))
    rv.impute$imputePlotsSteps$step0 <- obj()
  })
  
  
  #########
  
  
  
  
  ##
  ## Definitions of the screens
  ##
  
  ###---------------------------------------------------------------------------------###
  ###                                 Screen 1                                        ###
  ###---------------------------------------------------------------------------------###
  
  output$Screen_Impute_1 <- renderUI({
    
    tagList(
      tags$div(
        tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                  uiOutput(ns("sidebar_imputation_step1"))) ,
        tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                  uiOutput(ns("POV_Params"))),
        tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                  uiOutput(ns("POV_showDetQuantValues")))
      ),
      
      tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                actionButton(ns("perform.imputationClassical.button"),
                             "Perform imputation", class = actionBtnClass)),
      tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",       
                uiOutput(ns("ImputationStep1Done"))),
      
      htmlOutput(ns("helpForImputation")),
      tags$hr(),
      mod_plots_mv_for_imputation_ui(ns("mvImputationPlots_MV"))
    )
  })
  
  
  observeEvent(input$POV_missing.value.algorithm, {
    rv.impute$widgets$POV_algorithm <- input$POV_missing.value.algorithm
  })
  
  observeEvent(input$POV_detQuant_quantile, {
    rv.impute$widgets$POV_detQuant_quantile <- input$POV_detQuant_quantile
  })
  
  observeEvent(input$POV_detQuant_factor, {
    rv.impute$widgets$POV_detQuant_factor <- input$POV_detQuant_factor
  })
  
  observeEvent(input$KNN_nbNeighbors, {
    rv.impute$widgets$POV_KNN_n <- input$KNN_nbNeighbors
  })
  
  
  observeEvent(input$perform.imputationClassical.button,{

      nbMV_Before <- length(which(is.na(assay(rv.impute$dataIn, length(names(rv.impute$dataIn))))))
      
      withProgress(message = '',detail = '', value = 0, {
        
        incProgress(0.5, detail = 'POV Imputation')
        
        #browser()
        switch(rv.impute$widgets$POV_algorithm,
               slsa = {
                 rv.impute$dataIn <- impute_dapar(object = rv.impute$dataIn,
                                                  i = length(names(rv.impute$dataIn)),
                                                  name = 'POV_impute',
                                                  method = 'POV_slsa',
                                                  sampleTab = colData(rv.impute$dataIn))
                 },
               detQuantile = {
                 rv.impute$dataIn <- impute_dapar(object = rv.impute$dataIn,
                                                  i = length(names(rv.impute$dataIn)),
                                                  name = 'POV_impute',
                                                  method = 'POV_det_quant',
                                                  conds = colData(rv.impute$dataIn)$Condition,
                                                  qval = rv.impute$widgets$POV_detQuant_quantile/100,
                                                  factor = rv.impute$widgets$POV_detQuant_factor)
                 },
               KNN = {
                 rv.impute$dataIn <- impute_dapar(object = rv.impute$dataIn,
                                                  i =  length(names(rv.impute$dataIn)),
                                                  name = 'POV_impute',
                                                  method = 'POV_knn_by_conds',
                                                  conds = colData(rv.impute$dataIn)$Condition,
                                                  k = rv.impute$widgets$POV_KNN_n)
               }
        )
        
        incProgress(1, detail = 'Finalize POV imputation')
        #rv.impute$i <- length(names(rv.impute$dataIn))
        nbMV_After <- length(which(is.na(assay(rv.impute$dataIn, length(names(rv.impute$dataIn))))))
        rv.impute$nb_POV_imputed <-  nbMV_Before - nbMV_After
        
        rv.impute$imputePlotsSteps$step1 <- rv.impute$dataIn
        r.nav$isDone[1] <- TRUE
      })

  })
  
  
  
  
  
  output$POV_Params <- renderUI({
    req(rv.impute$widgets$POV_algorithm)
    
 
      switch(rv.impute$widgets$POV_algorithm,
             detQuantile = {
               
               tagList(
                 tags$div( style="display:inline-block; vertical-align: top; padding-right: 40px;",
                           numericInput(ns("POV_detQuant_quantile"), "Quantile", 
                                        value = rv.impute$widgets$POV_detQuant_quantile, 
                                        step=0.5, min=0, max=100, width='100px')),
                 tags$div( style="display:inline-block; vertical-align: top; padding-right: 40px;",
                           numericInput(ns("POV_detQuant_factor"), "Factor", 
                                        value = rv.impute$widgets$POV_detQuant_factor,
                                        step=0.1, min=0, max=10, width='100px'))
               )
             },
             KNN = {
               numericInput(ns("KNN_nbNeighbors"), "Neighbors", 
                            value = rv.impute$widgets$POV_KNN_n, step=1, min=0, 
                            max=max(nrow(rv.impute$dataIn), rv.impute$widgets$POV_KNN_n), 
                            width='100px')
             }
      )
      

  })
  
  
  
  output$sidebar_imputation_step1 <- renderUI({
    # req(rv.impute$dataIn)
    

      if (length(grep("imputed", names(rv.impute$dataIn)))==0){
        shinyjs::enable("perform.imputationClassical.button")
        
      } else {
        shinyjs::disable("perform.imputationClassical.button")
      }
      
      algo <- imputationAlgorithmsProteins_POV
      
      tags$div( style="display:inline-block; vertical-align: top; padding-right: 40px;",
                selectInput(ns("POV_missing.value.algorithm"),"Algorithm for POV",
                            choices = algo, 
                            selected=rv.impute$widgets$POV_algorithm, 
                            width='150px')
      )
  })

  
  
  output$POV_showDetQuantValues <- renderUI({
    
    req(rv.impute$widgets$POV_algorithm)
    
    if (rv.impute$widgets$POV_algorithm == 'detQuantile')
    {
      tagList(
        h5("The POV will be imputed by the following values :"),
        mod_det_quant_impute_Values_ui(ns("POV_DetQuantValues_DT"))
      )
    }
  })
  
  
  
  output$ImputationStep1Done <- renderUI({

    if (isTRUE(r.nav$isDone[1])) {
      tagList(
        h5(paste0("POV imputation done.", rv.impute$nb_POV_imputed, " were imputed")),
        # br(),
        h5("Updated graphs can be seen on step \"2 - Missing on the Entire Condition\".")
      )
    }
  })
  
  
  ###---------------------------------------------------------------------------------###
  ###                                 Screen 2                                        ###
  ###---------------------------------------------------------------------------------###
  
  output$Screen_Impute_2 <- renderUI({
    
    tagList(
      uiOutput("warningMECImputation"),
      tags$div(
        tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                  uiOutput(ns("MEC_chooseImputationMethod"))),
        tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                  uiOutput(ns("MEC_Params"))),
        tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                  uiOutput(ns("MEC_showDetQuantValues")))),
      
      tagList(
        tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                  actionButton(ns("btn.perform.imputationMEC"),"Perform imputation", class = actionBtnClass)),
        tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                  uiOutput(ns("ImputationStep2Done")))),
      tags$hr(),
      withProgress(message = '',detail = '', value = 0, {
        incProgress(0.5, detail = 'Building plots...')
        
        mod_plots_mv_for_imputation_ui(ns("mvImputationPlots_MEC"))
      })
    )
    
  })
  
  observeEvent(input$MEC_detQuant_quantile, {
    rv.impute$widgets$MEC_detQuant_quantile <- input$MEC_detQuant_quantile
  })
  
  observeEvent(input$MEC_fixedValue, {
    rv.impute$widgets$MEC_fixedValue <- input$MEC_fixedValue
  })
  
  observeEvent(input$MEC_detQuant_factor, {
    rv.impute$widgets$MEC_detQuant_factor <- input$MEC_detQuant_factor
  })
  
  observeEvent(input$MEC_missing.value.algorithm, {
    rv.impute$widgets$MEC_algorithm <- input$MEC_missing.value.algorithm
  })
  
  
  
  output$warningMECImputation<- renderUI({
    
    tags$p(tags$b("Warning:"),"Imputing MEC in a conservative way
  is a real issue as, in the given condition, there is no observed value to rely on.
   Thus, if imputation is not avoidable, imputed MEC must be very cautiously interpreted.")
  })
  
  
  
  observeEvent(input$btn.perform.imputationMEC,{
    

      withProgress(message = '',detail = '', value = 0, {
        #rv.impute$dataIn <- reIntroduceMEC(rv.impute$dataIn, rv.impute$MECIndex)
        
        nbMV_Before <- length(which(is.na(assay(rv.impute$dataIn, length(names(rv.impute$dataIn))))))
        incProgress(0.75, detail = 'MEC Imputation')
        switch(rv.impute$widgets$MEC_algorithm,
               detQuantile = {
                 rv.impute$dataIn <- impute_dapar(object = rv.impute$dataIn , 
                                                  i = length(names(rv.impute$dataIn)),
                                                  name = 'MEC_impute',
                                                  method = 'det_quant',
                                                  qval = rv.impute$widgets$MEC_detQuant_quantile/100,
                                                  factor = rv.impute$widgets$MEC_detQuant_factor
                                                  )
               },
               fixedValue = {
                 rv.impute$dataIn <- impute_dapar(object = rv.impute$dataIn,
                                                  i = length(names(rv.impute$dataIn)),
                                                  name = 'MEC_impute',
                                                  method = 'fixed_val',
                                                  value = rv.impute$widgets$MEC_fixedValue
                                                  )
               }
        )
        
        #rv.impute$i <- length(names(rv.impute$dataIn))
        nbMV_After <- length(which(is.na(assay(rv.impute$dataIn , length(names(rv.impute$dataIn))))))
        rv.impute$nb_MEC_imputed <-  nbMV_Before - nbMV_After
        
        incProgress(1, detail = 'Finalize MEC imputation')
        #rv.impute$impute_Step <- 2
        rv.impute$imputePlotsSteps$step2 <- rv.impute$dataIn
        r.nav$isDone[2] <- TRUE
      })
  })
  
  
  
  output$MEC_Params <- renderUI({
    req(rv.impute$widgets$MEC_algorithm)

      switch (rv.impute$widgets$MEC_algorithm,
              detQuantile = {
                tagList(
                  tags$div( style="display:inline-block; vertical-align: top; padding-right: 40px;",
                            numericInput(ns("MEC_detQuant_quantile"), "Quantile", 
                                         value = rv.impute$widgets$MEC_detQuant_quantile,
                                         step=0.5, min=0, max=100,
                                         width='100px')),
                  tags$div( style="display:inline-block; vertical-align: top; padding-right: 40px;",
                            numericInput(ns("MEC_detQuant_factor"), "Factor", 
                                         value = rv.impute$widgets$MEC_detQuant_factor, 
                                         step=0.1, min=0, max=10,
                                         width='100px'))
                )
              },
              fixedValue = {
                
                numericInput(ns("MEC_fixedValue"), "Fixed value", 
                             value = rv.impute$widgets$MEC_fixedValue, 
                             step=0.1, min=0, max=100,
                             width='100px')
              })

  })
  
  
  
  output$MEC_chooseImputationMethod <- renderUI({
    algo <- imputationAlgorithmsProteins_MEC
    
    tags$div( style="display:inline-block; vertical-align: top; padding-right: 40px;",
              selectInput(ns("MEC_missing.value.algorithm"), "Algorithm for MEC", choices = algo,
                          selected=rv.impute$widgets$MEC_algorithm, width='150px')
    )
  })
  
  
  
  output$MEC_showDetQuantValues <- renderUI({
    
    req(rv.impute$widgets$MEC_algorithm)
    
    if (rv.impute$widgets$MEC_algorithm == 'detQuantile')
    {
      tagList(
        h5("The MEC will be imputed by the following values :"),
        mod_det_quant_impute_Values_ui(ns("MEC_DetQuantValues_DT"))
      )
    }
  })
  
  
  output$ImputationStep2Done <- renderUI({

    if (r.nav$isDone[2]) {
      tagList(
        h5("MEC imputation done.", rv.impute$nb_MEC_imputed, " were imputed"),
        h5("Updated graphs cans be seen on step \"3 - Save\"."))
    }

  })
  
  
  
  ###---------------------------------------------------------------------------------###
  ###                                 Screen 3                                        ###
  ###---------------------------------------------------------------------------------###
  output$Screen_Impute_3 <- renderUI({
    
    tagList(
      tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                actionButton(ns("ValidImputation"),"Save imputation", class = actionBtnClass)),
      tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                uiOutput(ns("ImputationSaved"))),
      tags$hr(),
      mod_plots_mv_for_imputation_ui(ns("mvImputationPlots_Valid"))
    )
  })
  
  
  observeEvent(input$ValidImputation,{
    rv.impute$dataIn
    
      ind <- grep('_impute', names(rv.impute$dataIn))
      
      if (length(ind) > 1){
        txt <- lapply(ind, function(x){metadata(rv.impute$dataIn[[x]])$Params})
        ind <- ind[-length(ind)]
        rv.impute$dataIn <- rv.impute$dataIn[ , ,-ind]
        metadata(rv.impute$dataIn[[length(names(rv.impute$dataIn))]])$Params <- txt
      } else if (length(ind)==1){
        names(rv.impute$dataIn)[length(names(rv.impute$dataIn))] <- 'impute'
      }
      
      rv.impute$dataOut <- rv.impute$dataIn
      r.nav$isDone[3] <- TRUE
  })

  return({reactive(rv.impute$dataOut)})
  
  
}
    
## To be copied in the UI
# mod_pipe_prot_impute_ui("pipe_prot_impute_ui_1")
    
## To be copied in the server
# callModule(mod_pipe_prot_impute_server, "pipe_prot_impute_ui_1")
 
