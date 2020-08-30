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
  
  callModule(mod_plots_mv_server,"mvImputationPlots_MV", 
             qData=reactive({assay(rv.impute$dataIn, rv.impute$i)}),
             conds = reactive({colData(rv.impute$dataIn)}),
             title = reactive("POV distribution"),
             palette =reactive(unique(rv.impute$settings()$basePalette)))
  
  callModule(mod_plots_mv_server,"mvImputationPlots_MEC", 
             qData=reactive({assay(rv.impute$dataIn, rv.impute$i)}),
             conds = reactive({colData(rv.impute$dataIn)}),
             title = reactive("Distribution after POV imputation"),
             palette =reactive(unique(rv.impute$settings()$basePalette)))
  
  callModule(mod_plots_mv_server,"mvImputationPlots_Valid", 
             qData=reactive({assay(rv.impute$dataIn, rv.impute$i)}),
             conds = reactive({colData(rv.impute$dataIn)}),
             title = reactive("Distribution after POV and MEC imputation"),
             palette =reactive(unique(rv.impute$settings()$basePalette)))
  
  callModule(mod_det_quant_impute_Values_server, "POV_DetQuantValues_DT", 
             qData = reactive({req(rv.impute$dataIn)
                                   assay(rv.impute$dataIn,rv.impute$i)
                                   }),
             quant = reactive({rv.impute$widgets$POV_detQuant_quantile}), 
             factor = reactive({rv.impute$widgets$POV_detQuant_factor}))
  
  callModule(mod_det_quant_impute_Values_server, "MEC_DetQuantValues_DT", 
             qData = reactive({req(rv.impute$dataIn)
                              assay(rv.impute$dataIn,rv.impute$i)
                              }),
             quant = reactive({rv.impute$widgets$MEC_detQuant_quantile}), 
             factor = reactive({rv.impute$widgets$MEC_detQuant_factor}))

  
  
  ########
  observe({
    req(obj())
    rv.impute$dataIn <- obj()
    rv.impute$i <- length(names(obj()))
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
      mod_plots_mv_ui(ns("mvImputationPlots_MV"))
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
    
    isolate({
      
      rv.impute$MECIndex <-NULL
      rv.impute$dataIn <- rv.impute$imputePlotsSteps[["step0"]]
      nbMV_Before <- length(which(is.na(assay(rv.impute$dataIn, rv.impute$i))))
      
      withProgress(message = '',detail = '', value = 0, {
        incProgress(0.25, detail = 'Find MEC blocks')
        
        incProgress(0.5, detail = 'POV Imputation')
        switch(rv.impute$widgets$POV_algorithm,
               slsa = {
                 rv.impute$dataIn <- impute_dapar(obj = rv.impute$dataIn,
                                                  i = rv.impute$i,
                                                  'POV_slsa',
                                                  sampleTab = colData(rv.impute$dataIn))
                 },
               detQuantile = {
                 rv.impute$dataIn <- impute_dapar(obj = rv.impute$dataIn,
                                                  i = rv.impute$i,
                                                  'POV_det_quant',
                                                  qval = rv.impute$widgets$POV_detQuant_quantile/100,
                                                  factor = rv.impute$widgets$POV_detQuant_factor)
                 },
               KNN = {
                 rv.impute$dataIn <- impute_dapar(obj = rv.impute$dataIn,
                                                  i = rv.impute$i,
                                                  'knn_by_conds',
                                                  conds = colData(rv.impute$dataIn)$Condition,
                                                  k = rv.impute$widgets$POV_KNN_n)
               }
        )
        incProgress(0.75, detail = 'Reintroduce MEC blocks')
        incProgress(1, detail = 'Finalize POV imputation')
        
        nbMV_After <- length(which(is.na(assay(rv.impute$dataIn, rv.impute$i))))
        rv.impute$nbPOVimputed <-  nbMV_Before - nbMV_After
        
        rv.impute$impute_Step <- 1
        rv.impute$imputePlotsSteps[["step1"]] <- rv.impute$dataIn
        rv.nav$isDone[1] <- TRUE
        # 
        # shinyjs::enable("btn.perform.imputationMEC)
        # shinyjs::enable("ValidImputation")
        
      })
    })
  })
  
  
  
  
  
  output$POV_Params <- renderUI({
    req(rv.impute$widgets$POV_algorithm)
    
    isolate({
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
  })
  
  
  
  output$sidebar_imputation_step1 <- renderUI({
    # req(rv.impute$dataIn)
    
    isolate({
      if (length(grep("Imputed", names(rv.impute$dataIn)))==0){
        rv.impute$imputePlotsSteps[["step0"]] <- obj()
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
    #isolate({
    if (isTRUE(r.nav$isDone[1])) {
      tagList(
        h5(paste0("POV imputation done.", rv.impute$nbPOVimputed, " were imputed")),
        # br(),
        h5("Updated graphs can be seen on step \"2 - Missing on the Entire Condition\".")
      )
    }
    # })
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
        
        mod_plots_mv(ns("mvImputationPlots_MEC"))
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
    
    isolate({
      withProgress(message = '',detail = '', value = 0, {
        incProgress(0.25, detail = 'Reintroduce MEC')
        
        #rv.impute$dataIn <- reIntroduceMEC(rv.impute$dataIn, rv.impute$MECIndex)
        
        nbMV_Before <- length(which(is.na(assay(rv.impute$dataIn, rv.impute$i))))
        incProgress(0.75, detail = 'MEC Imputation')
        switch(rv.impute$widgets$MEC_algorithm,
               detQuantile = {
                 rv.impute$dataIn <- impute_dapar(rv.impute$dataIn , 
                                                  rv.impute$i,
                                                  'det_quant',
                                                  qval = rv.impute$widgets$MEC_detQuant_quantile/100,
                                                  factor = rv.impute$widgets$MEC_detQuant_factor)
               },
               fixedValue = {
                 rv.impute$dataIn <- impute_dapar(obj = rv.impute$dataIn,
                                                  i = rv.impute$i,
                                                   'impute_fixed_value',
                                                   value = rv.impute$widgets$MEC_fixedValue)
               }
        )
        
        nbMV_After <- length(which(is.na(assay(rv.impute$dataIn , rv.impute$i))))
        rv.impute$nbMECimputed <-  nbMV_Before - nbMV_After
        
        incProgress(1, detail = 'Finalize MEC imputation')
        rv.impute$impute_Step <- 2
        rv.impute$imputePlotsSteps[["step2"]] <- rv.impute$dataIn
        r.nav$isDone[2] <- TRUE
      })
    })
  })
  
  
  
  output$MEC_Params <- renderUI({
    req(rv.impute$widgets$MEC_algorithm)
    isolate({
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
    #isolate({
    if (rv.nav$isDone[2]) {
      tagList(
        h5("MEC imputation done.", rv.impute$nbMECimputed, " were imputed"),
        h5("Updated graphs cans be seen on step \"3 - Save\"."))
    }
    #})
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
      mod_plots_mv(ns("mvImputationPlots_Valid"))
    )
  })
  
  
  observeEvent(input$ValidImputation,{
    
    isolate({
      
      name <- paste0("Imputed", ".", rv.impute$typeOfDataset)
      rv.impute$dataIn <- saveParameters(rv.impute$dataIn, name,"proteinImputation",build_ParamsList_ProteinImputation())
      UpdateDatasetWidget(rv.impute$dataIn, name)
      
      
      rv.impute$ValidImputationClicked <- TRUE
      r.nav$isDone[3] <- TRUE
    })
  })
  
  
  
  
  
  
  output$ImputationSaved <- renderUI({
    req(input$datasets)
    if ((length(grep("Imputed",input$datasets)) !=1) ) {return(NULL)  }
    else if (grep("Imputed",input$datasets) == 1 ) {
      h4("The imputed dataset has been saved.")
    }
  })
  
  
  
  
}
    
## To be copied in the UI
# mod_pipe_prot_impute_ui("pipe_prot_impute_ui_1")
    
## To be copied in the server
# callModule(mod_pipe_prot_impute_server, "pipe_prot_impute_ui_1")
 
