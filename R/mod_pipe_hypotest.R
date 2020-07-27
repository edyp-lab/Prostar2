#' pipe_hypotest UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_pipe_hypotest_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_navigation_ui(ns('nav_pipe_hypotest'))
  )
}

#' pipe_hypotest Server Function
#'
#' @noRd
#' 
#' @param input,output,session
#' 
#' @param obj
#' 
#' @param ind
#' 
mod_pipe_hypotest_server <- function(input, output, session, obj, ind){
  ns <- session$ns
  
  
  ## Section navigation module
  # Variable to manage the different screens of the module
  r.nav <- reactiveValues(
    name = "Hypothesis Test",
    stepsNames = c("HypothesisTest", "Save"),
    ll.UI = list( screenStep1 = uiOutput(ns("Screen_Hypotest_1")),
                  screenStep2 = uiOutput(ns("Screen_Hypotest_2"))
    ),
    isDone =  rep(FALSE,2),
    mandatory =  rep(TRUE,2),
    reset = FALSE
  )
  
  ## reactive values for variables in the module
  rv.hypotest <- reactiveValues(
    name = "processHypotest",
    dataIn = NULL,
    i = NULL,
    #settings = NULL,
    # contient l'objet de sortie du module (ie. a QFeatures instance)
    dataOut = NULL,
    widgets = list(design = "None",
                   method = "None",
                   ttest_options = "Student",
                   th_logFC = 0,
                   listNamesComparison = NULL),
    res_AllPairwiseComparisons = NULL
  )
  
  
  observeEvent(req(r.nav$reset),{
    
    rv.hypotest$widgets <- list(design = "None",
                                method = "None",
                                ttest_options = "Student",
                                th_logFC = 0,
                                listNomsComparison = NULL)
    rv.hypotest$res_AllPairwiseComparisons = NULL
    
    rv.hypotest$dataIn <- obj()
    rv.hypotest$i <- ind()
    
    ## do not modify this part
    r.nav$isDone <- rep(FALSE, 2)
    r.nav$reset <- FALSE
    ## end of no modifiable part
    
    
    
  })
  
  
  callModule(mod_navigation_server, 'nav_pipe_hypotest', style=2, pages=r.nav)
  
  #### END of template part of the module
  
  
  
  ##
  ##  
  ## Calls to other modules
  ##
  ##
  
  # rv.process$settings <- callModule(mod_settings_server,
  #                                   "settings", 
  #                                   obj = reactive({obj()}))
  
  
  
  ##
  ## Definitions of the screens
  ##
  
  observe({
    req(obj())
    rv.hypotest$dataIn <- obj()
    rv.hypotest$i <- ind()
  })
  
  
  
  observeEvent(input$anaDiff_Design, {
    rv.hypotest$widgets$design <- as.numeric(input$anaDiff_Design)
  })
  observeEvent(input$diffAnaMethod,  {
    rv.hypotest$widgets$method <- as.numeric(input$diffAnaMethod)
  })
  observeEvent(input$ttest_options,  {
    rv.hypotest$widgets$ttest_options <- as.numeric(input$ttest_options)
  })
  
  observeEvent(input$diffAnaMethod,{
    toggle(id = "ttest_options",  condition = (input$diffAnaMethod == "ttests"))
  })
  
  observeEvent(input$seuilLogFC,  {
    rv.hypotest$widgets$th_logFC <- as.numeric(input$seuilLogFC)
  })
  
  
  
  ###---------------------------------------------------------------------------------###
  ###                                 Screen 1                                        ###
  ###---------------------------------------------------------------------------------###
  output$Screen_Hypotest_1 <- renderUI({
    
    
    isolate({
      NA.count<- length(which(is.na(assay(rv.hypotest$dataIn[[rv.hypotest$i]]))))
      if (NA.count > 0){
        tags$p("Your dataset contains missing values. Before using the differential analysis, you must filter/impute them")
      } else {
        tagList(
          
          tags$div(
            tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                      selectInput(ns("anaDiff_Design"), "Contrast", 
                                  choices=c("None"="None", "One vs One"='OnevsOne', "One vs All"="OnevsAll"),
                                  selected=rv.hypotest$widgets$design,
                                  width='150px')
            ),
            tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                      selectInput(ns("diffAnaMethod"),"Statistical test",
                                  choices = c("None"="None",
                                              "Limma"="Limma", 
                                              "t-tests"="ttests"),
                                  selected=rv.hypotest$widgets$method,
                                  width='150px')
            ),
            tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                      hidden( radioButtons(ns("ttest_options"), "t-tests options",choices=c("Student", "Welch"),
                                           selected=rv.hypotest$widgets$ttest_options,
                                           width='150px'))
            ),
            tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                      textInput(ns("seuilLogFC"), "log(FC) threshold",  
                                value=rv.hypotest$widgets$th_logFC,
                                width='150px'),
                      #module_Not_a_numericUI(ns("test_seuillogFC"))
            ),
            tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                      uiOutput(ns("correspondingRatio"))
                      
            ),
            tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                      actionButton(ns("PerformLogFCPlot"), "Perform log FC plot",class = actionBtnClass )
                      
            )
          ),
          tags$hr(),
          highchartOutput(ns("FoldChangePlot"), height="100%")
        )
        
      }
    })
  })
  
  
  
  output$FoldChangePlot <- renderHighchart({
    req(input$PerformLogFCPlot)
    
    data <- ComputeComparisons()
    
    if (length(ind)>0) { hc_logFC_DensityPlot(data,as.numeric(input$seuilLogFC)) }
    
    
  })
  
  ########################################################
  
  ### calcul des comparaisons                         ####
  ########################################################
  ComputeComparisons <- reactive({
    req(input$diffAnaMethod)
    req(input$anaDiff_Design)
    input$ttest_options
    
    if ((input$diffAnaMethod=="None")|| (input$anaDiff_Design=="None")) {return (NULL)}
    if (length(which(is.na(assay(rv.hypotest$dataIn[[rv.hypotest$i]])))) > 0) { return(NULL)}
    
    
    
    isolate({
      switch(input$diffAnaMethod,
             Limma={
               rv.hypotest$res_AllPairwiseComparisons <- t_test_sam(object=rv.hypotest$dataIn[[rv.hypotest$i]],
                                                                    sampleTab=data.frame(colData(rv.hypotest$dataIn)),
                                                                    FUN="limma.complete.test",
                                                                    comp.type=input$anaDiff_Design)
             },
             ttests={
               rv.hypotest$res_AllPairwiseComparisons <- t_test_sam(object=rv.hypotest$dataIn[[rv.hypotest$i]],
                                                                    sampleTab=data.frame(colData(rv.hypotest$dataIn)),
                                                                    FUN="compute.t.test",
                                                                    contrast = input$anaDiff_Design,
                                                                    type=input$ttest_options)
             })
      
      
      rv.hypotest$widgets$listNamesComparison <- names(metadata(rv.hypotest$res_AllPairwiseComparisons)[['t_test']])[1]
      
      
      ind <- grep('_logFC', colnames(metadata(rv.hypotest$res_AllPairwiseComparisons)$t_test))
      df <- setNames(as.data.frame(metadata(rv.hypotest$res_AllPairwiseComparisons)$t_test[,ind]),
                     colnames(metadata(rv.hypotest$res_AllPairwiseComparisons)$t_test)[ind])
      df
      
    })
    
  })
  
  
  output$correspondingRatio <- renderUI({
    
    ratio <- as.numeric(rv.hypotest$widgets$th_logFC)
    
    p("(FC = ", 2^(ratio), ")")
    
  })
  
  
  observeEvent(input$PerformLogFCPlot, {
    
    r.nav$isDone[1] <- TRUE
    
  })
  
  
  
  ###---------------------------------------------------------------------------------###
  ###                                 Screen 2                                        ###
  ###---------------------------------------------------------------------------------###
  
  output$Screen_HypoTest_2 <- renderUI({
    tagList(
      uiOutput(ns("btn_valid"))
    )
  })
  
  output$btn_valid <- renderUI({
    
    cond <- (input$diffAnaMethod != "None")&&(input$anaDiff_Design != "None")
    print(conds)
    if (!cond){return(NULL)}
    actionButton(ns("ValidTest"),"Save significance test", class = actionBtnClass)
    
  })
  
  observeEvent(input$ValidTest,{
    
    metadata(rv.hypotest$dataIn[[rv.hypotest$i]])$Params <- list(
      design = rv.hypotest$widgets$design,
      method = rv.hypotest$widgets$method,
      ttest_options = rv.hypotest$widgets$ttest_options,
      th_logFC = rv.hypotest$widgets$th_logFC,
      listNamesComparison =rv.hypotest$widgets$listNamesComparison
    )
    
    rv.hypotest$dataOut <- rv.hypotest$dataIn
    r.nav$isDone[2] <- TRUE
  })
  
  
  return({reactive(rv.hypotest$dataOut)})
  
}


## To be copied in the UI
# mod_pipe_hypotest_ui("pipe_hypotest_ui_1")

## To be copied in the server
# callModule(mod_pipe_hypotest_server, "pipe_hypotest_ui_1")

