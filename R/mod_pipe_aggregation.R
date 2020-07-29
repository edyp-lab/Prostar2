#' pipe_aggregation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_pipe_aggregation_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_navigation_ui(ns('nav_pipe_aggregation'))
  )
}


#' pipe_aggregation Server Function
#'
#' @noRd
#' 
#' @param input,output,session
#' 
#' @param obj
#' 
#' @param ind
#' 
mod_pipe_aggregation_server <- function(input, output, session, obj, ind){
  ns <- session$ns
  
  
  
  ## Section navigation module
  # Variable to manage the different screens of the module
  r.nav <- reactiveValues(
    name = "Aggregation",
    stepsNames = c("Aggregation", "Add metadata", "Save"),
    ll.UI = list( screenStep1 = uiOutput(ns("Screen_Aggregation_1")),
                  screenStep2 = uiOutput(ns("Screen_Aggregation_2")),
                  screenStep3 = uiOutput(ns("Screen_Aggregation_3"))
    ),
    isDone =  rep(FALSE,2),
    mandatory =  rep(TRUE,2),
    reset = FALSE
  )
  
  
  ## reactive values for variables in the module
  rv.aggregation <- reactiveValues(
    name = "processAggregation",
    dataIn = NULL,
    i = NULL,
    settings = NULL,
    # contient l'objet de sortie du module (ie. a QFeatures instance)
    dataOut = NULL,
    widgets = list(includeSharedPeptides = "Yes2",
                   operator = "Mean",
                   considerPeptides = 'allPeptides',
                   proteinId = "None",
                   topN = 3),
    temp.aggregate = NULL
  )
  
  
  
  observeEvent(req(r.nav$reset),{
    ## update widgets whose names are in r.widgets with the value in this list
    ## This part must be before the reinitialization of r.nav$isDone
    updateRadioButtons(session, "radioBtn_includeShared", selected="Yes2")
    updateRadioButtons(session, "AggregationConsider", selected='allPeptides')
    updateNumericInput(session, "nTopn", value=3)
    updateSelectInput(session,'proteinId', selected="None")
    updateRadioButtons(session, "AggregationOperator", selected='Mean')
    updateNumericInput(session, "nbPeptides", value=0)
    # updateCheckboxInput(session,"replaceAllZeros", value=NULL)
    
    rv.aggregation$widgets$includeSharedPeptides <- "Yes2"
    rv.aggregation$widgets$operator <- "Mean"
    rv.aggregation$widgets$considerPeptides <- 'allPeptides'
    rv.aggregation$widgets$proteinId <- "None"
    rv.aggregation$widgets$topN <- 3
    rv.aggregation$temp.aggregate <- NULL
    
    
    rv.aggregation$dataIn <- obj()
    rv.aggregation$i <- ind()
    
    ## do not modify this part
    r.nav$isDone <- rep(FALSE, 2)
    r.nav$reset <- FALSE
    ## end of no modifiable part
  })
  
  
  callModule(mod_navigation_server, 'nav_pipe_aggregation', style=2, pages=r.nav)
  
  
  
  #### END of template part of the module
  
  
  
  ##
  ##  
  ## Calls to other modules
  ##
  ##
  
  rv.aggregation$settings <- callModule(mod_settings_server,
                                        "settings", 
                                        obj = reactive({obj()}))
  
  #callModule(moduleStaticDataTable,"overview_Aggregation", table2show=reactive({GetDatasetOverview2()}))
  
  
  callModule(mod_popover_for_help_server,"modulePopover_includeShared", 
             data = list(title=HTML(paste0("<strong>Include shared peptides</strong>")),
                         content= HTML(paste0("<ul>
                                  <li><strong>No:</strong>only protein-specific peptides</li>
                                  <li><strong>Yes 1:</strong>shared peptides processed as protein specific</li>
                                  <li><strong>Yes 2</strong>: proportional redistribution of shared peptides</li></ul>")) ))
  
  callModule(mod_popover_for_help_server,"modulePopover_colsForAggreg", 
             data = list(title=HTML(paste0("<strong><font size=\"4\">Columns of the meta-data</font></strong>")),
                         content= "Select the columns of the meta-data (related to proteins) that have to be recorded 
                                  in the new protein dataset (e.g. the columns which contains the protein ID if you wish to 
                                  perform a GO analysis.)"))
  
  
  
  observe({
    req(obj())
    rv.aggregation$dataIn <- obj()
    rv.aggregation$i <- ind()
  })
  
  
  ##
  ## Definitions of the screens
  ##
  
  ###---------------------------------------------------------------------------------###
  ###                                 Screen 1                                        ###
  ###---------------------------------------------------------------------------------###
  
  output$Screen_Aggregation_1 <- renderUI({
    
    tagList(
      uiOutput(ns("warningAggregationMethod")),
      div(
        div( style="display:inline-block; vertical-align: top;",
             uiOutput(ns("chooseProteinId"))),
        div( style="display:inline-block; vertical-align: top;",       
             mod_popover_for_help_ui(ns("modulePopover_includeShared")),
             radioButtons(ns("radioBtn_includeShared"), NULL,
                          choices=c("No" = "No",
                                    "as protein specific"= "Yes1" ,
                                    "redistribution" = "Yes2" ),
                          selected=rv.aggregation$widgets$includeSharedPeptides)),
        div( style="display:inline-block; vertical-align: top; padding-right: 10px;",
             radioButtons(ns("AggregationConsider"), "Consider", 
                          choices=c('all peptides'="allPeptides", 
                                    "N most abundant"="onlyN"), 
                          selected=rv.aggregation$widgets$considerPeptides)),
        div( style="display:inline-block; vertical-align: top; padding-right: 10px;",
             numericInput(ns("nTopn"), "N",value = rv.aggregation$widgets$topN, min = 0, step=1, width='100px')),
        
        div( style="display:inline-block; vertical-align: top;",
             uiOutput(ns("operatorChoice"))
        )
      ),
      actionButton(ns("perform.aggregation"),"Perform aggregation", class = actionBtnClass),
      uiOutput(ns("ObserverAggregationDone")),
      
      hr(),
      div(
        div( style="display:inline-block; vertical-align: top;",
             uiOutput(ns("specificPeptideBarplot"))),
        div( style="display:inline-block; vertical-align: top; padding-right: 20px;",       
             uiOutput(ns("allPeptideBarplot"))),
        div( style="display:inline-block; vertical-align: top;",
             DT::dataTableOutput(ns("aggregationStats")))
      )
    )
  })
  
  
  output$warningAggregationMethod <- renderUI({
    req(rv.aggregation$dataIn[[rv.aggregation$i]])
    
    if (length(which(is.na(assay(rv.aggregation$dataIn[[rv.aggregation$i]])))) > 0)
    {
      tags$p(tags$b('Warning:')," Your dataset contains missing values.
             For better results, you should impute them first")
    }
    
  })
  
  
  output$chooseProteinId <- renderUI({
    if (!is.null(metadata(rv.aggregation$dataIn)[["parentProtId"]])) {return(NULL)}
    
    selectInput(ns("proteinId"), 
                "Choose the protein ID",
                choices = c("None",colnames(rowData(rv.aggregation$dataIn[[rv.aggregation$i]]))),
                selected = rv.aggregation$widgets$proteinId)
  })
  
  
  observeEvent(input$radioBtn_includeShared, {
    if (input$radioBtn_includeShared=='Yes2'){
      ch <- c("Mean"="Mean")  
    } else {
      ch <- c("Sum"='Sum', "Mean"="Mean")
    }
    #updateRadioButtons(session,"AggregationOperator", choices=ch, selected=input$AggregationOperator)
  })
  
  
  observeEvent(input$AggregationConsider,{
    shinyjs::toggle('nTopn', condition=input$AggregationConsider=='onlyN')
  })
  
  
  
  
  output$operatorChoice <- renderUI({
    input$radioBtn_includeShared
    
    choice <- NULL
    if (input$radioBtn_includeShared %in% c("No", "Yes1")){
      choice <- c("Mean"="Mean","Sum"="Sum")
    } else {choice <- c("Mean"="Mean")}
    choice
    
    radioButtons(ns("AggregationOperator"), "Operator", 
                 choices=choice, 
                 selected=rv.aggregation$widgets$operator)
  })
  
  
  
  observeEvent(input$perform.aggregation,{
    input$radioBtn_includeShared
    input$AggregationOperator
    input$AggregationConsider
    input$nTopn
    
    print(paste0("agg",input$AggregationOperator))
    
    rv.aggregation$dataIn <- obj()
    rv.aggregation$i <- ind()
    
    isolate({
      withProgress(message = '',detail = '', value = 0, {
        incProgress(0.2, detail = 'loading foreach package')
        
        
        require(foreach)
        incProgress(0.5, detail = 'Aggregation in progress')
        
        if(input$radioBtn_includeShared %in% c("Yes2", "Yes1")){
          #X <- rv$matAdj$matWithSharedPeptides
          X <- "onlyShared"
          if (input$radioBtn_includeShared == 'Yes1'){
            if (input$AggregationConsider == 'allPeptides') {
              rv.aggregation$dataIn <- aggregateFeatures_sam(object = rv.aggregation$dataIn,
                                                             i = rv.aggregation$i,
                                                             aggType = X,
                                                             name = "peptides_aggregation",
                                                             fun = paste0("agg",input$AggregationOperator)
              )
              #do.call(paste0('aggregate',input$AggregationOperator),list( obj.pep=rv$current.obj,X=X))
            } else {
              rv.aggregation$dataIn <- aggregateFeatures_sam(object = rv.aggregation$dataIn,
                                                             i = rv.aggregation$i,
                                                             aggType = X,
                                                             name = "peptides_aggregation",
                                                             fun = 'aggTopn',
                                                             method = input$AggregationOperator,
                                                             n = as.numeric(input$nTopn)
              )
              #aggregateTopn(rv$current.obj, X,input$AggregationOperator, n=as.numeric(input$nTopn))
            }
          } else {
            if (input$AggregationConsider == 'allPeptides') {
              rv.aggregation$dataIn <- aggregateFeatures_sam(object = rv.aggregation$dataIn,
                                                             i = rv.aggregation$i,
                                                             aggType = X,
                                                             name = "peptides_aggregation",
                                                             fun = "aggIterParallel",
                                                             init.method = 'Sum',
                                                             method = 'Mean',
                                                             conditions = colData(rv.aggregation$dataIn)[['Condition']]
              )
              #aggregateIterParallel(rv$current.obj, X,init.method='Sum', method='Mean')
            } else {
              rv.aggregation$dataIn <- aggregateFeatures_sam(object = rv.aggregation$dataIn,
                                                             i = rv.aggregation$i,
                                                             aggType = X,
                                                             name = "peptides_aggregation",
                                                             fun = "aggIterParallel",
                                                             init.method = 'Sum',
                                                             method = 'onlyN',
                                                             n=input$nTopn,
                                                             conditions = colData(rv.aggregation$dataIn)[['Condition']]
              )
              #aggregateIterParallel(rv$current.obj, X, init.method='Sum', method='onlyN', n=input$nTopn)
            }
          }
        } else {
          #X <- rv$matAdj$matWithUniquePeptides
          X <- "onlySpec"
          if (input$AggregationConsider == 'allPeptides') {
            rv.aggregation$dataIn <- aggregateFeatures_sam(object = rv.aggregation$dataIn,
                                                           i = rv.aggregation$i,
                                                           aggType = X,
                                                           name = "peptides_aggregation",
                                                           fun = paste0("agg",input$AggregationOperator)
            )
            #do.call(paste0('aggregate',input$AggregationOperator),list(obj.pep=rv$current.obj,X=X))
          } else {
            rv.aggregation$dataIn <- aggregateFeatures_sam(object = rv.aggregation$dataIn,
                                                           i = rv.aggregation$i,
                                                           aggType = X,
                                                           name = "peptides_aggregation",
                                                           fun = 'aggTopn',
                                                           method = input$AggregationOperator,
                                                           n = as.numeric(input$nTopn)
            )
            #aggregateTopn(rv$current.obj, X, input$AggregationOperator,n=as.numeric(input$nTopn))
          }
        }
      } )
      rv.aggregation$i <- ind() + 1
      r.nav$isDone[1] <- TRUE
      
    })
  })
  
  
  
  
  output$ObserverAggregationDone <- renderUI({
    req(rv.aggregation$temp.aggregate)
    req(input$perform.aggregation)
    isolate({
      h3("Aggregation done")
      
    })
  })
  
  
  # output$specificPeptideBarplot <- renderUI({
  #   #req(rv$matAdj)
  #   tagList(
  #     h4("Only specific peptides"),
  #     plotOutput(ns("aggregationPlotUnique"), width="400px") #%>% shinycssloaders::withSpinner(type=spinnerType)
  #   )
  # })
  # 
  # 
  # output$aggregationPlotUnique <- renderPlot({
  #   GraphPepProt_hc(as.matrix(metadata(rv.aggregation$dataIn[[rv.aggregation$i]])[["list.matAdj"]][["onlySpec"]]))
  # })
  # 
  # 
  # output$allPeptideBarplot <- renderUI({
  #   #req(rv$matAdj)
  #   tagList(
  #     h4("All (specific & shared) peptides"),
  #     plotOutput(ns("aggregationPlotShared"), width="400px") #%>% shinycssloaders::withSpinner(type=spinnerType)
  #   )
  # })
  # 
  # output$aggregationPlotShared <- renderPlot({
  #   GraphPepProt_hc(as.matrix(metadata(rv.aggregation$dataIn[[rv.aggregation$i]])[["list.matAdj"]][["onlyShared"]]))
  # })
  # 
  
  
  
  
  
  
  output$aggregationStats <- DT::renderDataTable ({
    #req(input$proteinId)
    #req(rv.aggregation$dataIn[[rv.aggregation$i]])
    #print("toto")
    #print(rv.aggregation$widgets$proteinId)
    if (is.null(rv.aggregation$widgets$proteinId) || rv.aggregation$widgets$proteinId == "None") {return(NULL)}
    
    res <- getProteinsStats(metadata(rv.aggregation$dataIn[[rv.aggregation$i]])[["list.matAdj"]][["onlyShared"]])
    #print(res)
    rv$AggregProtStats$nb <- c(res$nbPeptides,
                               res$nbSpecificPeptides,
                               res$nbSharedPeptides,
                               res$nbProt,
                               length(res$protOnlyUniquePep),
                               length(res$protOnlySharedPep),
                               length(res$protMixPep))
    
    df <- as.data.frame(rv$AggregProtStats)
    names(df) <- c('Description', 'Value')
    DT::datatable(df, 
                  escape = FALSE,
                  rownames= FALSE,
                  extensions = c('Scroller', 'Buttons'),
                  option=list(initComplete = initComplete(),
                              dom = 'Brt',
                              autoWidth=TRUE,
                              ordering=F,
                              columnDefs = list(list(width='150px',targets= 0),
                                                list(width='100px',targets= 1))
                  )
    )
  })
  
  
  ###---------------------------------------------------------------------------------###
  ###                                 Screen 2                                        ###
  ###---------------------------------------------------------------------------------###
  
  output$Aggregation_Step2 <- renderUI({
    
    req(rv.aggregation$dataIn[[rv.aggregation$i]])
    
    #if (rv$current.obj@experimentData@other$typeOfData == typePeptide) {
    choices <- colnames(rowData(rv.aggregation$dataIn[[rv.aggregation$i]]))
    names(choices) <- colnames(rowData(rv.aggregation$dataIn[[rv.aggregation$i]]))
    tagList(
      uiOutput(ns("displayNbPeptides")),
      
      div(
        div(
          style="display:inline-block; vertical-align: middle; padding-right: 20px;",
          mod_popover_for_help_ui(ns("modulePopover_colsForAggreg"))
        ),
        div(
          style="display:inline-block; vertical-align: middle;",
          selectInput(ns("columnsForProteinDataset.box"),
                      label = "",
                      choices = choices,
                      multiple = TRUE, width='200px',
                      size = 10,
                      selectize = FALSE)
        )
      )
    )
    
    # } else {
    #   tagList(
    #     h4("The peptide dataset has been aggregated into a protein dataset."),
    #     tags$div(style="align: center;",
    #              moduleStaticDataTableUI("overview_Aggregation")
    #     )
    #   )
    # }
    
  })
  
  
  output$displayNbPeptides <- renderUI({
    req(input$filterProtAfterAggregation)
    
    if (input$filterProtAfterAggregation) {
      numericInput(ns("nbPeptides"), "Nb of peptides defining a protein", 
                   value = 0, min =0, step=1,
                   width = "250px")
    }
  })
  
  
  output$columnsForProteinDataset <- renderUI({
    req(rv.aggregation$dataIn[[rv.aggregation$i]])
    
    choices <- colnames(rowData(rv.aggregation$dataIn[[rv.aggregation$i]]))
    names(choices) <- colnames(rowData(rv.aggregation$dataIn[[rv.aggregation$i]]))
    
    selectInput(ns("columnsForProteinDataset.box"),
                label = "",
                choices = choices,
                multiple = TRUE, width='200px',
                size = 20,
                selectize = FALSE)
    
  })
  
  ###---------------------------------------------------------------------------------###
  ###                                 Screen 3                                        ###
  ###---------------------------------------------------------------------------------###
  output$screenAggregation3 <- renderUI({
    tagList(
      actionButton(ns("valid.aggregation"),"Save aggregation", class = actionBtnClass)
    )
  })
  
  
  
  observeEvent(input$valid.aggregation,{ 
    
    req(rv.aggregation$temp.aggregate)
    
    isolate({
      withProgress(message = '',detail = '', value = 0, {
        
        X <- NULL
        if(input$radioBtn_includeShared %in% c("Yes2", "Yes1")){
          X <- metadata(rv.aggregation$dataIn[[rv.aggregation$i]])[["list.matAdj"]][["onlyShared"]]}
        else { X <- metadata(rv.aggregation$dataIn[[rv.aggregation$i]])[["list.matAdj"]][["onlySpec"]]}
        
        
        total <- 60
        delta <- round(total / length(input$columnsForProteinDataset.box))
        cpt <- 10
        for(c in input$columnsForProteinDataset.box){
          newCol <- BuildColumnToProteinDataset(
            rowfData(rv.aggregation$dataIn[[rv.aggregation$i]]), X, c, rownames(rowData(rv$temp.aggregate)))
          cnames <- colnames(rowData(rv$temp.aggregate))
          rowData(rv.aggregation$temp.aggregate) <- 
            data.frame(rowData(rv.aggregation$temp.aggregate), newCol)
          colnames(rowData(rv.aggregation$temp.aggregate)) <- c(cnames, c)
          cpt <- cpt + delta
          incProgress(cpt/100, detail = paste0('Processing column ', c))
        }
        
        rv.aggregation$dataIn[[rv.aggregation$i]] <- rv.aggregation$temp.aggregate
        
        rv$current.obj@experimentData@other$Prostar_Version <- 
          installed.packages(lib.loc = Prostar.loc)["Prostar","Version"]
        rv$current.obj@experimentData@other$DAPAR_Version <- 
          installed.packages(lib.loc = DAPAR.loc)["DAPAR","Version"]
        rv$typeOfDataset <- rv$current.obj@experimentData@other$typeOfData
        
        name <- paste0("Aggregated", ".", rv$typeOfDataset)
        rv.aggregation$dataIn[[rv.aggregation$i]] <- saveParameters(rv.aggregation$dataIn[[rv.aggregation$i]], name,"Aggregation",build_ParamsList_Aggregation())
        
        rv$dataset[[name]] <- rv.aggregation$dataIn[[rv.aggregation$i]]
        r.nav$isDone[3] <- TRUE
        #updatePB(session,inputId="pb_SaveAggregation",value=70,text_value="70 %", striped = TRUE, active=TRUE)
        #updatePB(session,inputId="pb_SaveAggregation",value=90,text_value="90 %", striped = TRUE, active=TRUE)
        #}
        
        updateSelectInput(session, "datasets",  choices = names(rv$dataset), selected = name)
        
        
      })
    })
    
    
    return(reactive({rv.aggregation$dataOut}))
    
  })
  
}
