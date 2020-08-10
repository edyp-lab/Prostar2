library(QFeatures)
library(shiny)
library(Prostar2)
source(file.path('../R', 'global.R'), local=TRUE)$value
source(file.path('../R', 'mod_navigation.R'), local=TRUE)$value



#' pipe_prot_filter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_pipe_prot_filter_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_navigation_ui(ns('nav_pipe_process'))
  )
}
    
#' pipe_prot_filter Server Function
#'
#' @noRd 
mod_pipe_prot_filter_server <- function(input, output, session){
  ns <- session$ns
 
  ## Section navigation module
  # Variable to manage the different screens of the module
  r.nav <- reactiveValues(
    name = "Filtering",
    stepsNames = c("MV filtering", "String-based filtering","Numerical filtering", "Summary", "Validate"),
    ll.UI = list( screenStep1 = uiOutput(ns("Screen_Process_1")),
                  screenStep2 = uiOutput(ns("Screen_Process_2")),
                  screenStep3 = uiOutput(ns("Screen_Process_3")),
                  screenStep4 = uiOutput(ns("Screen_Process_4")),
                  screenStep5 = uiOutput(ns("Screen_Process_5"))
    ),
    isDone =  rep(FALSE,5),
    mandatory =  rep(FALSE,5),
    reset = FALSE
  )
  
  ## reactive values for variables in the module
  rv.process <- reactiveValues(
    name = "processProtNorm",
    dataIn = NULL,
    dataOut = NULL,
    widgets = list(ChooseFilters = "None",
                   seuilNA = 0,
                   DT_filterSummary = data.frame(Filter=NULL, 
                                                 Prefix=NULL,
                                                 nbDeleted=NULL, 
                                                 Total=NULL, 
                                                 stringsAsFactors=F),
                   DT_numfilterSummary = data.frame(Filter=NULL, 
                                                    Condition=NULL,
                                                    nbDeleted=NULL, 
                                                    Total=NULL, 
                                                    stringsAsFactors=F)
                  )
  )
  
  
  observeEvent(req(r.nav$reset),{
    
    rv.process$widgets <- list(ChooseFilters = "None",
                               seuilNA = 0,
                               DT_filterSummary = data.frame(Filter=NULL, 
                                                             Prefix=NULL,
                                                             nbDeleted=NULL, 
                                                             Total=NULL, 
                                                             stringsAsFactors=F),
                               DT_numfilterSummary = data.frame(Filter=NULL, 
                                                                Condition=NULL,
                                                                nbDeleted=NULL, 
                                                                Total=NULL, 
                                                                stringsAsFactors=F)
                            )
    
    ## do not modify this part
    rv.process$dataIn <- obj()
    rv.process$data <- data.frame()
    r.nav$isDone <- rep(FALSE, 5)
    r.nav$reset <- FALSE
    ## end of no modifiable part
  })
  
  callModule(mod_navigation_server, 'nav_pipe_process', style=2, pages=r.nav)
  
  #### END of template part of the module
  
  
  ##
  ##  
  ## Calls to other modules
  ##
  ##
  callModule(missingValuesPlots,"MVPlots_filtering",
             data=reactive({rv.process$dataIn}),
             palette = reactive({rv$PlotParams$paletteConditions})
  )
  
  callModule(moduleFilterStringbasedOptions,"filteringStringBasedOptions")
  callModule(modulePopover,"modulePopover_keepVal", data = reactive(list(title=tags$b("Keep vals"),
                                                                         content= "The user-defined threshold allows to tune the minimum amount of non-NA values for each line to be kept in the dataset (the line is filtered out otherwise). The threshold either applies on the whole dataset, on each condition or on at least one condition.")))
  
  observe({
    req(obj())
    rv.process$dataIn <- obj()
  })
  
  
  disableActionButton <- function(id,session) {
    session$sendCustomMessage(type="jsCode",
                              list(code= paste("$('#",id,"').prop('disabled',true)"
                                               ,sep="")))
  }
  
  ##
  ## Definitions of the screens
  ##
  
  ###---------------------------------------------------------------------------------###
  ###                                 Screen 1                                        ###
  ###---------------------------------------------------------------------------------###
  output$screenFiltering1 <- renderUI({
    
    isolate({
      tagList(
        div(
          id = "screen1Filtering",
          # tags$div(
          div(style="display:inline-block; vertical-align: middle; padding-right: 40px;",
              selectInput(ns("ChooseFilters"),"Type",  
                          choices = gFiltersList, 
                          selected=rv.process$widgets$ChooseFilters,
                          width='200px')
          ),
          div( style="display:inline-block; vertical-align: middle;  padding-right: 40px;",
               uiOutput(ns("seuilNADelete"))
          ),
          div( style="display:inline-block; vertical-align: middle;",
               actionButton(ns("perform.filtering.MV"), "Perform MV filtering", class = actionBtnClass)
          ),
          hr(),
          missingValuesPlotsUI(ns("MVPlots_filtering")),
          uiOutput(ns("ObserverMVFilteringDone"))
        )
        
      )
    })
    
  })
  
  
  
  ##
  ## Perform missing values filtering
  observeEvent(input$perform.filtering.MV,ignoreInit=TRUE,{
    print("In : observeEvent(input$perform.filtering.MV")
    rv.process$widgets$ChooseFilters
    input$seuilNA
    
    if (rv.process$widgets$ChooseFilters == gFilterNone){
      #rv$current.obj <- rv$dataset[[input$datasets]]
    } else {
      keepThat <- mvFilterGetIndices(rv$current.obj,
                                     rv.process$widgets$ChooseFilters,
                                     as.integer(input$seuilNA))
      if (!is.null(keepThat))
      {
        rv$deleted.mvLines <- rv$current.obj[-keepThat]
        rv$current.obj <- mvFilterFromIndices(rv$current.obj,
                                              keepThat,
                                              GetFilterText(rv.process$widgets$ChooseFilters, as.integer(input$seuilNA)))
        
        rvModProcess$moduleFilteringDone[1] <- TRUE
      }
    }
    #updateSelectInput(session, "ChooseFilters", selected = input$ChooseFilters)
    #updateSelectInput(session, "seuilNA", selected = input$seuilNA)
  })
  
  output$seuilNADelete <- renderUI({ 
    req(rv.process$widgets$ChooseFilters)
    
    if ((rv.process$widgets$ChooseFilters=="None") || (rv.process$widgets$ChooseFilters==gFilterEmptyLines)) {return(NULL)   }
    print(rv$current.obj)
    choix <- getListNbValuesInLines(rv$current.obj, type=rv.process$widgets$ChooseFilters)
    tagList(
      modulePopoverUI(ns("modulePopover_keepVal")),
      
      selectInput(ns("seuilNA"), NULL,
                  choices = choix,
                  selected = rv.process$widgets$seuilNA,
                  width='150px'))
    
  })
  
  
  output$ObserverMVFilteringDone <- renderUI({
    req(rv$deleted.mvLines)
    #isolate({
    
    n <- 0
    if(!is.null(rv$deleted.mvLines)){n <- nrow(rv$deleted.mvLines)}
    if (!rvModProcess$moduleFilteringDone[1])
    {return(NULL)  }
    else {
      h5(paste0("Missing values filtering done. ",n, " lines were deleted."))
    }
    
    # })
  })
  
  
  output$choixFiltres <- renderUI({
    req(input$file)
    radioButtons(ns("ChooseFilters"),"Filtering options",choices = gFiltersList)
    
  })
  
  
  
  output$helpTextMV <- renderUI({
    helpText("After checking the data, validate the filters.")
  })
  
  
  ###---------------------------------------------------------------------------------###
  ###                                 Screen 2                                        ###
  ###---------------------------------------------------------------------------------###
  output$screenFiltering2 <- renderUI({
    tagList(
      
      #   id = "screen2Filtering",
      tags$div(
        tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  selectInput(ns("symFilter_cname"), "Column name", choices = Get_symFilter_cname_choice())
        ),
        div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
             textInput(ns("symFilter_tagName"), "Prefix", value = "", width='50px')
        ),
        div( style="display:inline-block; vertical-align: middle;",
             p(""),actionButton(ns("actionButtonFilter"), "Perform", class = actionBtnClass)
        )
      ),
      hr(),
      div(
        div( style="display:inline-block; vertical-align: middle; align: center;",
             DT::dataTableOutput(ns("FilterSummaryData"))
        )
      )
      
    )
  })
  
  
  observeEvent(input$ChooseFilters,{
    rv.process$widgets$ChooseFilters <- input$ChooseFilters
  })
  
  observeEvent(input$seuilNA, ignoreNULL = TRUE,ignoreInit = TRUE, {
    rv.process$widgets$seuilNA <- input$seuilNA
  })
  
  
  #-----------------------------------------------
  output$ObserverStringBasedFilteringDone <- renderUI({
    
    isolate({
      if (!isDone[2]) 
      {return(NULL)  }
      else {
        h3("String-based filtering done")
      }
      
    })
  })
  
  ##  ---------------------------------------------------------
  ## perform symbolic filter
  ## ----------------------------------------------------------
  observeEvent(input$actionButtonFilter,{
    req(input$symFilter_cname)
    temp <- rv$current.obj
    
    if (input$symFilter_cname=="None"){return()}
    cname <- input$symFilter_cname
    tagName <- input$symFilter_tagName
    res <- StringBasedFiltering2(temp,cname, input$symFilter_tagName)
    nbDeleted <- 0
    
    if (!is.null(res[["deleted"]])){
      rv$deleted.stringBased <- rbindMSnset(rv$deleted.stringBased, res[["deleted"]])
      nbDeleted <-  nrow(res[["deleted"]])
    } else {
      nbDeleted <-  0
    }                          
    rv$current.obj <- res[["obj"]]
    rvModProcess$moduleFilteringDone[2] <- TRUE
    
    df <- data.frame(Filter=cname, Prefix=tagName, nbDeleted=nbDeleted, Total=nrow(rv$current.obj))
    rv.process$widgets$DT_filterSummary <- rbind(rv.process$widgets$DT_filterSummary , df)
  })
  
  
  Get_symFilter_cname_choice <- reactive({
    req(rv$current.obj)
    choice <- c("None", colnames(fData(rv$current.obj)))
    choice
  })
  
  
  ###---------------------------------------------------------------------------------###
  ###                                 Screen 3                                        ###
  ###---------------------------------------------------------------------------------###
  output$screenFiltering3 <- renderUI({
    req(rv$current.obj)
    
    ll <- lapply(fData(rv$current.obj), function(x){is.numeric(x)})
    choice <- c("None", colnames(fData(rv$current.obj))[which(ll == TRUE)])
    
    tagList(
      tags$div(
        tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  selectInput(ns("numericFilter_cname"), "Column name", choices = choice)
        ),
        
        tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  selectInput(ns("numericFilter_operator"), "Operator", 
                              choices = c('None' = '',
                                          '==' = '==',
                                          '<=' = '<=',
                                          '<' = '<',
                                          '>=' = '>=',
                                          '>' = '>',
                                          '!=' = '!='), width='100px')
        ),
        tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  numericInput(ns("numericFilter_value"), "Value", value = "", width='100px')
        ),
        tags$div( style="display:inline-block; vertical-align: middle;",
                  p(""),actionButton(ns("btn_numFilter"), "Perform", class = actionBtnClass)
        )
      ),
      tags$hr(),
      tags$div(
        tags$div( style="display:inline-block; vertical-align: middle; align: center;",
                  DT::dataTableOutput(ns("numericalFilterSummaryData"))
        )
      )
      
    )
  })
  
  
  ## ----------------------------------------------
  # Perform numerical filtering
  observeEvent(input$btn_numFilter,ignoreInit=TRUE,{
    temp <- rv$current.obj
    
    if (input$numericFilter_cname=="None"){return()}
    cname <- input$numericFilter_cname
    tagValue <- input$numericFilter_value
    
    print(input$numericFilter_value)
    print(input$numericFilter_operator)
    res <- NumericalFiltering(temp,cname, input$numericFilter_value,input$numericFilter_operator)
    nbDeleted <- 0
    
    
    if (!is.null(res[["deleted"]])){
      rv$deleted.numeric <- rbindMSnset(rv$deleted.numeric, res[["deleted"]])
      nbDeleted <-  nrow(res[["deleted"]])
    } else {
      nbDeleted <-  0
    }                          
    rv$current.obj <- res[["obj"]]
    rvModProcess$moduleFilteringDone[3] <- TRUE
    
    df <- data.frame(Filter=cname, 
                     Condition=paste0(input$numericFilter_operator,' ',tagValue), 
                     nbDeleted=nbDeleted, 
                     Total=nrow(rv$current.obj))
    rv.process$widgets$DT_numfilterSummary <- rbind(rv.process$widgets$DT_numfilterSummary, df)
    
  })
  
  
  
  ### ------------------------------------------------------------
  output$numericalFilterSummaryData <- DT::renderDataTable(server=TRUE,{
    req(rv$current.obj)
    req(rv.process$widgets$DT_numfilterSummary)
    
    isolate({
      if (nrow(rv.process$widgets$DT_numfilterSummary) == 0){
        df <- data.frame(Filter=NA, Condition=NA, nbDeleted=NA, Total=nrow(rv$current.obj), stringsAsFactors = FALSE)
        rv.process$widgets$DT_numfilterSummary <- rbind(rv.process$widgets$DT_numfilterSummary ,df)
      }
      
      
      DT::datatable(rv.process$widgets$DT_numfilterSummary,
                    extensions = c('Scroller', 'Buttons'),
                    rownames = FALSE,
                    
                    options=list(initComplete = initComplete(),
                                 buttons = list('copy',
                                                list(
                                                  extend = 'csv',
                                                  filename = 'NumericalFiltering_summary'
                                                ),'print'),
                                 dom='Brt',
                                 deferRender = TRUE,
                                 bLengthChange = FALSE
                    ))
    })
    
  })
  
  
  output$ObserverNumericalFilteringDone <- renderUI({
    req(rv$current.obj)
    rv$numericalFiltering_Done
    
    isolate({
      if (!rv$numericalFiltering_Done) 
      {return(NULL)  }
      else {
        h3("Numerical filtering done")
      }
      
    })
  })
  
  
  ###---------------------------------------------------------------------------------###
  ###                                 Screen 4                                        ###
  ###---------------------------------------------------------------------------------###
  output$screenFiltering4 <- renderUI({
    
    tagList(
      fluidRow(
        column(width=3,radioButtons(ns("ChooseTabAfterFiltering"),  "Choose the data to display",
                                    choices= list("Quantitative data" = "quantiData", "Meta data" = "metaData"),selected=character(0))),
        column(width=3,radioButtons(ns("ChooseViewAfterFiltering"), "Type of filtered data", 
                                    choices= list("Deleted on missing values" = "MissingValues",
                                                  "Deleted string based" = "StringBased",
                                                  "Deleted numeric filter" = "Numerical"),
                                    selected=character(0))),
        column(width=3,uiOutput(ns("legendForExprsData2")))
      ),
      hr(),
      uiOutput(ns("helpTextMV")),
      uiOutput(ns("Warning_VizualizeFilteredData")),
      DT::dataTableOutput(ns("VizualizeFilteredData"))
      
    )
  })
  
  
  output$FilterSummaryData <- DT::renderDataTable(server=TRUE,{
    req(rv$current.obj)
    req(rv.process$widgets$DT_numfilterSummary)
    isolate({
      
      if (nrow(rv.process$widgets$DT_filterSummary )==0){
        df <- data.frame(Filter="-", Prefix="-", nbDeleted=0, Total=nrow(rv$current.obj), stringsAsFactors = FALSE)
        #rv.process$widgets$DT_filterSummary <- rbind(rv.process$widgets$DT_numfilterSummary ,df)
        rv.process$widgets$DT_filterSummary <- df
      }
      
      
      DT::datatable(rv.process$widgets$DT_filterSummary,
                    extensions = c('Scroller', 'Buttons'),
                    rownames = FALSE,
                    options=list(buttons = list('copy',
                                                list(
                                                  extend = 'csv',
                                                  filename = 'Filtering_summary'
                                                ),'print'),
                                 dom='Brt',
                                 initComplete = initComplete(),
                                 deferRender = TRUE,
                                 bLengthChange = FALSE
                    ))
    })
  })
  
  
  
  getDataForNumericalFiltered <- reactive({
    req(rv$settings_nDigits)
    rv$deleted.numeric
    table <- as.data.frame(round(Biobase::exprs(rv$deleted.numeric),digits=rv$settings_nDigits))
    table <- cbind(table, Biobase::fData(rv$deleted.numeric)[,rv$deleted.numeric@experimentData@other$OriginOfValues])
    
    table
  })
  
  
  
  getDataForMVFiltered <- reactive({
    req(rv$settings_nDigits)
    rv$deleted.mvLines
    
    table <- as.data.frame(round(Biobase::exprs(rv$deleted.mvLines),digits=rv$settings_nDigits))
    table <- cbind(table, Biobase::fData(rv$deleted.mvLines)[,rv$deleted.mvLines@experimentData@other$OriginOfValues])
    
    table
  })
  
  
  
  
  getDataForMVStringFiltered <- reactive({
    req(rv$settings_nDigits)
    rv$deleted.stringBased
    table <- as.data.frame(round(Biobase::exprs(rv$deleted.stringBased),digits=rv$settings_nDigits))
    table <- cbind(table, Biobase::fData(rv$deleted.stringBased)[,rv$deleted.stringBased@experimentData@other$OriginOfValues])
    
    table
  })
  
  
  output$legendForExprsData2 <- renderUI({
    req(input$ChooseTabAfterFiltering)
    
    if (input$ChooseTabAfterFiltering != "quantiData"){return(NULL)}
    moduleLegendColoredExprsUI(ns("FilterColorLegend_DS"), rv$colorsTypeMV)
    
  })
  
  
  
  output$Warning_VizualizeFilteredData <- renderUI({
    if (length(GetDataFor_VizualizeFilteredData())==0){return(NULL)}
    if (nrow(GetDataFor_VizualizeFilteredData())>153) 
      p(MSG_WARNING_SIZE_DT)
    
  })
  
  
  
  GetDataFor_VizualizeFilteredData <- reactive({
    req(rv$settings_nDigits)
    rv$deleted.mvLines
    req(input$ChooseViewAfterFiltering)
    req(input$ChooseTabAfterFiltering)
    rv$deleted.stringBased
    rv$deleted.numeric
    
    
    data <- NULL
    if ((input$ChooseViewAfterFiltering == "MissingValues") && !is.null(rv$deleted.mvLines))
    {
      #print("DANS REACTIVE : If 1")
      #print(dim(getDataForMVFiltered()))
      switch(input$ChooseTabAfterFiltering,
             quantiData =  data <- getDataForMVFiltered(),
             metaData = data <- cbind(ID = rownames(Biobase::fData(rv$deleted.mvLines)), Biobase::fData(rv$deleted.mvLines))
      )
    } 
    
    else if ((input$ChooseViewAfterFiltering == "StringBased") && !is.null(rv$deleted.stringBased)) {
      
      #print("DANS REACTIVE : If 2")
      switch(input$ChooseTabAfterFiltering,
             quantiData =  data <- getDataForMVStringFiltered(),
             metaData = data <- Biobase::fData(rv$deleted.stringBased)
      )
    }  else if ((input$ChooseViewAfterFiltering == "Numerical") && !is.null(rv$deleted.numeric)) {
      #print("DANS REACTIVE : If 3")
      switch(input$ChooseTabAfterFiltering,
             quantiData =  data <- getDataForNumericalFiltered(),
             metaData = data <- Biobase::fData(rv$deleted.numeric)
      )
    }
    
    # print("END OF REACTIVE")
    #print(data)
    data
  })
  
  
  
  #----------------------------------------------
  output$VizualizeFilteredData <- DT::renderDataTable(server=TRUE,{
    input$ChooseTabAfterFiltering
    req(GetDataFor_VizualizeFilteredData())
    dt <- NULL
    data <- GetDataFor_VizualizeFilteredData()
    
    if(input$ChooseTabAfterFiltering =="quantiData"){
      dt <- DT::datatable( data,
                           extensions = c('Scroller', 'Buttons'),
                           options = list(
                             buttons = list('copy',
                                            list(
                                              extend = 'csv',
                                              filename = 'Prostar_export'),
                                            'print'),
                             dom='Brtip',
                             initComplete = initComplete(),
                             displayLength = 20,
                             deferRender = TRUE,
                             bLengthChange = FALSE,
                             scrollX = 200,
                             scrollY = 600,
                             scroller = TRUE,
                             ordering=FALSE,
                             columnDefs = list(list(targets = c(((ncol(data)/2)+1):ncol(data)), visible = FALSE),
                                               list(width='150px',targets= "_all"))
                           )
      ) %>%
        formatStyle(
          colnames(data)[1:(ncol(data)/2)],
          colnames(data)[((ncol(data)/2)+1):ncol(data)],
          backgroundColor = styleEqual(c("POV", "MEC"), c(rv$colorsTypeMV$POV, rv$colorsTypeMV$MEC))
        )
    } else {
      dt <- DT::datatable( data,
                           extensions = 'Scroller',
                           options = list(initComplete = initComplete(),
                                          displayLength = 20,
                                          deferRender = TRUE,
                                          bLengthChange = FALSE,
                                          scrollX = 200,
                                          scrollY = 600,
                                          scroller = TRUE,
                                          ordering=FALSE)) 
    }
    # }
    dt
    
  })
  
  ###---------------------------------------------------------------------------------###
  ###                                 Screen 5                                        ###
  ###---------------------------------------------------------------------------------###
  output$screenFiltering5 <- renderUI({     
    
    tagList(
      actionButton(ns("ValidateFilters"),"Save filtered dataset",class = actionBtnClass)
    )
  })
  
  
  observeEvent(input$ValidateFilters,ignoreInit = TRUE,{ 
    
    isolate({
      if((rv.process$widgets$ChooseFilters != gFilterNone) 
         || (nrow(rv.process$widgets$DT_filterSummary )>1)
         || (nrow(rv.process$widgets$DT_numfilterSummary )>1)){
        l.params <- build_ParamsList_Filtering()
        
        rv$typeOfDataset <- rv$current.obj@experimentData@other$typeOfData
        name <- paste0("Filtered", ".", rv$typeOfDataset)
        rv$current.obj <- saveParameters(rv$current.obj,name,"Filtering",l.params)
        
        dataOut<- rv$current.obj
        rvModProcess$moduleFilteringDone[5] <- TRUE
        
        if (rv$typeOfDataset == "peptide"  && !is.null(rv$proteinId)){
          ComputeAdjacencyMatrices()
          ComputeConnexComposants()
        }
        UpdateDatasetWidget(rv$current.obj, name)
      }
      rv.process$dataOut <- rv.process$dataIn
      rvModProcess$moduleFilteringDone[5] <- TRUE
    })
    
  })
  
  
  return({reactive(rv.process$dataOut)})
  
}
    
## To be copied in the UI
# mod_pipe_prot_filter_ui("pipe_prot_filter_ui_1")
    
## To be copied in the server
# callModule(mod_pipe_prot_filter_server, "pipe_prot_filter_ui_1")
 
