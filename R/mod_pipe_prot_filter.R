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
mod_pipe_prot_filter_server <- function(input, output, session, obj, ind){
  ns <- session$ns
 
  ## Section navigation module
  # Variable to manage the different screens of the module
  r.nav <- reactiveValues(
    name = "Filtering",
    stepsNames = c("MV filtering", "Field filtering", "Summary", "Validate"),
    ll.UI = list( screenStep1 = uiOutput(ns("Screen_Process_1")),
                  screenStep2 = uiOutput(ns("Screen_Process_2")),
                  screenStep3 = uiOutput(ns("Screen_Process_3")),
                  screenStep4 = uiOutput(ns("Screen_Process_4"))
    ),
    isDone =  rep(FALSE,4),
    mandatory =  rep(FALSE,4),
    reset = FALSE
  )
  
  ## reactive values for variables in the module
  rv.filter <- reactiveValues(
    name = "processProtNorm",
    dataIn = NULL,
    dataOut = NULL,
    i = NULL,
    settings = NULL,
    
    deleted.mvLines = NULL,
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
    
    rv.filter$widgets <- list(ChooseFilters = "None",
                               seuilNA = 0,
                               DT_filterSummary = data.frame(Filter=NULL, 
                                                             Prefix=NULL,
                                                             nbDeleted=NULL, 
                                                             Total=NULL, 
                                                             stringsAsFactors=F),
                               DT_fieldFilterSummary = data.frame(Filter=NULL, 
                                                                Condition=NULL,
                                                                nbDeleted=NULL, 
                                                                Total=NULL, 
                                                                stringsAsFactors=F)
                            )
    
    ## do not modify this part
    rv.filter$dataIn <- obj()
    rv.filter$i <- ind()
    rv.filter.deleted.mvLines <- NULL
    rv.filter.deleted.field <- NULL
    rv.filter$data <- data.frame()
    r.nav$isDone <- rep(FALSE, 4)
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
  callModule(mod_plots_group_mv_server,"MVPlots_filtering",
             obj = reactive({rv.filter$dataIn}),
             conds = reactive({colData(rv.filter$dataIn)}),
             base_palette = reactive({rv.filter$settings()$basePalette})
  )
  
  callModule(moduleFilterStringbasedOptions,"filteringStringBasedOptions")
  
  callModule(modulePopover,
             "modulePopover_keepVal", 
             data = reactive(list(title=tags$b("Keep vals"),
                                  content= "The user-defined threshold allows to tune the minimum amount of non-NA values for each line to be kept in the dataset (the line is filtered out otherwise). The threshold either applies on the whole dataset, on each condition or on at least one condition.")))
  
  observe({
    req(obj())
    rv.filter$dataIn <- obj()
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
                          selected=rv.filter$widgets$ChooseFilters,
                          width='200px')
          ),
          div( style="display:inline-block; vertical-align: middle;  padding-right: 40px;",
               uiOutput(ns("seuilNADelete"))
          ),
          div( style="display:inline-block; vertical-align: middle;",
               actionButton(ns("perform.filtering.MV"), "Perform MV filtering", class = actionBtnClass)
          ),
          hr(),
          mod_plots_group_mv_ui(ns("MVPlots_filtering")),
          uiOutput(ns("ObserverMVFilteringDone"))
        )
        
      )
    })
    
  })
  
  
  
  ##
  ## Perform missing values filtering
  observeEvent(input$perform.filtering.MV,ignoreInit=TRUE,{
    rv.filter$widgets$ChooseFilters
    rv.filter$widgets$seuilNA
    
    if (rv.filter$widgets$ChooseFilters == gFilterNone){
      #rv.filter$dataIn <- rv$dataset[[input$datasets]]
    } else {
      # keepThat <- mvFilterGetIndices(rv.filter$dataIn,
      #                                rv.filter$widgets$ChooseFilters,
      #                                as.integer(rv.filter$widgets$seuilNA))
      # if (!is.null(keepThat))
      # {
      #   rv.filter$deleted.mvLines <- rv.filter$dataIn[-keepThat]
      #   rv.filter$dataIn <- mvFilterFromIndices(object = rv.filter$dataIn,
      #                                         i = rv.filter$i,
      #                                         keepThat,
      #                                         GetFilterText(rv.filter$widgets$ChooseFilters, 
      #                                                       as.integer(rv.filter$widgets$seuilNA)
      #                                                       )
      #                                         )
      
      # create a duplicate of the last assay
      rv.filter$dataIn <-addAssay(rv.filter$dataIn, 
                                  rv.filter$dataIn[[length(experiments(rv.filter$dataIn))]],
                                  'na_filtered')
      
      rv.filter$dataIn <- MVrowsTagToOne(rv.filter$dataIn, 
                                         type = rv.filter$widgets$ChooseFilters, 
                                         th = as.integer(rv.filter$widgets$seuilNA), 
                                         percent = FALSE)
      
      ## keep rows where tagNA==0
      na_filter <- VariableFilter(field = "tagNA", value = "0", condition = "==")
      
      rv.filter$dataIn <- filterFeatures(rv.filter$dataIn, na_filter)
      rv.filter$dataIn <- removeAdditionalCol(rv.filter$dataIn, "tagNA")
      # key <- metadata(rv.filter$dataIn)$keyId
      # from <- length(experiments(rv.filter$dataIn)) -1
      # to <- length(experiments(rv.filter$dataIn))
      # rv.filter$dataIn <- addAssayLink(rv.filter$dataIn, from = from, to = to, varFrom = key, varTo = key)
      # 
      ## keep track of deleted rows
      rv.filter$deleted.mvLines <- setdiff(rowData(rv.filter$dataIn[[from]])[,metadata(rv.filter$dataIn)$keyId], rowData(rv.filter$dataIn[[to]])[,metadata(rv.filter$dataIn)$keyId])
      
      rv.filter$i <- ind() + 1
      r.nav$isDone[1] <- TRUE
     # }
    }
    #updateSelectInput(session, "ChooseFilters", selected = input$ChooseFilters)
    #updateSelectInput(session, "seuilNA", selected = input$seuilNA)
  })
  
  output$seuilNADelete <- renderUI({ 
    req(rv.filter$widgets$ChooseFilters)
    
    if ((rv.filter$widgets$ChooseFilters=="None") || (rv.filter$widgets$ChooseFilters==gFilterEmptyLines)) {return(NULL)   }
    choice <- getListNbValuesInLines(rv.filter$dataIn, type=rv.filter$widgets$ChooseFilters)
    tagList(
      modulePopoverUI(ns("modulePopover_keepVal")),
      
      selectInput(ns("seuilNA"), NULL,
                  choices = choice,
                  selected = rv.filter$widgets$seuilNA,
                  width='150px'))
    
  })
  
  
  output$ObserverMVFilteringDone <- renderUI({
    req(rv.norm$deleted.mvLines)
    #isolate({
    
    n <- 0
    if(!is.null(rv.norm$deleted.mvLines))
      n <- nrow(rv.norm$deleted.mvLines)
    
    if (!r.nav$isDone[1])
    {
      return(NULL)  
      } else {
      h5(paste0("Missing values filtering done. ",n, " lines were deleted."))
    }
    
    # })
  })
  
  output$helpTextMV <- renderUI({
    helpText("After checking the data, validate the filters.")
  })
  
  
  
  observeEvent(input$ChooseFilters, ignoreInit=TRUE,{
    rv.filter$widgets$ChooseFilters <- input$ChooseFilters
  })
  observeEvent(input$seuilNA, ignoreInit=TRUE,{
    rv.filter$widgets$seuilNA <- input$seuilNA
  })
  
  
  
  ###---------------------------------------------------------------------------------###
  ###                                 Screen 2                                        ###
  ###---------------------------------------------------------------------------------###
  output$screenFiltering2 <- renderUI({
    #req(rv.filter$dataIn)
    
   # ll.num <- lapply(fData(rv.filter$dataIn), function(x){is.numeric(x)})
    #ll.char <- lapply(fData(rv.filter$dataIn), function(x){is.character(x)})
    
    #choice <- c("None", colnames(fData(rv.filter$dataIn))[which(ll == TRUE)])
    choice_field <- c("None", colnames(fData(rv.filter$dataIn)))
    
    tagList(
      tags$div(
        tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  selectInput(ns("fieldName"), "Column name", choices = choice_field)
        ),
        
    
     
    tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  uiOutput(ns('chooseOperator'))
        ),
        tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  numericInput(ns("fieldFilter_value"), "Value", value = "", width='100px')
        ),
        tags$div( style="display:inline-block; vertical-align: middle;",
                  p(""),actionButton(ns("btn_fieldFilter"), "Perform", class = actionBtnClass)
        )
      ),
      tags$hr(),
      tags$div(
        tags$div( style="display:inline-block; vertical-align: middle; align: center;",
                  DT::dataTableOutput(ns("fieldFilterSummaryData"))
        )
      )
      
    )
  })
  
  
  output$chooseOperator <- renderUI({
    req(rv.filter$widgets$fieldName)
    
    numeric_operators <- c('None' = '',
                           '==' = '==',
                           '<=' = '<=',
                           '<' = '<',
                           '>=' = '>=',
                           '>' = '>',
                           '!=' = '!=')
    
    character_operators <- c('None' = '',
                             '==' = '==',
                             '!=' = '!=')
    
    if (is.numeric(rv.filter$dataIn[rv.filter$widgets$fieldName]))
      operators <- numeric_operators
    else if (is.character(rv.filter$dataIn[rv.filter$widgets$fieldName]))
      operators <- character_operators
    
    selectInput(ns("operator"), "Operator", choices = operators, width='100px')
    
  })
  
  ## ----------------------------------------------
  # Perform field filtering
  observeEvent(input$btn_fieldFilter,ignoreInit=TRUE,{
    req(rv.filter$dataIn[rv.filter$widgets$fieldName])
    
    temp <- rv.filter$dataIn
    
    if (rv.filter$widgets$fieldName=="None"){return()}
    
    fieldname <- rv.filter$widgets$fieldName
    tagValue <- rv.filter$widgets$fieldFilter_value
    
    # create a duplicate of the last assay
    rv.filter$dataIn <-addAssay(rv.filter$dataIn, 
                                rv.filter$dataIn[[length(experiments(rv.filter$dataIn))]],
                                paste0('field_filtered_', length(experiments(rv.filter$dataIn)))
    )
    
    if (is.numeric(rv.filter$dataIn[rv.filter$widgets$fieldName]))
      field_filter <- VariableFilter(field = rv.filter$widgets$fieldName, 
                                     value = as.numeric(rv.filter$widgets$fieldFilter_value), 
                                     condition = rv.filter$widgets$operator)
    else if (is.character(rv.filter$dataIn[rv.filter$widgets$fieldName]))
      field_filter <- VariableFilter(field = rv.filter$widgets$fieldName, 
                                     value = as.character(rv.filter$widgets$fieldFilter_value), 
                                     condition = rv.filter$widgets$operator)
    
    
    rv.filter$dataIn <- filterFeatures(rv.filter$dataIn, field_filter)
    
    ## keep track of deleted rows
    from <- length(experiments(rv.filter$dataIn))-1
    to <-length(experiments(rv.filter$dataIn))
    rv.filter$deleted.field <- setdiff(rowData(rv.filter$dataIn[[from]])[,metadata(rv.filter$dataIn)$keyId],
                                       rowData(rv.filter$dataIn[[to]])[,metadata(rv.filter$dataIn)$keyId])
    
    
    df <- data.frame(Filter = rv.filter$widgets$fieldName, 
                     Condition = paste0(rv.filter$widgets$operator,' ',rv.filter$widgets$fieldFilter_value), 
                     nbDeleted = length(rv.filter$deleted.field), 
                     Total = nrow(rv.filter$dataIn[[to]]))
    
    rv.filter$widgets$DT_fieldfilterSummary <- rbind(rv.filter$widgets$DT_fieldfilterSummary, df)
    rv.filter$i <- ind() + 1
    r.nav$isDone[2] <- TRUE
    
  })
  
  
  
  ### ------------------------------------------------------------
  output$fieldFilterSummaryData <- DT::renderDataTable(server=TRUE,{
    req(rv.filter$dataIn)
    req(rv.filter$widgets$DT_fieldfilterSummary)
    
    isolate({
      if (nrow(rv.filter$widgets$DT_fieldfilterSummary) == 0){
        df <- data.frame(Filter=NA, 
                         Condition=NA, 
                         nbDeleted=NA, 
                         Total=nrow(rv.filter$dataIn), 
                         stringsAsFactors = FALSE)
        rv.filter$widgets$DT_fieldfilterSummary <- rbind(rv.filter$widgets$DT_fieldfilterSummary ,df)
      }
      
      
      DT::datatable(rv.filter$widgets$DT_fieldfilterSummary,
                    extensions = c('Scroller', 'Buttons'),
                    rownames = FALSE,
                    
                    options=list(initComplete = initComplete(),
                                 buttons = list('copy',
                                                list(
                                                  extend = 'csv',
                                                  filename = 'fieldFiltering_summary'
                                                ),'print'),
                                 dom='Brt',
                                 deferRender = TRUE,
                                 bLengthChange = FALSE
                    ))
    })
    
  })
  
  
  
  observeEvent(input$operator, ignoreInit=TRUE,{
    rv.filter$widgets$operator <- input$operator
  })
  observeEvent(input$fieldFilter_value, ignoreInit=TRUE,{
    rv.filter$widgets$fieldFilter_value <- input$fieldFilter_value
  })
  
  
  observeEvent(input$fieldName, ignoreInit=TRUE,{
    rv.filter$widgets$fieldName <- input$fieldName
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
    req(rv.filter$dataIn)
    req(rv.filter$widgets$DT_numfilterSummary)
    isolate({
      
      if (nrow(rv.filter$widgets$DT_filterSummary )==0){
        df <- data.frame(Filter="-", Prefix="-", nbDeleted=0, Total=nrow(rv.filter$dataIn), stringsAsFactors = FALSE)
        #rv.filter$widgets$DT_filterSummary <- rbind(rv.filter$widgets$DT_numfilterSummary ,df)
        rv.filter$widgets$DT_filterSummary <- df
      }
      
      
      DT::datatable(rv.filter$widgets$DT_filterSummary,
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
      if((rv.filter$widgets$ChooseFilters != gFilterNone) 
         || (nrow(rv.filter$widgets$DT_filterSummary )>1)
         || (nrow(rv.filter$widgets$DT_numfilterSummary )>1)){
        l.params <- build_ParamsList_Filtering()
        
        rv$typeOfDataset <- rv.filter$dataIn@experimentData@other$typeOfData
        name <- paste0("Filtered", ".", rv$typeOfDataset)
        rv.filter$dataIn <- saveParameters(rv.filter$dataIn,name,"Filtering",l.params)
        
        dataOut<- rv.filter$dataIn
        rvModProcess$moduleFilteringDone[5] <- TRUE
        
        if (rv$typeOfDataset == "peptide"  && !is.null(rv$proteinId)){
          ComputeAdjacencyMatrices()
          ComputeConnexComposants()
        }
        UpdateDatasetWidget(rv.filter$dataIn, name)
      }
      rv.filter$dataOut <- rv.filter$dataIn
      rvModProcess$moduleFilteringDone[5] <- TRUE
    })
    
  })
  
  
  return({reactive(rv.filter$dataOut)})
  
}
    
## To be copied in the UI
# mod_pipe_prot_filter_ui("pipe_prot_filter_ui_1")
    
## To be copied in the server
# callModule(mod_pipe_prot_filter_server, "pipe_prot_filter_ui_1")
 
