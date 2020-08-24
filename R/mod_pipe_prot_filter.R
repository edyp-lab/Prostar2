#' pipe_prot_filter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @import shinyjs
#' 
mod_pipe_prot_filter_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
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
    ll.UI = list( screenStep1 = uiOutput(ns("Screen_Filtering_1")),
                  screenStep2 = uiOutput(ns("Screen_Filtering_2")),
                  screenStep3 = uiOutput(ns("Screen_Filtering_3")),
                  screenStep4 = uiOutput(ns("Screen_Filtering_4"))
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
    tmp = NULL,
    
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
  
  
  
  
  #global variables for the module
  gFiltersList <- c("None" = "None",
                    "Empty lines" = "EmptyLines",
                    "Whole matrix" = "WholeMatrix",
                    "For every condition" = "AllCond",
                    "At least one condition" = "AtLeastOneCond")
  gFilterNone <- gFiltersList[["None"]]
  gFilterEmptyLines <- gFiltersList[["Empty lines"]]
  gFilterWholeMat <- gFiltersList[["Whole matrix"]]
  gFilterAllCond <- gFiltersList[["For every condition"]]
  gFilterOneCond <- gFiltersList[["At least one condition"]]
  
  
  
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
    rv.filter$tmp <- NULL
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
  rv.filter$settings <- callModule(mod_settings_server, "settings", obj=reactive({rv.filter$dataIn}))
  callModule(mod_plots_group_mv_server,"MVPlots_filtering",
             obj = reactive({req(rv.filter$dataIn)
               if(length(experiments(rv.filter$dataIn))>0)
                      rv.filter$dataIn[[length(experiments(rv.filter$dataIn))]]
                else      NULL
               }),
             conds = reactive({colData(rv.filter$dataIn)}),
             base_palette = reactive({rv.filter$settings()$basePalette})
  )
  
callModule(mod_popover_for_help_server,
             "modulePopover_keepVal", 
             data = list(title=tags$b("Keep vals"),
                                  content= "The user-defined threshold allows to tune the minimum amount of non-NA values for each line to be kept in the dataset (the line is filtered out otherwise). The threshold either applies on the whole dataset, on each condition or on at least one condition."))
  
  observe({
    req(obj())
    rv.filter$dataIn <- obj()
  })
  
  observe({
    req(ind())
    rv.filter$i <- ind()
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
  output$Screen_Filtering_1 <- renderUI({
    
    
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
  
  
  
  ##
  ## Perform missing values filtering
  observeEvent(input$perform.filtering.MV,ignoreInit=TRUE,{
    rv.filter$widgets$ChooseFilters
    rv.filter$widgets$seuilNA
    rv.filter$dataIn
    
    
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
                                  rv.filter$dataIn[[rv.filter$i]],
                                  paste0('na_filtered_', rv.filter$i)
      )
      rv.filter$i <- rv.filter$i + 1
     #browser()
      rv.filter$dataIn <- MVrowsTagToOne(object =rv.filter$dataIn, 
                                         type = rv.filter$widgets$ChooseFilters, 
                                         th = as.integer(rv.filter$widgets$seuilNA), 
                                         percent = FALSE)
      
      ## keep rows where tagNA==0
      na_filter <- VariableFilter(field = "tagNA", value = "0", condition = "==")
      
      rv.filter$dataIn <- filterFeatures(rv.filter$dataIn, na_filter)
      rv.filter$dataIn <- removeAdditionalCol(rv.filter$dataIn, "tagNA")
       key <- metadata(rv.filter$dataIn)$keyId
       from <- rv.filter$i -1
       to <- rv.filter$i
      # rv.filter$dataIn <- addAssayLink(rv.filter$dataIn, from = from, to = to, varFrom = key, varTo = key)
       
      ## keep track of deleted rows
      rv.filter$deleted.mvLines <- setdiff(rowData(rv.filter$dataIn[[from]])[,metadata(rv.filter$dataIn)$keyId], 
                                           rowData(rv.filter$dataIn[[to]])[,metadata(rv.filter$dataIn)$keyId])
      #browser()
      #record parameters of filtering
      n_names <- grep('na_filter_', names(metadata(rv.filter$dataIn[[rv.filter$i]])))
      param_name <- paste0('na_filter_', 1+length(n_names))
      metadata(rv.filter$dataIn[[rv.filter$i]])[[param_name]] <- paste0('type=',rv.filter$widgets$ChooseFilters,' th=',rv.filter$widgets$seuilNA, ' percent=', rv.filter$widgets$percent)
      r.nav$isDone[1] <- TRUE
     # }
    }
    #updateSelectInput(session, "ChooseFilters", selected = input$ChooseFilters)
    #updateSelectInput(session, "seuilNA", selected = input$seuilNA)
  })
  
  output$seuilNADelete <- renderUI({ 
    req(rv.filter$widgets$ChooseFilters)
    
    if ((rv.filter$widgets$ChooseFilters=="None") || (rv.filter$widgets$ChooseFilters==gFilterEmptyLines)) {return(NULL)   }
    choice <- getListNbValuesInLines(obj = rv.filter$dataIn, 
                                     i = rv.filter$i, 
                                     type = rv.filter$widgets$ChooseFilters)
    tagList(
      mod_popover_for_help_ui(ns("modulePopover_keepVal")),
      
      selectInput(ns("seuilNA"), NULL,
                  choices = choice,
                  selected = rv.filter$widgets$seuilNA,
                  width='150px'))
    
  })
  
  
  output$ObserverMVFilteringDone <- renderUI({
    req(rv.filter$deleted.mvLines)
    #isolate({
    
    n <- 0
    if(!is.null(rv.filter$deleted.mvLines))
      n <- nrow(rv.filter$deleted.mvLines)
    
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
  output$Screen_Filtering_2 <- renderUI({
    req(rv.filter$dataIn)
    if (length(experiments(rv.filter$dataIn)) == 0){ return(NULL)}
    
    choice_field <- c("None",colnames(rowData(rv.filter$dataIn[[rv.filter$i]])))

      tagList(
      h4("Build the filter for the data you want to keep"),
      tags$div(
        tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  selectInput(ns("fieldName"), "Column name",
                              choices = choice_field,
                              selected = rv.filter$widgets$fieldName)
        ),

        tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  uiOutput(ns('filterFieldOptions'))
        ),
        
        tags$div( style="display:inline-block; vertical-align: middle;",
                  actionButton(ns("btn_test_fieldFilter"), "Perform", class = actionBtnClass)
                  ),
        tags$div( style="display:inline-block; vertical-align: middle;",
                  shinyjs::disabled(actionButton(ns("btn_perform_fieldFilter"), "Perform", class = actionBtnClass))
        )
      ),
      uiOutput(ns('preview_msg')),
      tags$hr(),
      tags$div(
        tags$div( style="display:inline-block; vertical-align: middle; align: center;",
                  mod_format_DT_ui(ns('DT_fieldfilterSummary'))
        )
      )
    )
   
  })
  
 
  
  
  output$filterFieldOptions <- renderUI({
    req(rv.filter$widgets$fieldName)
    if (rv.filter$widgets$fieldName == "None"){ return(NULL)}
    if (length(experiments(rv.filter$dataIn)) == 0){ return(NULL)}
    
    
      numeric_operators <- c('==' = '==',
                           '<=' = '<=',
                           '<' = '<',
                           '>=' = '>=',
                           '>' = '>',
                           '!=' = '!=')
    
    character_operators <- c( '==' = '==',
                             '!=' = '!=')
     
   vec <- rowData(rv.filter$dataIn[[rv.filter$i]])[,rv.filter$widgets$fieldName]
   if (is.numeric(vec))
      operators <- numeric_operators
    else if (is.character(vec))
      operators <- character_operators
    
   isolate({
    tagList(
      tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
              selectInput(ns("operator"), "Operator", 
                          choices = operators, 
                          selected = rv.filter$widgets$operator,
                          width='100px')
    ),
    tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
              textInput(ns("fieldFilter_value"), 
                        "Value", 
                        value = rv.filter$widgets$fieldFilter_value, 
                        width='100px')
    )
    )
})
  })
  
  
  
  # observeEvent(rv.filter$widgets$fieldFilter_value,{
  #   #req(rv.filter$widgets$fieldFilter_value)
  #   req(rv.filter$widgets$fieldName)
  #   if (rv.filter$widgets$fieldName %in% c('', 'None')) {return(NULL)}
  #   
  #    vec <- rowData(rv.filter$dataIn[[rv.filter$i]])[,rv.filter$widgets$fieldName]
  #    cond <- (is.numeric(vec) && as.numeric(rv.filter$widgets$fieldFilter_value)) || TRUE
  #   #   (is.character(vec) && rv.filter$widgets$fieldFilter_value)
  #   shinyjs::toggleState(id='toto', condition=TRUE)
  # })
  # 
  # 
  
  
  output$preview_msg <- renderUI({
    req(rv.filter$tmp)
    rv.filter$deleted.field
   # browser()
    if (length(experiments(rv.filter$tmp))==0)
      h4('Info: With these settings, your dataset will loose all its data. We advice to change the parameters.')
    else
      h4('Info: A total of ', length(rv.filter$deleted.field), 'lines will be deleted from the last assay of your dataset.')
    
  })
  
  
  ## ----------------------------------------------
  # Perform field filtering
  observeEvent(input$btn_test_fieldFilter,ignoreInit=TRUE,{
    req(rv.filter$widgets$fieldName)
    if (rv.filter$widgets$fieldName == 'None'){return(NULL)}
    req(rv.filter$dataIn)
    req(rowData(rv.filter$dataIn[[rv.filter$i]])[,rv.filter$widgets$fieldName])
    
    
    
    rv.filter$tmp <- rv.filter$dataIn
    i <- rv.filter$i
    if (rv.filter$widgets$fieldFilter_value %in% c('', 'None')){return(NULL)}
    vec <- rowData(rv.filter$tmp[[rv.filter$i]])[,rv.filter$widgets$fieldName]
    if(is.numeric(vec)) 
       if(is.na(as.numeric(rv.filter$widgets$fieldFilter_value)))
         return(NULL)
 
    
    
    fieldname <- rv.filter$widgets$fieldName
    tagValue <- rv.filter$widgets$fieldFilter_value
    
    
    # create a duplicate of the last assay
    rv.filter$tmp <-addAssay(rv.filter$tmp, 
                                rv.filter$tmp[[rv.filter$i]],
                                paste0('field_filtered_', rv.filter$i)
                                )
    i <- i +1
   # browser()
    field_filter <- NULL
    if (is.numeric(vec)){
      field_filter <- VariableFilter(field = rv.filter$widgets$fieldName, 
                                     value = as.numeric(rv.filter$widgets$fieldFilter_value), 
                                     condition = rv.filter$widgets$operator)
    } else { 
      if (is.character(vec))
        field_filter <- VariableFilter(field = rv.filter$widgets$fieldName, 
                                     value = as.character(rv.filter$widgets$fieldFilter_value), 
                                     condition = rv.filter$widgets$operator)
    }
    
    .tmp <- filterFeatures(rv.filter$tmp, field_filter)
    
    if (length(experiments(.tmp)) > 0) {
      rv.filter$tmp[[i]] <- .tmp[[i]]
      rv.filter$deleted.field <- setdiff(rowData(rv.filter$tmp[[i-1]])[,metadata(rv.filter$tmp)$keyId],
                                       rowData(rv.filter$tmp[[i]])[,metadata(rv.filter$tmp)$keyId])
      #browser()
      shinyjs::enable('btn_perform_fieldFilter')
    } else {
      rv.filter$tmp <- .tmp
      rv.filter$deleted.field <- NULL
      return(NULL)
    }
    
    
    
  })
  
  
  observeEvent(input$btn_perform_fieldFilter,ignoreInit=TRUE,{
    req(rv.filter$widgets$fieldName)
    if (rv.filter$widgets$fieldName == 'None'){return(NULL)}
    req(rowData(rv.filter$dataIn[[rv.filter$i]])[,rv.filter$widgets$fieldName])
    

    rv.filter$dataIn <- rv.filter$tmp
    if (length(experiments(rv.filter$dataIn )) > 0)
      rv.filter$i <- length(experiments(rv.filter$dataIn ))
    else 
      rv.filter$i <- NULL
    
    .total <- ifelse(length(experiments(rv.filter$dataIn )) > 0, nrow(rv.filter$dataIn[[rv.filter$i]]), 0)
    df <- data.frame(Filter = rv.filter$widgets$fieldName, 
                     Condition = paste0(rv.filter$widgets$operator,' ',rv.filter$widgets$fieldFilter_value), 
                     nbDeleted = length(rv.filter$deleted.field), 
                     Total = .total)
    
    rv.filter$widgets$DT_fieldfilterSummary <- rbind(rv.filter$widgets$DT_fieldfilterSummary, df)
    n_names <- grep('field_filter_', names(metadata(rv.filter$dataIn[[rv.filter$i]])))
    param_name <- paste0('field_filter_', 1+length(n_names))
    metadata(rv.filter$dataIn[[rv.filter$i]])[[param_name]] <- paste0('type=',rv.filter$widgets$ChooseFilters,' th=',rv.filter$widgets$seuilNA, ' percent=', rv.filter$widgets$percent)
    
    r.nav$isDone[2] <- TRUE
    
  })
  
  callModule(mod_format_DT_server,'DT_fieldfilterSummary', 
             table2show = reactive({rv.filter$widgets$DT_fieldfilterSummary}),
             style = reactive({NULL})) 
 
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
  ###                                 Screen 3                                        ###
  ###---------------------------------------------------------------------------------###
  output$Screen_Filtering_3 <- renderUI({
    
  #   tagList(
  #     fluidRow(
  #       column(width=3,radioButtons(ns("ChooseTabAfterFiltering"),  "Choose the data to display",
  #                                   choices= list("Quantitative data" = "quantiData", "Meta data" = "metaData"),selected=character(0))),
  #       column(width=3,radioButtons(ns("ChooseViewAfterFiltering"), "Type of filtered data", 
  #                                   choices= list("Deleted on missing values" = "MissingValues",
  #                                                 "Deleted field based" = "FieldBased"),
  #                                   selected=character(0))),
  #       column(width=3,uiOutput(ns("legendForExprsData2")))
  #     ),
  #     hr(),
  #     uiOutput(ns("helpTextMV")),
  #     uiOutput(ns("Warning_VizualizeFilteredData")),
  #     DT::dataTableOutput(ns("VizualizeFilteredData"))
  #     
  #   )
  # })
  # 
  # 
  # output$FilterSummaryData <- DT::renderDataTable(server=TRUE,{
  #   req(rv.filter$dataIn)
  #   req(rv.filter$widgets$DT_fieldfilterSummary)
  #   isolate({
  #     
  #     if (nrow(rv.filter$widgets$DT_filterSummary )==0){
  #       df <- data.frame(Filter="-", 
  #                        Prefix="-", 
  #                        nbDeleted=0, 
  #                        Total=nrow(rv.filter$dataIn), 
  #                        stringsAsFactors = FALSE)
  #       rv.filter$widgets$DT_filterSummary <- df
  #     }
  #     
  #     
  #     DT::datatable(rv.filter$widgets$DT_filterSummary,
  #                   extensions = c('Scroller', 'Buttons'),
  #                   rownames = FALSE,
  #                   options=list(buttons = list('copy',
  #                                               list(
  #                                                 extend = 'csv',
  #                                                 filename = 'na_Filtering_summary'
  #                                               ),'print'),
  #                                dom='Brt',
  #                                initComplete = initComplete(),
  #                                deferRender = TRUE,
  #                                bLengthChange = FALSE
  #                   ))
  #   })
  # })
  # 
  # 
  # 
  # getDataForFieldFiltered <- reactive({
  #   req(rv$settings_nDigits)
  #   rv$deleted.field
  #   table <- as.data.frame(round(Biobase::exprs(rv$deleted.field),digits=rv$settings_nDigits))
  #   table <- cbind(table, Biobase::fData(rv$deleted.field)[,rv$deleted.numeric@experimentData@other$OriginOfValues])
  #   
  #   table
  # })
  # 
  # 
  # 
  # getDataForMVFiltered <- reactive({
  #   req(rv$settings_nDigits)
  #   rv$deleted.mvLines
  #   
  #   table <- as.data.frame(round(Biobase::exprs(rv$deleted.mvLines),digits=rv$settings_nDigits))
  #   table <- cbind(table, Biobase::fData(rv$deleted.mvLines)[,rv$deleted.mvLines@experimentData@other$OriginOfValues])
  #   
  #   table
  # })
  # 
  # 
  # 
  # 
  # getDataForMVStringFiltered <- reactive({
  #   req(rv$settings_nDigits)
  #   rv$deleted.stringBased
  #   table <- as.data.frame(round(Biobase::exprs(rv$deleted.stringBased),digits=rv$settings_nDigits))
  #   table <- cbind(table, Biobase::fData(rv$deleted.stringBased)[,rv$deleted.stringBased@experimentData@other$OriginOfValues])
  #   
  #   table
  # })
  # 
  # 
  # output$legendForExprsData2 <- renderUI({
  #   req(input$ChooseTabAfterFiltering)
  #   
  #   if (input$ChooseTabAfterFiltering != "quantiData"){return(NULL)}
  #   moduleLegendColoredExprsUI(ns("FilterColorLegend_DS"), rv$colorsTypeMV)
  #   
  # })
  # 
  # 
  # 
  # output$Warning_VizualizeFilteredData <- renderUI({
  #   if (length(GetDataFor_VizualizeFilteredData())==0){return(NULL)}
  #   if (nrow(GetDataFor_VizualizeFilteredData())>153) 
  #     p(MSG_WARNING_SIZE_DT)
  #   
  # })
  # 
  # 
  # 
  # GetDataFor_VizualizeFilteredData <- reactive({
  #   req(rv$settings_nDigits)
  #   rv$deleted.mvLines
  #   req(input$ChooseViewAfterFiltering)
  #   req(input$ChooseTabAfterFiltering)
  #   rv$deleted.stringBased
  #   rv$deleted.numeric
  #   
  #   
  #   data <- NULL
  #   if ((input$ChooseViewAfterFiltering == "MissingValues") && !is.null(rv$deleted.mvLines))
  #   {
  #      switch(input$ChooseTabAfterFiltering,
  #            quantiData =  data <- getDataForMVFiltered(),
  #            metaData = data <- cbind(ID = rownames(Biobase::fData(rv$deleted.mvLines)), Biobase::fData(rv$deleted.mvLines))
  #     )
  #   } 
  #   
  #   else if ((input$ChooseViewAfterFiltering == "StringBased") && !is.null(rv$deleted.stringBased)) {
  #     
  #     #print("DANS REACTIVE : If 2")
  #     switch(input$ChooseTabAfterFiltering,
  #            quantiData =  data <- getDataForMVStringFiltered(),
  #            metaData = data <- Biobase::fData(rv$deleted.stringBased)
  #     )
  #   }  else if ((input$ChooseViewAfterFiltering == "Numerical") && !is.null(rv$deleted.numeric)) {
  #     #print("DANS REACTIVE : If 3")
  #     switch(input$ChooseTabAfterFiltering,
  #            quantiData =  data <- getDataForNumericalFiltered(),
  #            metaData = data <- Biobase::fData(rv$deleted.numeric)
  #     )
  #   }
  #   
  #   # print("END OF REACTIVE")
  #   #print(data)
  #   data
  # })
  # 
  # 
  # 
  # #----------------------------------------------
  # output$VizualizeFilteredData <- DT::renderDataTable(server=TRUE,{
  #   input$ChooseTabAfterFiltering
  #   req(GetDataFor_VizualizeFilteredData())
  #   dt <- NULL
  #   data <- GetDataFor_VizualizeFilteredData()
  #   
  #   if(input$ChooseTabAfterFiltering =="quantiData"){
  #     dt <- DT::datatable( data,
  #                          extensions = c('Scroller', 'Buttons'),
  #                          options = list(
  #                            buttons = list('copy',
  #                                           list(
  #                                             extend = 'csv',
  #                                             filename = 'Prostar_export'),
  #                                           'print'),
  #                            dom='Brtip',
  #                            initComplete = initComplete(),
  #                            displayLength = 20,
  #                            deferRender = TRUE,
  #                            bLengthChange = FALSE,
  #                            scrollX = 200,
  #                            scrollY = 600,
  #                            scroller = TRUE,
  #                            ordering=FALSE,
  #                            columnDefs = list(list(targets = c(((ncol(data)/2)+1):ncol(data)), visible = FALSE),
  #                                              list(width='150px',targets= "_all"))
  #                          )
  #     ) %>%
  #       formatStyle(
  #         colnames(data)[1:(ncol(data)/2)],
  #         colnames(data)[((ncol(data)/2)+1):ncol(data)],
  #         backgroundColor = styleEqual(c("POV", "MEC"), c(rv$colorsTypeMV$POV, rv$colorsTypeMV$MEC))
  #       )
  #   } else {
  #     dt <- DT::datatable( data,
  #                          extensions = 'Scroller',
  #                          options = list(initComplete = initComplete(),
  #                                         displayLength = 20,
  #                                         deferRender = TRUE,
  #                                         bLengthChange = FALSE,
  #                                         scrollX = 200,
  #                                         scrollY = 600,
  #                                         scroller = TRUE,
  #                                         ordering=FALSE)) 
  #   }
  #   # }
  #   dt
  #   
   })
  
  ###---------------------------------------------------------------------------------###
  ###                                 Screen 4                                        ###
  ###---------------------------------------------------------------------------------###
  output$Screen_Filtering_4 <- renderUI({     
    
    tagList(
      actionButton(ns("ValidateFilters"),"Save filtered dataset",class = actionBtnClass)
    )
  })
  
  
  observeEvent(input$ValidateFilters,ignoreInit = TRUE,{ 
    
    isolate({
      # if((rv.filter$widgets$ChooseFilters != gFilterNone) 
      #    || (nrow(rv.filter$widgets$DT_filterSummary )>1)
      #    || (nrow(rv.filter$widgets$DT_fieldfilterSummary )>1)){
      #   l.params <- build_ParamsList_Filtering()
      #   
      #   rv$typeOfDataset <- rv.filter$dataIn@experimentData@other$typeOfData
      #   name <- paste0("Filtered", ".", rv$typeOfDataset)
      #   rv.filter$dataIn <- saveParameters(rv.filter$dataIn,name,"Filtering",l.params)
      #   metadata(rv.filter$dataIn[[rv.filter$i]])$Params <- list(
      #     method = rv.filter$widgets$method,
      #     type = rv.filter$widgets$type,
      #     varReduction = rv.filter$widgets$varReduction,
      #     quantile = rv.filter$widgets$quantile,
      #     spanLOESS =rv.filter$widgets$spanLOESS
      #   )
        
      rv.filter$dataOut <- rv.filter$dataIn
      if (metadata(rv.filter$dataOut[[length(experiments(rv.filter$dataOut))]])$typeOfData == "peptide"  
            && !is.null(metadata(rv.filter$dataOut)$parentProtId)){
          #ComputeAdjacencyMatrices()
          #ComputeConnexComposants()
        }
        
       # UpdateDatasetWidget(rv.filter$dataIn, name)
      #}
      
      
      r.nav$isDone[4] <- TRUE
    })
    
  })
  
  
  return({reactive(rv.filter$dataOut)})
  
}
    
## To be copied in the UI
# mod_pipe_prot_filter_ui("pipe_prot_filter_ui_1")
    
## To be copied in the server
# callModule(mod_pipe_prot_filter_server, "pipe_prot_filter_ui_1")
 
