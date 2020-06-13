########################## gerer les rv$widgets$machins
########################## attention aux renderDataTable..., remplacer par mod_format_DT

mod_filtering_protein_ui <- function(id){
  ns <- NS(id)
  
  fluidPage(
    tagList(
      br(),br(),
      mod_navigation_ui(ns('nav'))
    )
  )
}




# obj est un msnset de type protein
mod_filtering_protein_server <- function(input, output, session, obj) {
  ns <- session$ns  
  
  r.nav <- reactiveValues(
    name = "test",
    stepsNames = c("MV filtering", "String based filtering","Numerical filtering","Summary","Validate"),
    ll.UI = list( screenStep1 = uiOutput(ns("screen1")),
                  screenStep2 = uiOutput(ns("screen2")),
                  screenStep3 = uiOutput(ns("screen3")),
                  screenStep4 = uiOutput(ns("screen4")),
                  screenStep5 = uiOutput(ns("screen5"))
                  ),
    isDone =  c(TRUE,FALSE, FALSE, FALSE, FALSE),
    mandatory =  c(FALSE,FALSE, FALSE, FALSE, FALSE),
    reset = FALSE
  )
  
  # default values for the widgets
  r.widgets <- reactiveValues(
    ChooseFilters = "None",
    seuilNA = 0
    )
  
  rv <- reactiveValues(
    deleted.mvLines = NULL
  )
  
  
  callModule(mod_navigation_server, "nav",style=2, pages = r.nav)
  
 
  observeEvent(req(r.nav$reset),{
     r.nav$isDone <- c(TRUE,FALSE, FALSE, FALSE, FALSE) #rep(FALSE, 5)
    
    r.nav$reset <- FALSE
    
    r.widgets$ChooseFilters <- NULL
    r.widgets$seuilNA <- 0
    
    rv$deleted.mvLines <- NULL
    
    
  })
  
  observeEvent(input$done1,{r.nav$isDone[1] <- TRUE})
  observeEvent(input$done2,{r.nav$isDone[2] <- TRUE})
  observeEvent(input$done3,{r.nav$isDone[3] <- TRUE})
  observeEvent(input$done2,{r.nav$isDone[4] <- TRUE})
  observeEvent(input$done3,{r.nav$isDone[5] <- TRUE})
  
  observeEvent(input$ChooseFilters,ignoreInit=TRUE, {r.widgets$ChooseFilters <- input$ChooseFilters})
  observeEvent(input$seuilNA,ignoreInit=TRUE, {r.widgets$seuilNA <- input$seuilNA})
  observeEvent(input$deleted.mvLines, ignoreInit=TRUE, {rv$deleted.mvLines <- input$deleted.mvLines})
  
  #Global variables
  def.progress.saveFiltering <- c("Build Parameters list", "Save Parameters list", "Save new dataset")
  gFiltersList <- c("None" = "None",
                    "Empty lines" = "EmptyLines",
                    "Whole matrix" = "wholeMatrix",
                    "For every condition" = "allCond",
                    "At least one condition" = "atLeastOneCond")
  gFilterNone <- gFiltersList[["None"]]
  gFilterEmptyLines <- gFiltersList[["Empty lines"]]
  gFilterWholeMat <- gFiltersList[["Whole matrix"]]
  gFilterAllCond <- gFiltersList[["For every condition"]]
  gFilterOneCond <- gFiltersList[["At least one condition"]]
  gFilterTextPrefix <- "Filtered with"
  
  GetFilterText <- function(type, seuil){
    return (
      paste(gFilterTextPrefix," ",type , " (threshold = ", seuil, " ).", sep=""))
  }
  
  
  Get_symFilter_cname_choice <- reactive({
    choice <- c("None", colnames(Biobase::fData(obj)))
    choice
  })
  
  
  
  ###################################################
  ## Screen 1 ##
  ###################################################
  
  output$screen1 <- renderUI({
    tagList(
        div(
          div(style="display:inline-block; vertical-align: middle; padding-right: 40px;",
              selectInput(ns("ChooseFilters"),
                          label="Type",
                          choices = gFiltersList,
                          selected = r.widgets$ChooseFilters,
                          width='200px')),
          div( style="display:inline-block; vertical-align: middle;  padding-right: 40px;",
               uiOutput(ns("seuilNADelete"))),
          div( style="display:inline-block; vertical-align: middle;",
               actionButton(ns("perform.filtering.MV"),
                            label="Perform MV filtering",
                            class = actionBtnClass)
          )),
        hr(),
        #missingValuesPlotsUI("MVPlots_filtering"),
        uiOutput(ns("ObserverMVFilteringDone"))
      )
  })
  
  
  
  output$seuilNADelete <- renderUI({
    req(r.widgets$ChooseFilters)

    if ((r.widgets$ChooseFilters=="None") || (r.widgets$ChooseFilters==gFilterEmptyLines)) {return(NULL)   }
    
    choix <- getListNbValuesInLines(obj, type=r.widgets$ChooseFilters)
    
   
    tagList(
      modulePopoverUI(ns("modulePopover_keepVal")),
      selectInput(ns("seuilNA"), NULL,
                  choices = choix,
                  selected = r.widgets$seuilNA,
                  width='150px'))
  })
  
  callModule(modulePopover,"modulePopover_keepVal",
             data = reactive(list(title=tags$b("Keep vals"),
                                  content= "The user-defined threshold allows to tune the minimum amount of non-NA values for each line to be kept in the dataset (the line is filtered out otherwise). The threshold either applies on the whole dataset, on each condition or on at least one condition.")))
  
  
  
  # callModule(missingValuesPlots,"MVPlots_filtering",
  #            obj=reactive({obj}),
  #            palette = reactive({rv$PlotParams$paletteConditions})
  # )
  
  
  output$ObserverMVFilteringDone <- renderUI({
    req(rv$deleted.mvLines)

    n <- 0
    if(!is.null(rv$deleted.mvLines)){n <- nrow(rv$deleted.mvLines)}
    if (!r.nav$isDone[1])
    {return(NULL)  }
    else {
      h5(paste0("Missing values filtering done. ",n, " lines were deleted."))
    }
  })
  
  
  
  
  ###################################################
  ## Screen 2 ##
  ###################################################


  output$screen2 <- renderUI({
    tagList(
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
  


  output$FilterSummaryData <- DT::renderDataTable(server=TRUE,{
    req(obj)
    req(input$DT_numfilterSummary)
    isolate({
      if (nrow(input$DT_filterSummary )==0){
        df <- data.frame(Filter="-", Prefix="-", nbDeleted=0, Total=nrow(obj), stringsAsFactors = FALSE)
        #input$DT_filterSummary <- rbind(input$DT_numfilterSummary ,df)
        input$DT_filterSummary <- df
      }

      DT::datatable(input$DT_filterSummary,
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



  # ###################################################
  # ## Screen 3 ##
  # ###################################################
  # 
  # output$screenFiltering3 <- renderUI({
  #   req(obj)
  #   
  #   ll <- lapply(fData(obj), function(x){is.numeric(x)})
  #   print("ll")
  #   print(ll)
  #   print("colnames(fData(obj))")
  #   print(head(colnames(fData(obj))))
  #   choice <- c("None", colnames(fData(obj))[which(ll == TRUE)])
  #   
  #   tagList(
  #     tags$div(
  #       tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
  #                 selectInput("numericFilter_cname", "Column name", choices = choice)
  #       ),
  #       
  #       tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
  #                 selectInput("numericFilter_operator", "Operator", 
  #                             choices = c('None' = '',
  #                                         '==' = '==',
  #                                         '<=' = '<=',
  #                                         '<' = '<',
  #                                         '>=' = '>=',
  #                                         '>' = '>',
  #                                         '!=' = '!='), width='100px')
  #       ),
  #       tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
  #                 numericInput("numericFilter_value", "Value", value = "", width='100px')
  #       ),
  #       tags$div( style="display:inline-block; vertical-align: middle;",
  #                 p(""),actionButton("btn_numFilter", "Perform", class = actionBtnClass)
  #       )
  #     ),
  #     tags$hr(),
  #     tags$div(
  #       tags$div( style="display:inline-block; vertical-align: middle; align: center;",
  #                 DT::dataTableOutput("numericalFilterSummaryData")
  #       )
  #     )
  #     
  #   )
  # })
  # 
  # 
  # output$numericalFilterSummaryData <- DT::renderDataTable(server=TRUE,{
  #   req(obj)
  #   req(input$DT_numfilterSummary)
  #   isolate({
  #     if (nrow(input$DT_numfilterSummary) == 0){
  #       df <- data.frame(Filter=NA, Condition=NA, nbDeleted=NA, Total=nrow(obj), stringsAsFactors = FALSE)
  #       input$DT_numfilterSummary <- rbind(input$DT_numfilterSummary ,df)
  #     }
  #     
  #     DT::datatable(input$DT_numfilterSummary,
  #                   extensions = c('Scroller', 'Buttons'),
  #                   rownames = FALSE,
  #                   options=list(initComplete = initComplete(),
  #                                buttons = list('copy',
  #                                               list(
  #                                                 extend = 'csv',
  #                                                 filename = 'NumericalFiltering_summary'
  #                                               ),'print'),
  #                                dom='Brt',
  #                                deferRender = TRUE,
  #                                bLengthChange = FALSE
  #                   ))
  #     })
  # })
  # 
  # 
  # 
  # ###################################################
  # ## Screen 4 ##
  # ###################################################
  # 
  # output$screenFiltering4 <- renderUI({
  #   tagList(
  #     fluidRow(
  #       column(width=3,radioButtons("ChooseTabAfterFiltering",  "Choose the obj to display",
  #                                   choices= list("Quantitative obj" = "quantiData", "Meta obj" = "metaData"),selected=character(0))),
  #       column(width=3,radioButtons("ChooseViewAfterFiltering", "Type of filtered obj", 
  #                                   choices= list("Deleted on missing values" = "MissingValues",
  #                                                 "Deleted string based" = "StringBased",
  #                                                 "Deleted numeric filter" = "Numerical"),
  #                                   selected=character(0))),
  #       column(width=3,uiOutput("legendForExprsData2"))
  #     ),
  #     hr(),
  #     uiOutput("helpTextMV"),
  #     uiOutput("Warning_VizualizeFilteredData"),
  #     DT::dataTableOutput("VizualizeFilteredData")
  #   )
  # })
  # 
  # output$legendForExprsData2 <- renderUI({
  #   req(input$ChooseTabAfterFiltering)
  #   
  #   if (input$ChooseTabAfterFiltering != "quantiData"){return(NULL)}
  #   moduleLegendColoredExprsUI("FilterColorLegend_DS", rv$colorsTypeMV)
  # })
  # 
  # 
  # output$helpTextMV <- renderUI({
  #   helpText("After checking the obj, validate the filters.")
  # })
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
  # output$Warning_VizualizeFilteredData <- renderUI({
  #   if (length(GetDataFor_VizualizeFilteredData())==0){return(NULL)}
  #   if (nrow(GetDataFor_VizualizeFilteredData())>153) 
  #     p(MSG_WARNING_SIZE_DT)
  #   
  # })
  # 
  # 
  # 
  # ###################################################
  # ## Screen 5 ##
  # ###################################################
  # output$screenFiltering5 <- renderUI({     
  #   
  #   tagList(
  #     actionButton("ValidateFilters","Save filtered dataset",class = actionBtnClass)
  #   )
  # })
  
  
  ###################################################
  ## Input effects ##
  ###################################################
  
  # Screen 1
  
  observeEvent(input$perform.filtering.MV,ignoreInit=TRUE,{
    r.widgets$ChooseFilters
    r.widgets$seuilNA
    
    if (r.widgets$ChooseFilters != gFilterNone){
      
      keepThat <- mvFilterGetIndices(obj,
                                     r.widgets$ChooseFilters,
                                     as.integer(r.widgets$seuilNA))
      if (!is.null(keepThat)) {
        rv$deleted.mvLines <- obj[-keepThat]
        obj <- mvFilterFromIndices(obj, 
                                   keepThat,
                                   GetFilterText(r.widgets$ChooseFilters, as.integer(r.widgets$seuilNA)))
        r.nav$isDone[1] <- TRUE
        
      }}
    
  })
  
  
  
  
  
  # Screen 2
  observeEvent(input$actionButtonFilter,{
    req(input$symFilter_cname)
    temp <- obj

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
    obj <- res[["obj"]]
    r.nav$isDone[2] <- TRUE

    df <- data.frame(Filter=cname, Prefix=tagName, nbDeleted=nbDeleted, Total=nrow(obj))
    input$DT_filterSummary <- rbind(input$DT_filterSummary , df)
  })



  observeEvent(input$actionButtonFilter,{
    req(input$symFilter_cname)
    temp <- obj

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
    obj <- res[["obj"]]
    r.nav$isDone[2] <- TRUE

    df <- data.frame(Filter=cname, Prefix=tagName, nbDeleted=nbDeleted, Total=nrow(obj))
    input$DT_filterSummary <- rbind(input$DT_filterSummary , df)
  })



  observeEvent(input$actionButtonFilter,{
    req(input$symFilter_cname)
    temp <- obj

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
    obj <- res[["obj"]]
    r.nav$isDone[2] <- TRUE

    df <- data.frame(Filter=cname, Prefix=tagName, nbDeleted=nbDeleted, Total=nrow(obj))
    input$DT_filterSummary <- rbind(input$DT_filterSummary , df)
  })




  # # Screen 3
  # observeEvent(input$btn_numFilter,ignoreInit=TRUE,{
  #   temp <- obj
  #   
  #   if (input$numericFilter_cname=="None"){return()}
  #   cname <- input$numericFilter_cname
  #   tagValue <- input$numericFilter_value
  #   
  #   print(input$numericFilter_value)
  #   print(input$numericFilter_operator)
  #   res <- NumericalFiltering(temp,cname, input$numericFilter_value,input$numericFilter_operator)
  #   nbDeleted <- 0
  #   
  #   
  #   if (!is.null(res[["deleted"]])){
  #     rv$deleted.numeric <- rbindMSnset(rv$deleted.numeric, res[["deleted"]])
  #     nbDeleted <-  nrow(res[["deleted"]])
  #   } else {
  #     nbDeleted <-  0
  #   }                          
  #   obj <- res[["obj"]]
  #   rvModProcess$moduleFilteringDone[3] <- TRUE
  #   
  #   df <- data.frame(Filter=cname, 
  #                    Condition=paste0(input$numericFilter_operator,' ',tagValue), 
  #                    nbDeleted=nbDeleted, 
  #                    Total=nrow(obj))
  #   input$DT_numfilterSummary <- rbind(input$DT_numfilterSummary, df)
  # })
  # 
  # 
  # # Screen 4
  # output$legendForExprsData2 <- renderUI({
  #   req(input$ChooseTabAfterFiltering)
  #   
  #   if (input$ChooseTabAfterFiltering != "quantiData"){return(NULL)}
  #   moduleLegendColoredExprsUI("FilterColorLegend_DS", rv$colorsTypeMV)
  #   
  # })
  # 
  # 
  # GetDataFor_VizualizeFilteredData <- reactive({
  #   req(rv$settings_nDigits)
  #   rv$deleted.mvLines
  #   req(input$ChooseViewAfterFiltering)
  #   req(input$ChooseTabAfterFiltering)
  #   rv$deleted.stringBased
  #   rv$deleted.numeric
  #   #print("DANS REACTIVE : GetDataFor_VizualizeFilteredData")
  #   
  #   obj <- NULL
  #   if ((input$ChooseViewAfterFiltering == "MissingValues") && !is.null(rv$deleted.mvLines))
  #   {
  #     #print("DANS REACTIVE : If 1")
  #     #print(dim(getDataForMVFiltered()))
  #     switch(input$ChooseTabAfterFiltering,
  #            quantiData =  obj <- getDataForMVFiltered(),
  #            metaData = obj <- cbind(ID = rownames(Biobase::fData(rv$deleted.mvLines)), Biobase::fData(rv$deleted.mvLines))
  #     )
  #   } 
  #   
  #   else if ((input$ChooseViewAfterFiltering == "StringBased") && !is.null(rv$deleted.stringBased)) {
  #     
  #     #print("DANS REACTIVE : If 2")
  #     switch(input$ChooseTabAfterFiltering,
  #            quantiData =  obj <- getDataForMVStringFiltered(),
  #            metaData = obj <- Biobase::fData(rv$deleted.stringBased)
  #     )
  #   }  else if ((input$ChooseViewAfterFiltering == "Numerical") && !is.null(rv$deleted.numeric)) {
  #     #print("DANS REACTIVE : If 3")
  #     switch(input$ChooseTabAfterFiltering,
  #            quantiData =  obj <- getDataForNumericalFiltered(),
  #            metaData = obj <- Biobase::fData(rv$deleted.numeric)
  #     )
  #   }
  #   
  #   # print("END OF REACTIVE")
  #   #print(obj)
  #   obj
  # })
  # 
  # 
  # output$VizualizeFilteredData <- DT::renderDataTable(server=TRUE,{
  #   input$ChooseTabAfterFiltering
  #   req(GetDataFor_VizualizeFilteredData())
  #   dt <- NULL
  #   obj <- GetDataFor_VizualizeFilteredData()
  #   
  #   if(input$ChooseTabAfterFiltering =="quantiData"){
  #     dt <- DT::datatable( obj,
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
  #                            columnDefs = list(list(targets = c(((ncol(obj)/2)+1):ncol(obj)), visible = FALSE),
  #                                              list(width='150px',targets= "_all"))
  #                          )
  #     ) %>%
  #       formatStyle(
  #         colnames(obj)[1:(ncol(obj)/2)],
  #         colnames(obj)[((ncol(obj)/2)+1):ncol(obj)],
  #         backgroundColor = styleEqual(c("POV", "MEC"), c(rv$colorsTypeMV$POV, rv$colorsTypeMV$MEC))
  #       )
  #   } else {
  #     dt <- DT::datatable( obj,
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
  # })
  # 
  # 
  # 
  # 
  # # Screen 5
  # observeEvent(input$ValidateFilters,ignoreInit = TRUE,{
  #   
  #   isolate({
  #     if((input$ChooseFilters != gFilterNone) 
  #        || (nrow(input$DT_filterSummary )>1)
  #        || (nrow(input$DT_numfilterSummary )>1)){
  #       l.params <- build_ParamsList_Filtering()
  #       
  #       rv$typeOfDataset <- obj@experimentData@other$typeOfData
  #       name <- paste0("Filtered", ".", rv$typeOfDataset)
  #       rv$current.obj <- saveParameters(rv$current.obj,name,"Filtering",l.params)
  #       dataOut<- rv$current.obj
  #       rvModProcess$moduleFilteringDone[5] <- TRUE
  #       
  #       if (rv$typeOfDataset == "peptide"  && !is.null(rv$proteinId)){
  #         ComputeAdjacencyMatrices()
  #         ComputeConnexComposants()
  #       }
  #       UpdateDatasetWidget(rv$current.obj, name)
  #     }
  #     rvModProcess$moduleFilteringDone[5] <- TRUE
  #   })
  #   
  # })
  
  
  
}

## To be copied in the UI
# mod_filtering_protein_ui("filtering_protein_ui_1")

## To be copied in the server
# callModule(mod_filtering_protein_server, "filtering_protein_ui_1")
