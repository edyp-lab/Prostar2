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
#' @importFrom shinyalert useShinyalert
#' 
mod_pipe_protein_Filtering_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    shinyalert::useShinyalert(),
    div(id=ns('div_nav_pipe_process'), mod_navigation_ui(ns('nav_pipe_process')))
    )
}
    
#' pipe_prot_filter Server Function
#'
#' @noRd 
#' 
#' @import DAPAR2
#' @import QFeatures
#' @importFrom shinyalert shinyalert
#' 
mod_pipe_protein_Filtering_server <- function(input, output, session, obj, indice){
  ns <- session$ns
 
  ## Section navigation module
  # Variable to manage the different screens of the module
  r.nav <- reactiveValues(
    name = "Filtering",
    stepsNames = c("MV filtering", "Field filtering", "Validate"),
    ll.UI = list( screenStep1 = uiOutput(ns("Screen_Filtering_1")),
                  screenStep2 = uiOutput(ns("Screen_Filtering_2")),
                  screenStep3 = uiOutput(ns("Screen_Filtering_3"))
                  ),
    isDone =  rep(FALSE,3),
    mandatory =  rep(FALSE,3),
    reset = FALSE
  )
  
  ## reactive values for variables in the module
  rv.filter <- reactiveValues(
    name = "processProtFilter",
    dataIn = NULL,
    dataOut = NULL,
    i = NULL,
    settings = NULL,
    tmp = NULL,

    deleted.mvLines = NULL,
    widgets = list(ChooseFilters = "None",
                   seuilNA = 0,
                   DT_naFilterSummary = data.frame(Filter=NULL, 
                                                 seuilNA=NULL,
                                                 nbDeleted=NULL, 
                                                 Total=NULL, 
                                                 stringsAsFactors=F),
                   DT_fieldFilterSummary = data.frame(Filter=NULL, 
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
  
  observeEvent(req(rv.filter$dataIn, rv.filter$i ), {
#browser()
    a <- (length(rv.filter$dataIn) != rv.filter$i) && !r.nav$isDone[length(r.nav$isDone)]
    if (!a) return(NULL)

    shinyalert::shinyalert(
      title = 'title',
      text = "This is a modal",
      size = "xs", 
      closeOnEsc = TRUE,
      closeOnClickOutside = FALSE,
      html = FALSE,
      type = "info",
      showConfirmButton = TRUE,
      showCancelButton = TRUE,
      confirmButtonText = "OK",
      confirmButtonCol = "#15A4E6",
      cancelButtonText = "Cancel",
      timer = 0,
      imageUrl = "",
      animation = FALSE
    )
  })
  
  
  
  observeEvent(req(r.nav$reset),{
    
    rv.filter$widgets <- list(ChooseFilters = "None",
                               seuilNA = 0,
                               DT_naFilterSummary = data.frame(Filter=NULL, 
                                                               seuilNA=NULL,
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
    rv.filter$i <- indice()
    #length(experiments(rv.filter$dataIn)) <- ind()
    rv.filter$tmp <- NULL
    rv.filter.deleted.mvLines <- NULL
    rv.filter.deleted.field <- NULL
    rv.filter$data <- data.frame()

    r.nav$isDone <- rep(FALSE, 3)
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
               if(rv.filter$i>0)
                      rv.filter$dataIn[[rv.filter$i]]
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
    req(obj(), indice())

    rv.filter$dataIn <- obj()
    rv.filter$i <- indice()
    

    if (metadata(obj()[[indice()]])$typeOfData != 'protein'){
      stop("The type of data contained in the dataset is not 'protein'")
      return(NULL)
    }
    
  })
  
  
  # If the user accepts the conditions on the shinyalert, then the process module is activated
  observe({
    input$shinyalert
    rv.filter$i
    if(is.null(input$shinyalert))return(NULL)
    
    c1 <- input$shinyalert
    c2 <- rv.filter$i == length(rv.filter$dataIn)
    c3 <- r.nav$isDone[length(r.nav$isDone)]
    if (c1 && !c2 && !c3){
      #Delete all assays after that one indicated by the indice given in parameter
      rv.filter$dataIn <- rv.filter$dataIn[ , , -((rv.filter$i+1):length(rv.filter$dataIn))]
      c1 <- input$shinyalert
      c2 <- rv.filter$i == length(rv.filter$dataIn)
      c3 <- r.nav$isDone[length(r.nav$isDone)]
    } else {
      # Do nothing, the module interface is still disabled
    }
    shinyjs::toggleState('div_nav_pipe_process', condition = !c3 && (c1||c2))
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
          tags$div( style="display:inline-block; vertical-align: middle;",
                    actionButton(ns("btn_remove_naFilter"), "Remove last filter", class = actionBtnClass)
          ),
          hr(),
          tags$hr(),
          tags$div(
            tags$div( style="display:inline-block; vertical-align: middle; align: center;",
                      mod_format_DT_ui(ns('DT_naFilterSummary'))
            )
          ),
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
    
    
    isolate({
      if (rv.filter$widgets$ChooseFilters == gFilterNone){
      #rv.filter$dataIn <- rv$dataset[[input$datasets]]
    } else {
      
      # create a duplicate of the last assay
      #browser()
      rv.filter$dataIn <- DAPAR2::MVrowsTagToOne(object = rv.filter$dataIn, 
                                         type = rv.filter$widgets$ChooseFilters, 
                                         th = as.integer(rv.filter$widgets$seuilNA), 
                                         percent = FALSE)
      
      ## keep rows where tagNA==0
      na_filter <- QFeatures::VariableFilter(field = "tagNA", value = "0", condition = "==")
      
      rv.filter$dataIn <- DAPAR2::filterFeaturesSam(object = rv.filter$dataIn, 
                                                    i = rv.filter$i, 
                                                    name = paste0('na_filter_',rv.filter$i), 
                                                    filter=na_filter)
      rv.filter$i <- rv.filter$i +1
      rv.filter$dataIn <- DAPAR2::removeAdditionalCol(rv.filter$dataIn, "tagNA")
      key <- metadata(rv.filter$dataIn)$keyId
      
      # rv.filter$dataIn <- addAssayLink(rv.filter$dataIn, from = from, to = to, varFrom = key, varTo = key)
       
      ## keep track of deleted rows
      rv.filter$deleted.mvLines <- setdiff(SummarizedExperiment::rowData(rv.filter$dataIn[[rv.filter$i - 1]])[,metadata(rv.filter$dataIn)$keyId], 
                                           SummarizedExperiment::rowData(rv.filter$dataIn[[rv.filter$i]])[,metadata(rv.filter$dataIn)$keyId])

      #browser()
      # ind <- grep('_filtered_', names(rv.filter$dataIn))
      # if (length(ind) >= 2)
      #   metadata(rv.filter$dataIn[[i]])$Params <- metadata(rv.filter$dataIn[[ind[length(ind)-1]]])$Params
      # 
      if (rv.filter$widgets$ChooseFilters == 'EmptyLines')
        txt <- paste0('type=',rv.filter$widgets$ChooseFilters,
                      ' percent=', rv.filter$widgets$percent)
      else 
        txt <- paste0('type=',rv.filter$widgets$ChooseFilters,
                      ' th=',rv.filter$widgets$seuilNA, 
                      ' percent=', rv.filter$widgets$percent)
      
      metadata(rv.filter$dataIn[[rv.filter$i]])$Params[[paste0('na_filter_',rv.filter$i)]] <- txt
      
      .total <- ifelse(rv.filter$i > 0, nrow(rv.filter$dataIn[[rv.filter$i]]), 0)
      df <- data.frame(Filter = rv.filter$widgets$ChooseFilters, 
                       seuilNA = as.integer(rv.filter$widgets$seuilNA), 
                       nbDeleted = length(rv.filter$deleted.mvLines), 
                       Total = .total)
      
      rv.filter$widgets$DT_naFilterSummary <- rbind(rv.filter$widgets$DT_naFilterSummary, df)
      
      
    
      r.nav$isDone[1] <- TRUE
     # }
    }
    #updateSelectInput(session, "ChooseFilters", selected = input$ChooseFilters)
    #updateSelectInput(session, "seuilNA", selected = input$seuilNA)
      
    })
  })
  
  # observe({
  #   rv.filter$dataIn
  #   shinyjs::toggle('btn_remove_naFilter', condition=length(grep('na_filter_', names(rv.filter$dataIn))))
  # })
  
  observeEvent(input$btn_remove_naFilter, ignoreInit = TRUE,{
    rv.filter$dataIn
    #browser()
    ind <- grep('na_filter_', names(rv.filter$dataIn))
    if (length(ind) >= 1){
      rv.filter$dataIn <- rv.filter$dataIn[ ,  , -ind[length(ind)]]
      rv.filter$widgets$DT_naFilterSummary <- rv.filter$widgets$DT_naFilterSummary[-nrow(rv.filter$widgets$DT_naFilterSummary),]
    }
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
  
  callModule(mod_format_DT_server,'DT_naFilterSummary', 
             table2show = reactive({rv.filter$widgets$DT_naFilterSummary}),
             style = reactive({NULL})) 
  
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
    if (rv.filter$i == 0){ return(NULL)}
    
    choice_field <- c("None",colnames(SummarizedExperiment::rowData(rv.filter$dataIn[[rv.filter$i]])))

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
        
        # tags$div( style="display:inline-block; vertical-align: middle;",
        #           actionButton(ns("btn_test_fieldFilter"), "Test", class = actionBtnClass)
        #           ),
        tags$div( style="display:inline-block; vertical-align: middle;",
                  actionButton(ns("btn_perform_fieldFilter"), "Perform", class = actionBtnClass)
        ),
        tags$div( style="display:inline-block; vertical-align: middle;",
                  actionButton(ns("btn_remove_fieldFilter"), "Remove last filter", class = actionBtnClass)
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
    if (rv.filter$i == 0){ return(NULL)}
    
    
      numeric_operators <- c('==' = '==',
                           '<=' = '<=',
                           '<' = '<',
                           '>=' = '>=',
                           '>' = '>',
                           '!=' = '!=')
    
    character_operators <- c( '==' = '==',
                             '!=' = '!=')
     
   vec <- SummarizedExperiment::rowData(rv.filter$dataIn[[rv.filter$i]])[,rv.filter$widgets$fieldName]
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
  
  

  
  output$preview_msg <- renderUI({
    req(rv.filter$tmp)
    rv.filter$deleted.field
   # browser()
    if (length(experiments(rv.filter$tmp))==0)
      h4('Info: With these settings, your dataset will loose all its data. We advice to change the parameters.')
    else
      h4('Info: A total of ', length(rv.filter$deleted.field), 'lines will be deleted from the last assay of your dataset.')
    
  })
  
  
  
  observeEvent(input$btn_remove_fieldFilter, ignoreInit = TRUE,{
    rv.filter$dataIn
   
      ind <- grep('field_filter_', names(rv.filter$dataIn))
      if (length(ind) >= 1){
        rv.filter$dataIn <- rv.filter$dataIn[ ,  , -ind[length(ind)]]
        rv.filter$widgets$DT_fieldfilterSummary <- rv.filter$widgets$DT_fieldfilterSummary[-nrow(rv.filter$widgets$DT_fieldfilterSummary),]
      }
    })

  
  observeEvent(input$btn_perform_fieldFilter,ignoreInit=TRUE,{
    req(rv.filter$widgets$fieldName)
    if (rv.filter$widgets$fieldName == 'None'){return(NULL)}
    req(SummarizedExperiment::rowData(rv.filter$dataIn[[rv.filter$i]])[ , rv.filter$widgets$fieldName])
    
   # browser()
    
    i <- rv.filter$i
    if (rv.filter$widgets$fieldFilter_value %in% c('', 'None')){return(NULL)}
    vec <- SummarizedExperiment::rowData(rv.filter$dataIn[[rv.filter$i]])[,rv.filter$widgets$fieldName]
    if(is.numeric(vec)) 
      if(is.na(as.numeric(rv.filter$widgets$fieldFilter_value)))
        return(NULL)
    
    fieldname <- rv.filter$widgets$fieldName
    tagValue <- rv.filter$widgets$fieldFilter_value
    
    field_filter <- NULL
    if (is.numeric(vec)){
      field_filter <- QFeatures::VariableFilter(field = rv.filter$widgets$fieldName, 
                                     value = as.numeric(rv.filter$widgets$fieldFilter_value), 
                                     condition = rv.filter$widgets$operator)
    } else { 
      if (is.character(vec))
        field_filter <- QFeatures::VariableFilter(field = rv.filter$widgets$fieldName, 
                                       value = as.character(rv.filter$widgets$fieldFilter_value), 
                                       condition = rv.filter$widgets$operator)
    }
    tryCatch({
      rv.filter$dataIn <- DAPAR2::filterFeaturesSam(object = rv.filter$dataIn, 
                                               i = rv.filter$i, 
                                               filter = field_filter, 
                                               name=paste0('field_filter_', rv.filter$i)
                                                )
    }
    , warning = function(w) {
       shinyjs::info(paste("Warning in CreateMSnSet",":",
                           conditionMessage(w),
                           sep=" "))
     
    }, error = function(e) {
      print(e)
      # shinyjs::info(paste("Error :","CreateMSnSet",":",
      #                     conditionMessage(e),
      #                     sep=" "))
    }, finally = {
      #cleanup-code
      
      if (i < rv.filter$i) {
        i <- rv.filter$i
        metadata(rv.filter$dataIn[[i]])$Params[[paste0('field_filter_',i)]] <- paste0('field=', rv.filter$widgets$fieldName,
                                                                                      ' operator=',rv.filter$widgets$operator, 
                                                                                      ' value=', rv.filter$widgets$fieldFilter_value)
        df <- data.frame(Filter = rv.filter$widgets$fieldName, 
                       Condition = paste0(rv.filter$widgets$operator,' ',rv.filter$widgets$fieldFilter_value), 
                       nbDeleted = nrow(rv.filter$dataIn[[i-1]]) -nrow(rv.filter$dataIn[[i]]) , 
                       Total = nrow(rv.filter$dataIn[[i]]))
 
        rv.filter$widgets$DT_fieldfilterSummary <- rbind(rv.filter$widgets$DT_fieldfilterSummary, df)
      }
      
    r.nav$isDone[2] <- TRUE
    
  })
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
    
    tagList(
      actionButton(ns("ValidateFilters"),"Save filtered dataset",class = actionBtnClass)
    )
  })
  
  
  observeEvent(input$ValidateFilters,ignoreInit = TRUE,{ 
    
      #get indices of temporary SE
      ind <- grep('_filter_', names(rv.filter$dataIn))
      if (length(ind)>1){
        ind <- ind[-length(ind)]
        rv.filter$dataIn <- rv.filter$dataIn[ , ,-ind]
      }
      ind <- grep('_filter_', names(rv.filter$dataIn))
      names(rv.filter$dataIn)[ind] <- 'filtered'
      rv.filter$i <- length(rv.filter$dataIn)
      rv.filter$dataOut <- rv.filter$dataIn

      r.nav$isDone[3] <- TRUE
  })
  
  
  return({reactive(rv.filter$dataOut)})
  
}
    
## To be copied in the UI
# mod_pipe_protein_Filtering_ui("pipe_prot_filter_ui_1")
    
## To be copied in the server
# callModule(mod_pipe_protein_Filtering_server, "pipe_prot_filter_ui_1")
 
