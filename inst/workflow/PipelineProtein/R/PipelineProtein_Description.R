#' @title PipelineProtein Description module
#'
#' @description
#' This module contains the description step of the protein pipeline.
#' 
#' @param id A `character(1)` which is the 'id' of the module.
#'
#' @param dataIn An instance of the class `MultiAssayExperiment`
#'
#' @param steps.enabled A vector of boolean which has the same length of the steps
#' of the pipeline. This information is used to enable/disable the widgets. It is not
#' a communication variable between the caller and this module, thus there is no
#' corresponding output variable
#'
#' @param remoteReset It is a remote command to reset the module. An `integer()` that
#' indicates is the pipeline has been reseted by a program of higher level
#' Basically, it is the program which has called this module
#'
#' @param steps.status A vector of `character()` which indicates the status of each step
#' which can be either 'validated', 'undone' or 'skipped'. Enabled or disabled in the UI.
#' 
#' @param current.pos A `integer(1)` which acts as a remote command to make
#'  a step active in the timeline. Default is 1.
#'  
#' @param path A `character()` which is the path to the directory which 
#' contains the files and directories of the pipeline.
#' 
#' @examples
#' if (interactive()){
#'   Prostar2("PipelineProtein_Description")
#' }
#' 
#' @name PipelineProtein_Description
#' 
#' @importFrom QFeatures addAssay removeAssay
#' @import DaparToolshed
#' 
#' @return An instance of the class `MultiAssayExperiment`
#' 
NULL


#' @rdname PipelineProtein_Description
#' @export
#' 
PipelineProtein_Description_conf <- function(){
  MagellanNTK::Config(
    fullname = 'PipelineProtein_Description',
    mode = 'process'
    )
}


#' @rdname PipelineProtein_Description
#' @export
#' 
PipelineProtein_Description_ui <- function(id){
  ns <- NS(id)
}


#' @rdname PipelineProtein_Description
#' @export
#' 
PipelineProtein_Description_server <- function(id,
    dataIn = reactive({NULL}),
    steps.enabled = reactive({NULL}),
    remoteReset = reactive({0}),
    steps.status = reactive({NULL}),
    current.pos = reactive({1}),
    path = NULL,
    btnEvents = reactive({NULL})
){
  
  pkgs_require(c('QFeatures', 'SummarizedExperiment', 'S4Vectors'))
  
  # Define default selected values for widgets
  # By default, this list is empty for the Description module
  # but it can be customized
  widgets.default.values <- NULL
  rv.custom.default.values <- list(
    result_open_dataset = reactive({NULL}),
    history = MagellanNTK::InitializeHistory()
    )
  
  ###-------------------------------------------------------------###
  ###                                                             ###
  ### ------------------- MODULE SERVER --------------------------###
  ###                                                             ###
  ###-------------------------------------------------------------###
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Insert necessary code which is hosted by MagellanNTK
    # DO NOT MODIFY THIS LINE
    core.code <- MagellanNTK::Get_Workflow_Core_Code(
      mode = 'process',
      name = id,
      w.names = names(widgets.default.values),
      rv.custom.names = names(rv.custom.default.values)
    )
    
    eval(str2expression(core.code))
    add_resourcePath()
    
    
    ###########################################################################-
    #
    #-----------------------------DESCRIPTION-----------------------------------
    #
    ###########################################################################-
    output$Description <- renderUI({
      file <- normalizePath(file.path(
        system.file('workflow', package = 'Prostar2'),
        unlist(strsplit(id, '_'))[1], 
        'md', 
        paste0(id, '.Rmd')))
     
      MagellanNTK::process_layout(session,
        ns = NS(id),
        sidebar = tagList(
          uiOutput(ns('open_dataset_UI'))
          ),
        content = tagList(
          if (file.exists(file))
            includeMarkdown(file)
          else
            p('No Description available')
          #uiOutput(ns('Description_infos_dataset_UI'))
        )
      )
    })
    
    #### _sidebar -----
    output$open_dataset_UI <- renderUI({
      req(session$userData$runmode == 'process')
      req(is.null(dataIn()))
      req(NULL)
      rv.custom$result_open_dataset <- MagellanNTK::open_dataset_server(
        id = "open_dataset",
        class = 'QFeatures',
        extension = "qf",
        remoteReset = reactive({remoteReset()})
      )
      
      MagellanNTK::open_dataset_ui(id = ns("open_dataset"))
    })
    
    #### _content -----
    # output$Description_infos_dataset_UI <- renderUI({
    #   req(rv$dataIn)
    #   
    #   infos_dataset_server(
    #     id = "Description_infosdataset",
    #     dataIn = reactive({rv$dataIn})
    #   )
    #   
    #   infos_dataset_ui(id = ns("Description_infosdataset"))
    # })
    
    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE,{
      req(grepl('Description', btnEvents()))
      #rv.custom$result_open_dataset()$dataset
      req(dataIn())
      rv$dataIn <- dataIn()
      
      if(!is.null(rv.custom$result_open_dataset()$dataset))
        rv$dataIn <- rv.custom$result_open_dataset()$dataset
      
      rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'Description', 'Description', 'Initialization', '-')
      
      for (i in names(rv$dataIn))
        DaparToolshed::paramshistory(rv$dataIn[[i]]) <- rbind(DaparToolshed::paramshistory(rv$dataIn[[i]]),
                                                              rv.custom$history)
      
      # DO NOT MODIFY THE THREE FOLLOWING LINES
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- rv$dataIn
      rv$steps.status['Description'] <- MagellanNTK::stepStatus$VALIDATED
    })
    
    ####### _END_ -----
    
    # Insert necessary code which is hosted by MagellanNTK
    # DO NOT MODIFY THIS LINE
    eval(parse(text = MagellanNTK::Module_Return_Func()))
    }
  )
}
