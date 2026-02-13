#' @title xxx
#' @name PipelinePeptideSC_Description
#' @importFrom QFeatures addAssay removeAssay
#' @import DaparToolshed
#' 
NULL

#' @rdname PipelinePeptideSC_Description
#' @export
#' 
PipelinePeptideSC_Description_conf <- function(){
  MagellanNTK::Config(
    fullname = 'PipelinePeptideSC_Description',
    mode = 'process'
    )
}



#' @export
#' @rdname PipelinePeptideSC_Description
PipelinePeptideSC_Description_ui <- function(id){
  ns <- NS(id)
}


#' @export
#' @rdname PipelinePeptideSC_Description
PipelinePeptideSC_Description_server <- function(id,
    dataIn = reactive({NULL}),
    steps.enabled = reactive({NULL}),
    remoteReset = reactive({0}),
    steps.status = reactive({NULL}),
    current.pos = reactive({1}),
    path = NULL,
    btnEvents = reactive({NULL})
){
  
  pkgs.require(c('QFeatures', 'SummarizedExperiment', 'S4Vectors'))
  
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
    add.resourcePath()
    
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
            p('No Description available'),
        )
      )
    })
    
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
    
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE,{
      req(grepl('Description', btnEvents()))
      req(dataIn())
      rv$dataIn <- dataIn()
      
      if(!is.null(rv.custom$result_open_dataset()$dataset))
        rv$dataIn <- rv.custom$result_open_dataset()$dataset
      
      rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'Description', 'Description', 'Initialization', '-')
      
      for (i in names(rv$dataIn))
        DaparToolshed::paramshistory(rv$dataIn[[i]]) <- rbind(DaparToolshed::paramshistory(rv$dataIn[[i]]),
                                                              rv.custom$history)
      
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- rv$dataIn
      rv$steps.status['Description'] <- MagellanNTK::stepStatus$VALIDATED
    })
    
    # <<< end ------------------------------------------------------------------
    
    # Insert necessary code which is hosted by MagellanNTK
    # DO NOT MODIFY THIS LINE
    eval(parse(text = MagellanNTK::Module_Return_Func()))
  }
  )
}
