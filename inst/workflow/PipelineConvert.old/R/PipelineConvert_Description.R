#' @title xxx
#' @name PipelineConvert_Description
#' @importFrom QFeatures addAssay removeAssay
#' @import DaparToolshed
#' 
NULL

#' @rdname PipelineConvert_Description
#' @export
#' 
PipelineConvert_Description_conf <- function(){
  MagellanNTK::Config(
    fullname = 'PipelineConvert_Description',
    mode = 'process'
  )
}



#' @export
#' @rdname PipelineConvert_Description
PipelineConvert_Description_ui <- function(id){
  ns <- NS(id)
  
  
}


#' @export
#' @rdname PipelineConvert_Description
PipelineConvert_Description_server <- function(id,
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
    result_open_dataset = reactive({NULL})
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
    
    
    ###### ------------------- Code for Description (step 0) -------------------------    #####
    output$Description <- renderUI({
      
      file <- normalizePath(file.path(
        system.file('workflow', package = 'Prostar2'),
        unlist(strsplit(id, '_'))[1], 
        'md', 
        paste0(id, '.Rmd')))
      
      MagellanNTK::process_layout(
        session,
        ns = NS(id),
        sidebar = tagList(
          uiOutput(ns('open_dataset_UI'))
        ),
        content = tagList(
          
          if (file.exists(file))
            includeMarkdown(file)
          else
            p('No Description available'),
          uiOutput(ns('Description_infos_dataset_UI'))
        )
      )
    })
    
    
    
    
    output$open_dataset_UI <- renderUI({
      rv.custom$result_open_dataset <- MagellanNTK::open_dataset_server(
        id = "open_dataset",
        class = 'QFeatures',
        extension = "qf",
        remoteReset = reactive({remoteReset()})
      )
      
      MagellanNTK::open_dataset_ui(id = ns("open_dataset"))
    })
    
    observeEvent(rv.custom$result_open_dataset()$trigger, ignoreNULL = FALSE, {
      #browser()
      print(rv.custom$result_open_dataset()$trigger)
      print(rv.custom$result_open_dataset()$dataset)
    })
    
    
    output$Description_infos_dataset_UI <- renderUI({
      req(rv.custom$result_open_dataset()$dataset)
      
      infos_dataset_server(
        id = "Description_infosdataset",
        dataIn = reactive({rv.custom$result_open_dataset()$dataset})
      )
      
      infos_dataset_ui(id = ns("Description_infosdataset"))
    })
    
    
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE,{
      req(grepl('Description', btnEvents()))
      rv.custom$result_open_dataset()$dataset
      
      # On envoie un objet vide, fictif car sinon l'etape ne se valide
      # pas et on ne peut pas faire le convert
      rv$dataIn <- QFeatures::QFeatures()
      rv$dataIn <- QFeatures::addAssay(rv$dataIn, SummarizedExperiment::SummarizedExperiment(), name = 'tmp')
      
      rv$dataIn <- MultiAssayExperiment::MultiAssayExperiment()
      if(!is.null(rv.custom$result_open_dataset()$dataset))
        rv$dataIn <- rv.custom$result_open_dataset()$dataset
      browser()
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- rv$dataIn
      rv$steps.status['Description'] <- stepStatus$VALIDATED
    })
    
    
    # Insert necessary code which is hosted by MagellanNTK
    # DO NOT MODIFY THIS LINE
    eval(parse(text = MagellanNTK::Module_Return_Func()))
    
  }
  )
}
