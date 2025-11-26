#' @title xxx
#' @name PipelineProtein_Description
#' @importFrom QFeatures addAssay removeAssay
#' @import DaparToolshed
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



#' @export
#' @rdname PipelineProtein_Description
PipelineProtein_Description_ui <- function(id){
  ns <- NS(id)

  
}


#' @export
#' @rdname PipelineProtein_Description
PipelineProtein_Description_server <- function(id,
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
  rv.custom.default.values <- NULL
  
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
      # file <- normalizePath(file.path(session$userData$workflow.path, 
      #   'md', paste0(id, '.md')))
      
      file <- normalizePath(file.path(
        system.file('workflow', package = 'Prostar2'),
        unlist(strsplit(id, '_'))[1], 
        'md', 
        paste0(id, '.Rmd')))
     
      MagellanNTK::process_layout(
        session,
        ns = NS(id),
        sidebar = NULL,
        content = tagList(
          if (file.exists(file))
            includeMarkdown(file)
          else
            p('No Description available'),
        )
      )
    })
    

    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE,{
      req(grepl('Description', btnEvents()))
      rv$dataIn <- dataIn()

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
