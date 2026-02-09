#' @title xxx
#' @name PipelineProtein_Save
#' 
#' @examples
#' NULL
#' 
#' @importFrom QFeatures addAssay removeAssay
#' @import DaparToolshed
#' 

#' @export
#' @rdname PipelineProtein_Save
PipelineProtein_Save_conf <- function(){
  MagellanNTK::Config(
    fullname = 'PipelineProtein_Save',
    mode = 'process'
  )
}



#' @export
#' @rdname PipelineProtein_Save
PipelineProtein_Save_ui <- function(id){
  ns <- NS(id)
}


#' @export
#' @rdname PipelineProtein_Save
PipelineProtein_Save_server <- function(id,
  dataIn = reactive({NULL}),
  steps.enabled = reactive({NULL}),
  remoteReset = reactive({0}),
  steps.status = reactive({NULL}),
  current.pos = reactive({1}),
  btnEvents = reactive({NULL})
){
  
  
  pkgs.require(c('QFeatures', 'SummarizedExperiment', 'S4Vectors'))
  
  # Define default selected values for widgets
  # By default, this list is empty for the Save module
  # but it can be customized
  widgets.default.values <- list()
  rv.custom.default.values <- list(
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
    
    
    observeEvent(req(dataIn()), {
      rv$dataIn <- dataIn()
    })
    
    
    ###### ------------------- Code for Save (step 0) -------------------------    #####
    output$Save <- renderUI({
      
      file <- normalizePath(file.path(
        system.file('workflow', package = 'Prostar2'),
        unlist(strsplit(id, '_'))[1], 
        'md', 
        paste0(id, '.Rmd')))
      
      
      MagellanNTK::process_layout(session,
                                  ns = NS(id),
                                  sidebar = tagList(),
                                  content = tagList(
                                    uiOutput(ns('dl_ui')),
                                    if (file.exists(file))
                                      includeMarkdown(file)
                                    else
                                      p('No Save available'),
                                  )
      )
    })
    
    output$dl_ui <- renderUI({
      req(rv$steps.status['Save'] == MagellanNTK::stepStatus$VALIDATED)
      req(config@mode == 'pipeline')
      
      Prostar2::download_dataset_ui(ns(paste0(id, '_createQuickLink')))
    })
    

    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl('Save', btnEvents()))
      shiny::withProgress(message = paste0("Saving all processes", id), {
        shiny::incProgress(0.5)
        
        S4Vectors::metadata(rv$dataIn)$name.pipeline <- 'PipelineProtein'
        # DO NOT MODIFY THE THREE FOLLOWINF LINES
        dataOut$trigger <- MagellanNTK::Timestamp()
        dataOut$value <- rv$dataIn
        rv$steps.status['Save'] <- MagellanNTK::stepStatus$VALIDATED
        
        Prostar2::download_dataset_server('createQuickLink', dataIn = reactive({dataOut$value}))
      })
    })
    
    
    # Insert necessary code which is hosted by MagellanNTK
    # DO NOT MODIFY THIS LINE
    eval(parse(text = MagellanNTK::Module_Return_Func()))
    
  }
  )
}
