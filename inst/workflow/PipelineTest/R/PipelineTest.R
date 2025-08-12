#' @title Shiny example module `Pipeline A`
#'
#' @description
#' This module contains the configuration information for the corresponding pipeline.
#' It is called by the nav_pipeline module of the package MagellanNTK
#' This documentation is for developers who want to create their own pipelines nor processes
#' to be managed with `MagellanNTK`.
#' 
#' @name module_PiplelineTest
#' @examples
#' if (interactive()){
#' source("~/GitHub/Prostar2/inst/extdata/workflow/PipelineTest/R/PipelineTest.R")
#' path <- system.file('extdata/workflow/PipelineTest', package = 'Prostar2')
#' shiny::runApp(MagellanNTK::workflowApp("PipelineTest"))
#' }
#' 
#' @name PipelineTest
#' 
#' @example inst/workflow/PipelineTest/examples/example_pipelineTest.R
#' @import QFeatures
#' @import DaparToolshed
#' @import MagellanNTK
#' 
NULL


#' @rdname PipelineTest
#' @export
#' 
PipelineTest_conf <- function(){
  MagellanNTK::Config(
  mode = 'pipeline',
  fullname = 'PipelineTest',
  steps = c('Normalization'),
  mandatory = c(FALSE)
)
}



#' @param id xxx
#'
#' @rdname PipelineTest
#'
#' @author Samuel Wieczorek
#' 
#' @export
#' 
PipelineTest_ui <- function(id){
  ns <- NS(id)
}



#' @param id xxx
#'
#' @param dataIn The dataset
#'
#' @param steps.enabled A vector of boolean which has the same length of the steps
#' of the pipeline. This information is used to enable/disable the widgets. It is not
#' a communication variable between the caller and this module, thus there is no
#' corresponding output variable
#'
#'
#' @param remoteReset It is a remote command to reset the module. A boolean that
#' indicates is the pipeline has been reseted by a program of higher level
#' Basically, it is the program which has called this module
#'
#' @param steps.status xxx
#' 
#' @param current.pos xxx
#' 
#' @rdname PipelineTest
#'
#' @importFrom shiny moduleServer reactiveValues observeEvent NS tagList actionLink fluidRow column uiOutput hr reactive fluidPage
#' @importFrom stats setNames
#' 
#' @export
#'
PipelineTest_server <- function(id,
  dataIn = reactive({NULL}),
  steps.enabled = reactive({NULL}),
  remoteReset = reactive({0}),
  steps.status = reactive({NULL}),
  current.pos = reactive({1}),
  path = NULL
  ){


  
  
  # Contrary to the simple workflow, there is no widget in this module
  # because all the widgets are provided by the simple workflows.
  widgets.default.values <- NULL
  rv.custom.default.values <- NULL
  
  ###-------------------------------------------------------------###
  ###                                                             ###
  ### ------------------- MODULE SERVER --------------------------###
  ###                                                             ###
  ###-------------------------------------------------------------###
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    core.code <- MagellanNTK::Get_Workflow_Core_Code(
      name = id,
      w.names = names(widgets.default.values),
      rv.custom.names = names(rv.custom.default.values)
    )
    
    eval(str2expression(core.code))
    
    # Insert necessary code which is hosted by MagellanNTK
    # DO NOT MODIFY THIS LINE
    eval(parse(text = MagellanNTK::Module_Return_Func()))
    }
  )
}


