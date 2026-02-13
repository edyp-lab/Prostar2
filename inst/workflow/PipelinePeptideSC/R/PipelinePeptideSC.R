#' @title Shiny example module `Pipeline A`
#'
#' @description
#' This module contains the configuration information for the corresponding pipeline.
#' It is called by the nav_pipeline module of the package MagellanNTK
#' This documentation is for developers who want to create their own pipelines nor processes
#' to be managed with `MagellanNTK`.
#' 
#' @name module_PipelinePeptideSC
#' @examples
#' \dontrun{
#' source("~/GitHub/Prostar2/inst/extdata/workflow/PipelinePeptideSC/R/PipelinePeptideSC.R")
#' path <- system.file('extdata/workflow/PipelinePeptideSC', package = 'Prostar2')
#' shiny::runApp(MagellanNTK::workflowApp("PipelinePeptideSC")
#' }
#' 
#' @name PipelinePeptideSC
#' 
#' @example inst/workflow/PipelinePeptideSC/examples/example_PipelinePeptideSC.R
#' 
#' 
NULL


#' @rdname PipelinePeptideSC
#' @export
#' 
PipelinePeptideSC_conf <- function(){
  MagellanNTK::Config(
  mode = 'pipeline',
  fullname = 'PipelinePeptideSC',
  steps = c('Filtering', 'Normalization', 'Imputation', 'Aggregation', 'DifferentialAnalysis'),
  mandatory = c(FALSE, FALSE, FALSE, TRUE, FALSE)
)
}



#' @param id xxx
#'
#' @rdname PipelinePeptideSC
#'
#' @author Samuel Wieczorek
#' 
#' @export
#' 
PipelinePeptideSC_ui <- function(id){
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
#' @rdname PipelineProtein
#'
#' @import shiny
#' @importFrom stats setNames
#' 
#' @export
#'
PipelinePeptideSC_server <- function(id,
  dataIn = reactive({NULL}),
  steps.enabled = reactive({NULL}),
  remoteReset = reactive({0}),
  steps.status = reactive({NULL}),
  current.pos = reactive({1}),
  path = NULL
  ){
  
  pkgs.require(c('QFeatures', 'SummarizedExperiment', 'S4Vectors'))
  
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
    add.resourcePath()
    
    # Insert necessary code which is hosted by MagellanNTK
    # DO NOT MODIFY THIS LINE
    eval(parse(text = MagellanNTK::Module_Return_Func()))
    }
  )
}
