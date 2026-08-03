#' @title PipelineProtein module
#'
#' @description
#' This module contains the configuration information for the protein pipeline.
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
#'   Prostar2("PipelineProtein")
#' }
#' 
#' @name PipelineProtein
#' 
#' @importFrom shiny moduleServer reactiveValues observeEvent NS tagList actionLink fluidRow column uiOutput hr reactive fluidPage
#' @importFrom stats setNames
#' @importFrom QFeatures addAssay removeAssay
#' @import DaparToolshed
#' 
#' @return An instance of the class `MultiAssayExperiment`
#' 
NULL


#' @rdname PipelineProtein
#' @export
#' 
PipelineProtein_conf <- function(){
  MagellanNTK::Config(
  mode = 'pipeline',
  fullname = 'PipelineProtein',
  steps = c('Filtering', 'Normalization', 'Imputation', 'HypothesisTest', 'DA'),
  mandatory = c(FALSE, FALSE, FALSE, TRUE, FALSE)
)
}


#' @rdname PipelineProtein
#' @export
#' 
PipelineProtein_ui <- function(id){
  ns <- NS(id)
}


#' @rdname PipelineProtein
#' @export
#' 
PipelineProtein_server <- function(id,
  dataIn = reactive({NULL}),
  steps.enabled = reactive({NULL}),
  remoteReset = reactive({0}),
  steps.status = reactive({NULL}),
  current.pos = reactive({1}),
  path = NULL
  ){
  
  pkgs_require(c('QFeatures', 'SummarizedExperiment', 'S4Vectors'))
  
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
    add_resourcePath()
    
    # Insert necessary code which is hosted by MagellanNTK
    # DO NOT MODIFY THIS LINE
    eval(parse(text = MagellanNTK::Module_Return_Func()))
    }
  )
}
