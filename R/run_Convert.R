#' @title Wrapper to Convert pipeline
#' 
#' @description
#' These functions are inspired by the functions run_workflow() in the package
#' `MagellanNTK`. They wrap the entire workflow into a single function
#' 
#' @examples
#' \dontrun{
#' library(DaparToolshed)
#' library(MagellanNTK)
#' library(Prostar2)
#' shiny::runApp(Prostar2::convert_dataset())
#' }
#' 
#' @name Convert_wrapper
#' 
NULL


#' @rdname Convert_wrapper
#' @export
convert_dataset_ui <- function(id){
  requireNamespace('MagellanNTK')
  ns <- NS(id)
    tagList(
      MagellanNTK::nav_ui(ns('PipelineConvert_Convert'))
    )
}


#' @export
#' @rdname Convert_wrapper
#' 
convert_dataset_server <- function(id,
  reset = reactive({NULL}),
  is.enabled = reactive({TRUE})){
  
  requireNamespace('MagellanNTK')
  

  path <- system.file('workflow/PipelineConvert', package = 'Prostar2')
  dataIn <- NULL
  mode <- "user"
  
  
  MagellanNTK::source_shinyApp_files()
  
  MagellanNTK::source_wf_files(path)
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    dataOut <- reactiveVal()
    session$userData$workflow.path <- path
    
    #observeEvent(dataIn, {
     # session$userData$workflow.path <- path
      
      dataOut(
        nav_server(id = 'PipelineConvert_Convert',
          dataIn = reactive({data.frame()}))
      )
    #})
    
    return(reactive({dataOut()}))
    
  })
}





#' @rdname Convert_wrapper
#' @export
convert_dataset <- function() {
  
  ui <- fluidPage(
    uiOutput('test')
  )
  
  server <- function(input, output, session) {
    
    rv.core <- reactiveValues(
      result_convert = reactive({NULL})
    )
    
    
    output$test <- renderUI({

    rv.core$result_convert <- convert_dataset_server('Convert')
    
    convert_dataset_ui('Convert')
  })
    
    
    observeEvent(req(rv.core$result_convert()$dataOut()$trigger), {
      print(rv.core$result_convert()$dataOut()$value$data)
      print(rv.core$result_convert()$dataOut()$value$name)
    })
  }

  app <- shiny::shinyApp(ui, server)
}
