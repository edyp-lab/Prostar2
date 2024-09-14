#' @title   infos_dataset_ui and infos_dataset_server
#' @description  A shiny Module.
#' 
#' @param id shiny id
#' @param obj An instance of the class `QFeatures`.
#' 
#' @return A shiny app
#'
#' 
#' @name convert_dataset
#' 
#' @examplesIf interactive()
#' library(MagellanNTK)
#' library(Prostar2)
#' library(DaparToolshed)
#' data(Exp1_R25_prot, package = 'DaparToolshedData')
#' shiny::runApp(convert_dataset())
#' 

NULL



#'
#'
#' @rdname convert_dataset
#'
#' @export 
#' @importFrom shiny NS tagList 
#' @import QFeatures
#' 
convert_dataset_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    
  )
}





# Module Server

#' @rdname convert_dataset
#' @export
#' 
#' @keywords internal
#' 
#' @importFrom tibble as_tibble
#' 
convert_dataset_server <- function(id,
  remoteReset = reactive({0}),
  is.enabled = reactive({TRUE})
){
  
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    
  
  
  }
)
}
  



#' @export
#' @rdname convert_dataset
#' 
convert_dataset <- function(){
  
  ui <- fluidPage(convert_dataset_ui("mod_info"))
  
  server <- function(input, output, session) {
    convert_dataset_server("mod_info")
  }
  
  app <- shiny::shinyApp(ui, server)
}