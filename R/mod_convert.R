#' @title   infos_dataset_ui and infos_dataset_server
#' @description  A shiny Module.
#' 
#' @param id shiny id
#' @param obj An instance of the class `QFeatures`.
#' 
#' @return A shiny app
#'
#' 
#' @name infos_dataset
#' 
#' @examplesIf interactive()
#' data(Exp1_R25_prot, package = 'DaparToolshedData')
#' shiny::runApp(infos_dataset(Exp1_R25_prot))

NULL



#'
#'
#' @rdname infos_dataset
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

#' @rdname infos_dataset
#' @export
#' 
#' @keywords internal
#' 
#' @importFrom tibble as_tibble
#' 
convert_dataset_server <- function(id,
  remoteReset = reactive({NULL}),
  is.enabled = reactive({TRUE})
){
  
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    
  
  
  }
)
}
  



#' @export
#' @rdname infos_dataset
#' 
convert_dataset <- function(obj){
  
  ui <- fluidPage(infos_dataset_ui("mod_info"))
  
  server <- function(input, output, session) {
    infos_dataset_server("mod_info", reactive({obj}))
  }
  
  app <- shiny::shinyApp(ui, server)
}