#' det_quant_impute_Values UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom DAPAR2 getQuantile4Imp
#' @importFrom tibble as_tibble_row
#' 
mod_det_quant_impute_Values_ui <- function(id){
  ns <- NS(id)
  tagList(
    h5("The missing values will be imputed by the following values :"),
    mod_format_DT_ui(ns('detQuantValues_DT'))
  )
}

#' det_quant_impute_Values Server Function
#'
#' @noRd 
mod_det_quant_impute_Values_server <- function(id, qData, quant, factor){
  
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    req(qData)
    values <- reactiveVal(NULL)
    
    mod_format_DT_server('detQuantValues_DT',
                         table2show = reactive({req(values()$shiftedImpVal)
                           as_tibble_row(round(values()$shiftedImpVal, digits=3))
                         }),
                         style = reactive({NULL})
    )
    
    observe({
      req(qData(), quant(), factor())
      
      tmp <- DAPAR2::getQuantile4Imp(qData(), quant()/100, factor())
      values(tmp)
    })
    
    
  })
  
}

## To be copied in the UI
# mod_det_quant_impute_Values_ui("det_quant_impute_Values_ui_1")

## To be copied in the server
# callModule(mod_det_quant_impute_Values_server, "det_quant_impute_Values_ui_1")

