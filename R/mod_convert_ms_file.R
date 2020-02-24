# Module UI
  
#' @title   mod_convert_ms_file_ui and mod_convert_ms_file_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_convert_ms_file
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_convert_ms_file_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_navigation_ui(ns('nav_convert'))
  )
}
    
# Module Server
    
#' @rdname mod_convert_ms_file
#' @export
#' @keywords internal
#' @importFrom shinyBS bsModal
    
mod_convert_ms_file_server <- function(input, output, session){
  ns <- session$ns
  
  
  callModule(mod_popover_for_help_server,"modulePopover_convertChooseDatafile", 
             data = reactive(list(title = HTML(paste0("<strong><font size=\"4\">Data file</font></strong>")), 
                                  content="Select one (.txt, .csv, .tsv, .xls, .xlsx) file.")))
  
  # Variable to manage the different screens of the module
  rv.convert <- reactiveValues(
    # structure needed fior the navigation module
    nav = NavStructure(name = "Convert",
                       stepsNames = c("Select file", "Data Id", "Epx. & feat. data", "Build design", "Convert"),
                       ll.UI = list( screenStep1 = uiOutput(ns("Convert_SelectFile")),
                                     screenStep2 = uiOutput(ns("Convert_DataId")),
                                     screenStep3 = uiOutput(ns("Convert_ExpFeatData")),
                                     screenStep4 = uiOutput(ns("Convert_BuildDesign")),
                                     screenStep5 = uiOutput(ns("Convert_Convert"))
                       ),
                       isDone =  rep(FALSE,5),
                       forceReset = FALSE
                      ),
    data2convert = NULL,
    obj =  NULL,
    design = NULL,
    
    # contient l'objet de sortie du module (ie. a MAE instance)
    dataOut = NULL, 
    name = "processConvert"
  )
  
  
  callModule(mod_popover_for_help_server,"modulePopover_convertIdType", 
             data = reactive(list(title = HTML(paste0("<strong><font size=\"4\">ID definition</font></strong>")), 
                                  content="If you choose the automatic ID, Prostar will build an index.")))
  
  callModule(mod_popover_for_help_server,"modulePopover_convertDataQuanti", 
             data = reactive(list(title = HTML(paste0("<strong><font size=\"4\">Quantitative data</font></strong>")), 
                                  content="Select the columns that are quantitation values by clicking in the field below.")))
  
  callModule(mod_popover_for_help_server,"modulePopover_convertProteinID", 
             data = reactive(list(title = HTML(paste0("<strong><font size=\"4\">Select protein IDs</font></strong>")), 
                                  content="Select the column containing the parent protein IDs.")))
  
  
  callModule(mod_insert_md_server, "FAQ_MD2",URL_FAQ)
  
  
  callModule(mod_infos_dataset_server, "infoAboutMSnset",
             obj = reactive({
               req(rv.convert$dataOut)
               rv.convert$dataOut@datasets[[1]]
             }))
  
  rv.convert$data2convert <- callModule(mod_import_file_from_server,'importFile')
  
  rv.convert$design <- callModule(mod_build_design_server, 'buildDesign', sampleNames=reactive({NULL}))
 
  
  
  callModule(mod_navigation_server, 'nav_convert',pages=reactive({rv.convert$nav}))
 
  
  resetModuleConvert<- reactive({  
    # ## update widgets values (reactive values)
    # resetModuleProcess("Convert")
    # 
    # ## update widgets in UI
    # updateCheckboxInput(session,"selectIdent", value = FALSE)
    # updateSelectInput(session,"convert_proteinId",selected = character(0))
    # updateSelectInput(session,"idBox", selected = "Auto ID")
    # updateRadioButtons(session, "typeOfData", selected="peptide")
    # updateRadioButtons(session, "checkDataLogged", selected="no")
    # updateCheckboxInput(session,"replaceAllZeros", value= TRUE)
    
    rv.convert$nav@isDone = rep(FALSE, 5)
  })

  
  
  
  ##
  ## Definitions of the screens
  ##
  ## Screen 1
  output$Convert_SelectFile <- renderUI({
    tagList(
      mod_import_file_from_ui(ns('importFile')),
      br(),
      uiOutput(ns("ConvertOptions"))
    )
  })
  

  #################################
  ### Screen 2
  output$Convert_DataId <- renderUI({

    tagList(
      tags$div(
        tags$div( style="display:inline-block; vertical-align: top; padding-right: 100px;",
                  uiOutput(ns("id")),
                  uiOutput(ns("warningNonUniqueID"))
        ),
        tags$div( style="display:inline-block; vertical-align: top;",
                  uiOutput(ns("convertChooseProteinID_UI"))
        )
      )
    )
  })


  #################################
  ### Screen 3
  output$Convert_ExpFeatData <- renderUI({

    tagList(
      fluidRow(
        column(width=4,checkboxInput(ns("selectIdent"),
                                     "Select columns for identification method",
                                     value = FALSE)),
        column(width=4,uiOutput(ns("checkIdentificationTab")))
      ),
      fluidRow(
        column(width=4,uiOutput(ns("eData"),width = "400px")),
        column(width=8,DT::dataTableOutput(ns("x1"), width='500px'))),
      tags$script(HTML("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
                     Shiny.unbindAll($('#'+id).find('table').DataTable().table().node());})"))
    )
  })


  #################################
  ### Screen 4 : Build design
  output$Convert_BuildDesign <- renderUI({
    req(rv$widgets$Convert$datafile)
    req(input$file1)
    
    mod_build_design_ui('buildDesign')
 })



  #################################
  ### Screen 5
  output$Convert_Convert <- renderUI({
    tagList(
      uiOutput(ns("convertFinalStep")),
      uiOutput(ns("conversionDone")),
      mod_infos_dataset_ui(ns("infoAboutMSnset"))
    )
  })
  
  

  
  
  output$ConvertOptions <- renderUI({
    req(input$data2convert)

    tagList(
      radioButtons(ns("typeOfData"),
                   "Choose the pipeline to use with the data",
                   choices=c("peptide" = "peptide",
                             "protein" = "protein",
                             "peptide to protein (p2p)" = "p2p")
      )

      ,radioButtons(ns("checkDataLogged"),
                    "Are your data already log-transformed ?",
                    #width = widthWellPanel,
                    choices=c("yes (they stay unchanged)" = "yes",
                              "no (they will be log-transformed by the conversion tool)"="no"),
                    selected="no")
      ,br()
      ,checkboxInput(ns("replaceAllZeros"),
                     "Replace all '0' and 'NaN' by NA",
                     value= TRUE)
    )
  })

  
  

 #  
 #  ############################################################################
 #  ####### ENd definitino of UI   ##################
 #  #############################################################################
 
  
  return(reactive({rv.convert$dataOut}))

}
    
## To be copied in the UI
# mod_convert_ms_file_ui("convert_ms_file_ui_1")
    
## To be copied in the server
# callModule(mod_convert_ms_file_server, "convert_ms_file_ui_1")
 
