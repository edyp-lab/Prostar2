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
  r.nav <- reactiveValues(
    name = "Convert",
    stepsNames = c("Select file", "Data Id", "Epx. & feat. data", "Build design", "Convert"),
    ll.UI = list( screenStep1 = uiOutput(ns("Convert_SelectFile")),
                  screenStep2 = uiOutput(ns("Convert_DataId")),
                  screenStep3 = uiOutput(ns("Convert_ExpFeatData")),
                  screenStep4 = uiOutput(ns("Convert_BuildDesign")),
                  screenStep5 = uiOutput(ns("Convert_Convert"))
                  ),
    isDone =  rep(FALSE,5),
    mandatory =  rep(FALSE,5),
    reset = FALSE
  )
  
  ## reactive values for variables in the module
  rv.convert <- reactiveValues(
    data2convert = NULL,
    dataIn = NULL,
    obj =  NULL,
    design = NULL,
    
    # contient l'objet de sortie du module (ie. a MAE instance)
    dataOut = NULL, 
    name = "processConvert"
  )
  
  
  
  observeEvent(req(r.nav$reset),{
    ## do not modify this part
    r.nav$isDone <- rep(FALSE, 3)
    r.nav$reset <- FALSE
    ## end of no modifiable part
    
    ## update widgets whose names are in r.widgets with the value in this list
    # updateCheckboxInput(session,'selectIdent', value=NULL)
    # updateSelectInput(session,'convert_proteinId', selected=NULL)
    # updateSelectInput(session,'idBox', selected=NULL)
    # updateRadioButtons(session, "typeOfData", selected=NULL)
    # updateRadioButtons(session, "checkDataLogged", selected=NULL)
    # updateCheckboxInput(session,"replaceAllZeros", value=NULL)
  })
  
  
  callModule(mod_navigation_server, 'nav_convert',style=2,pages=r.nav)
  
 
  
  
  
  #### END of template part of the module
  
  
  
  callModule(mod_popover_for_help_server,"modulePopover_convertDataQuanti", 
             data = reactive(list(title = HTML(paste0("<strong><font size=\"4\">Quantitative data</font></strong>")), 
                                  content="Select the columns that are quantitation values by clicking in the field below.")))
  

  
  
  callModule(mod_insert_md_server, "FAQ_MD2",URL_FAQ)
  
  
  callModule(mod_infos_dataset_server, "infoAboutMSnset",
             obj = reactive({
               req(rv.convert$dataOut)
               rv.convert$dataOut@datasets[[1]]
             }))
  
  
 
  
  
  ##
  ## Definitions of the screens
  ##
  
  ###---------------------------------------------###
  ###                 Screen 1                    ###
  ###---------------------------------------------###
  output$Convert_SelectFile <- renderUI({
    tagList(
      mod_import_file_from_ui(ns('importFile')),
      br(),
      uiOutput(ns("ConvertOptions"))
    )
  })
  
  
  
  output$ConvertOptions <- renderUI({
    req(rv.convert$dataIn)
    
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
  
  
  rv.convert$data2convert <- callModule(mod_import_file_from_server,'importFile')
  
  observeEvent(req(rv.convert$data2convert()),{ 
    rv.convert$dataIn <- rv.convert$data2convert()
    r.nav$isDone[1] <- TRUE
    })

  ###---------------------------------------------###
  ###                 Screen 2                    ###
  ###---------------------------------------------###
  output$Convert_DataId <- renderUI({

    tagList(
      tags$div(
        tags$div( style="display:inline-block; vertical-align: top; padding-right: 100px;",
                  uiOutput(ns("choose_keyID_ui")),
                  uiOutput(ns("warning_keyID_ui"))
        ),
         tags$div( style="display:inline-block; vertical-align: top;",
                   uiOutput(ns("choose_col_Parent_Protein_ui")),
                   tableOutput(ns('preview_col_Parent_Protein_ui')),
                   uiOutput(ns("note_col_Parent_Protein_ui")),
                   uiOutput("RemoveOrphanPept_ui")
         )
      )
    )
  })

  callModule(mod_popover_for_help_server,"modulePopover_convertIdType", 
             data = list(title = HTML(paste0("<strong><font size=\"4\">Key ID definition</font></strong>")), 
                                  content="If you choose the automatic ID, Prostar will build an index."))
  
  output$choose_keyID_ui <- renderUI({
    req(rv.convert$dataIn)
     .choices <- c("Auto ID",colnames(rv.convert$dataIn))
    names(.choices) <- c("Auto ID",colnames(rv.convert$dataIn))
    
    tagList(
      mod_popover_for_help_ui(ns("modulePopover_convertIdType")),
      selectInput(ns("choose_keyID"), label = "", choices = .choices)
    )
    
  })
   
  
  
  output$warning_keyID_ui <- renderUI({
    req(input$choose_keyID)
    
    #isolate({
      if (input$choose_keyID =="Auto ID") {
        text <- "<img src=\"images/Ok.png\" height=\"24\"></img>"
      }
      else {
        t <- (length(as.data.frame(rv.convert$dataIn)[, input$choose_keyID])
              == length(unique(as.data.frame(rv.convert$dataIn)[, input$choose_keyID])))
        
        if (!t){
          text <- "<img src=\"images/Problem.png\" height=\"24\"></img><font color=\"red\">
        Warning ! Your ID contains duplicate data.
        Please choose another one."
        }
        else {
          text <- "<img src=\"images/Ok\" height=\"24\"></img><font color=\"green\">
        This column is valid to serve as a unique ID for entities"
        }
      }
      HTML(text)
      
    #})
  })
  
  
  callModule(mod_popover_for_help_server,"parentProtein", 
             data = list(title = HTML(paste0("<strong><font size=\"4\">Select col for parent protein IDs</font></strong>")), 
                                  content="Select the column containing the parent protein IDs."))
  
  
  
  output$choose_col_Parent_Protein_ui <- renderUI({
    req(input$typeOfData)
    if (input$typeOfData != 'peptide') { return(NULL)}
    
    .choices <- c("",colnames(rv.convert$dataIn))
    names(.choices) <- c("",colnames(rv.convert$dataIn))
    tagList(
      mod_popover_for_help_ui(ns("parentProtein")),
      selectInput(ns("choose_col_Parent_Protein"),"",choices =  .choices , selected = character(0))
    )
  })
  
  
  output$preview_col_Parent_Protein_ui <- renderTable(
    #req(input$choose_col_Parent_Protein)
    if (is.null(input$choose_col_Parent_Protein) || input$choose_col_Parent_Protein == "") {
      return (NULL)
      } else{
     head(rv.convert$dataIn[,input$choose_col_Parent_Protein])
        },colnames=FALSE
    
  )
  
  output$note_col_Parent_Protein_ui <- renderUI({
    req(input$typeOfData)
    if (input$typeOfData != 'peptide') { return(NULL)}
    
    tagList(
      p("Please note that in case of multiple parent protein for one peptide, their IDs must be
          separated by a semi-colon. If not, the agregation tool and peptide-protein graphs 
          cannot be run."),
      checkboxInput(ns('confirm_separator'), 
                    'I confirm that the separator is correct',
                    value = FALSE)
    )
  })

  
  output$RemoveOrphanPept_ui <- renderUI({
    req(input$typeOfData)
    if (input$typeOfData != 'peptide') { return(NULL)}
    req(input$choose_col_Parent_Protein)
      index <- which(is.na(rv.convert$dataIn[,input$choose_col_Parent_Protein]))
      
      
      if (length(index) > 0) {
        
        ifelse (length(index)==1, 
                txt <-"One peptide does not have any parent protein.",
                txt <- paste0(length(index), " peptides don't have any parent protein.")
        )
        tagList(
          p(txt),
          actionButton(ns('RemoveOrphanPept_btn'), 'Remove orphan peptides')
        )
      } else {
        p("No orphan peptides were detected.")
      }
      
    }) 
    
    observeEvent(input$RemoveOrphanPept_btn, {
      req(input$choose_col_Parent_Protein)
      index <- which(is.na(rv.convert$dataIn[,input$choose_col_Parent_Protein]))
      rv.convert$dataIn <- rv.convert$dataIn[-index,]
    })
    
  
  ###---------------------------------------------###
  ###                 Screen 3                    ###
  ###---------------------------------------------###
  output$Convert_ExpFeatData <- renderUI({

    tagList(
 
    )
  })


  ###---------------------------------------------###
  ###                 Screen 4                    ###
  ###---------------------------------------------###
  output$Convert_BuildDesign <- renderUI({
    req(rv$widgets$Convert$datafile)
    req(input$file1)
    
    mod_build_design_ui('buildDesign')
 })

  rv.convert$design <- callModule(mod_build_design_server, 'buildDesign', sampleNames=reactive({NULL}))
  

  ###---------------------------------------------###
  ###                 Screen 5                    ###
  ###---------------------------------------------###
  output$Convert_Convert <- renderUI({
    tagList(
      uiOutput(ns("convertFinalStep")),
      uiOutput(ns("conversionDone")),
      mod_infos_dataset_ui(ns("infoAboutMSnset"))
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
 
