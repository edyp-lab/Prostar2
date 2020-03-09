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
#' @importFrom shinyjs hidden toggle
    
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
    hot = NULL,
    tab1 = NULL,
    designChecked = NULL,
    # contient l'objet de sortie du module (ie. a MAE instance)
    dataOut = NULL, 
    name = "processConvert"
  )
  
  
  
  observeEvent(req(r.nav$reset),{
    
    
    ## update widgets whose names are in r.widgets with the value in this list
    ## This part must be before the reinitialization of r.nav$isDone
    # updateCheckboxInput(session,'selectIdent', value=NULL)
    # updateSelectInput(session,'convert_proteinId', selected=NULL)
    # updateSelectInput(session,'idBox', selected=NULL)
    # updateRadioButtons(session, "typeOfData", selected=NULL)
    # updateRadioButtons(session, "checkDataLogged", selected=NULL)
    # updateCheckboxInput(session,"replaceAllZeros", value=NULL)
    
    ## do not modify this part
    r.nav$isDone <- rep(FALSE, 5)
    r.nav$reset <- FALSE
    ## end of no modifiable part
  })
  
  
  callModule(mod_navigation_server, 'nav_convert',style=2,pages=r.nav)
  
 
  
  
  
  #### END of template part of the module
  
  
 
  
  
  callModule(mod_insert_md_server, "FAQ_MD2",URL_FAQ)
  
  
  callModule(mod_infos_dataset_server, "infoAboutMSnset",
             obj = reactive({
               req(rv.convert$dataOut)
               rv.convert$dataOut@datasets[[1]]
             }))
  
  
 
  
  
  ##
  ## Definitions of the screens
  ##
  
  ###---------------------------------------------------------------------------------###
  ###                                 Screen 1                                        ###
  ###---------------------------------------------------------------------------------###
  output$Convert_SelectFile <- renderUI({
    tagList(
      mod_import_file_from_ui(ns('importFile')),
      br(),
      uiOutput(ns("ConvertOptions"))
    )
  })
  
  
  
  output$ConvertOptions <- renderUI({
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

  ###---------------------------------------------------------------------------------###
  ###                                 Screen 2                                        ###
  ###---------------------------------------------------------------------------------###
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
                   uiOutput(ns("RemoveOrphanPept_ui"))
         )
      )
    )
  })

  callModule(mod_popover_for_help_server,"modulePopover_convertIdType", 
             data = list(title = HTML(paste0("<strong><font size=\"4\">Key ID definition</font></strong>")), 
                                  content="If you choose the automatic ID, Prostar will build an index."))
  
  output$choose_keyID_ui <- renderUI({
   isolate({
      .choices <- c("", "AutoID",colnames(rv.convert$dataIn))
    names(.choices) <- c("None","-- Auto ID --",colnames(rv.convert$dataIn))
    
    tagList(
      mod_popover_for_help_ui(ns("modulePopover_convertIdType")),
      selectInput(ns("choose_keyID"), label = "", choices = .choices, selected=character(0))
    )
    })
  })
   
  
  
  output$warning_keyID_ui <- renderUI({
    req(input$choose_keyID)
    
    #isolate({
      if (input$choose_keyID =="AutoID") {
        text <- "<img src=\"images/Ok\" height=\"24\"></img><font color=\"green\">
        This column is valid to serve as a unique ID for entities"
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
    isolate({
      .choices <- c("",colnames(rv.convert$dataIn))
      names(.choices) <- c("",colnames(rv.convert$dataIn))
      tagList(
        mod_popover_for_help_ui(ns("parentProtein")),
        selectInput(ns("choose_col_Parent_Protein"),"",choices =  .choices , selected = character(0))
      )
    })
  })
  
  
  output$preview_col_Parent_Protein_ui <- renderTable(
   # req(input$choose_col_Parent_Protein)
    if (input$typeOfData != 'peptide' || is.null(input$choose_col_Parent_Protein) || input$choose_col_Parent_Protein == "") {
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
    
    
    
    observe({
      req(rv.convert$dataIn)
      req(input$choose_keyID)
      test_keyID <- test_parentProt <- TRUE
      
      if(input$typeOfData == "peptide"){
        test_parentProt <- !(input$choose_col_Parent_Protein == "") && !is.null(input$choose_col_Parent_Protein) && isTRUE(input$confirm_separator)
       }
      
      
      if (input$choose_keyID =="AutoID") {
        test_keyID <- TRUE
      } else {
        test_keyID <- (length(as.data.frame(rv.convert$dataIn)[, input$choose_keyID])
                  == length(unique(as.data.frame(rv.convert$dataIn)[, input$choose_keyID])))
      }
      
      r.nav$isDone[2] <- test_keyID && test_parentProt
    })
    
    
    
    
  
###---------------------------------------------------------------------------------###
###                                 Screen 3                                        ###
###---------------------------------------------------------------------------------###
  output$Convert_ExpFeatData <- renderUI({

    tagList(
      fluidRow(
        column(width=4,uiOutput(ns("choose_quanti_data_col_ui"),width = "400px"))
         ,column(width=8,
                 shinyjs::hidden(checkboxInput(ns("select_Identification"), 
                               "Select columns for identification method", 
                               value = NULL))
                 ,uiOutput(ns("check_Identification_Tab_ui"))
                 
                 ,tags$script(HTML("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
                                   Shiny.unbindAll($('#'+id).find('table').DataTable().table().node());
                                   })"))
                 ,uiOutput(ns('show_Identification_Tab_ui'))
               )
         )
      )
  })
  
  
  
  callModule(mod_popover_for_help_server,"modulePopover_convertDataQuanti", 
             data = list(title = h3('Quantitative data'), 
                        content="Select the columns that are quantitation values by clicking in the field below."))
  
  
  
  output$choose_quanti_data_col_ui <- renderUI({
    req(rv.convert$dataIn)
    tagList(
      mod_popover_for_help_ui(ns("modulePopover_convertDataQuanti")),
      selectInput(ns("choose_quanti_data_col"),
                  label = "",
                  choices = colnames(rv.convert$dataIn),
                  selected = character(0),
                  multiple = TRUE, 
                  width='200px',
                  size = 20,
                  selectize = FALSE)
    )
  })
  
  observeEvent(input$choose_quanti_data_col,{
    shinyjs::toggle('select_Identification', condition=length(input$choose_quanti_data_col)>0)
  })
  
  
  
  check_Identification_Tab <- reactive({
    req(input$select_Identification)
    req(input$choose_quanti_data_col)
    
    temp <- shinyValue("colForOriginValue_",length(input$choose_quanti_data_col))
    
    isOk <- TRUE
    msg <- NULL
    if ((length(which(temp == "None")) == length(temp)))
    {
      isOk <- TRUE
      msg <- "Correct"
    }  else {
      if (length(which(temp == "None")) > 0)
      {
        isOk <- FALSE
        msg <- "The identification method is not appropriately defined for each sample."
      } else {
        if(length(temp) != length(unique(temp))){
          isOk <- FALSE
          msg <- "There are duplicates in identification columns."
        }else {
          isOk <- TRUE
          msg <- "Correct"
        }
      }
    }
     
    rep <- list(isOk=isOk, msg = msg)
    rep
})
  
  output$check_Identification_Tab_ui <- renderUI({
     rep <- check_Identification_Tab()
    
     if (isTRUE(rep$isOk)){
      img <- "images/Ok.png"
    } else {
      img <-"images/Problem.png"
    }
    tags$div(
      tags$div(
        tags$div(style="display:inline-block;",tags$img(src = img, height=25)),
        tags$div(style="display:inline-block;",tags$p(rep$msg))
      )
    )
  })
  
  
  shinyOutput <- function(FUN,id,num,...) {
    inputs <- character(num)
    for (i in seq_len(num)) {
      inputs[i] <- as.character(FUN(paste0(id,i),label=NULL,...))
    }
    inputs
  }
  
  
  # function for dynamic inputs in DT
  shinyInput <- function(FUN,id,num,...) {
    inputs <- character(num)
    for (i in seq_len(num)) {
      inputs[i] <- as.character(FUN(paste0(id,i),label=NULL,...))
    }
    inputs
  }
  
  
  # function to read DT inputs
  shinyValue <- function(id,num) {
    unlist(lapply(seq_len(num),function(i) {
      value <- input[[paste0(id,i)]]
      if (is.null(value)) NA else value
    }))
  }
  
  #####################
  

  
  
  # observeEvent(input$fData.box,ignoreInit = TRUE,{
  #   
  #   choices = colnames(rv$tab1)[-which(colnames(rv$tab1) %in% input$fData.box)]
  #   names(choices) = 
  #     colnames(rv$tab1)[-which(colnames(rv$tab1) %in% input$fData.box)]
  #   updateSelectInput(session, "choose_quanti_data_colx", 
  #                     label = "",
  #                     choices = choices,
  #                     selected = input$choose_quanti_data_col)
  # })
  # 
  

  output$show_Identification_Tab_ui <- renderUI({
    
    if (length(input$choose_quanti_data_col) == 0 || !isTRUE(input$select_Identification)){
      return(NULL)
    }
    DT::dataTableOutput(ns("show_table"), width='500px')
  })
  
  
  output$show_table <- DT::renderDataTable(
    quantiDataTable(),
    escape=FALSE,
    rownames = FALSE,
    extensions = c('Scroller'),
    server=FALSE,
    selection='none',
    class = 'compact',
    options=list(
      preDrawCallback=JS(
        'function() {
            Shiny.unbindAll(this.api().table().node());}'),
      drawCallback= JS(
        'function(settings) {
            Shiny.bindAll(this.api().table().node());}'),
      # rowCallback = JS("function(r,d) {$(r).attr('height', '10px')}"),
      dom = 'Bfrtip',
      autoWidth=TRUE,
      deferRender = TRUE,
      bLengthChange = FALSE,
      scrollX = 200,
      scrollY = 500,
      scroller = TRUE,
      ajax = list(url = dataTableAjax(session, quantiDataTable()))

    )

  )
  

  
  quantiDataTable <- reactive({
    print("IN quantiDataTable()")
    req(input$choose_quanti_data_col)
    req(rv.convert$dataIn)

    session$sendCustomMessage('unbind-DT', 'show_table')
    df <- NULL
    choices <- c("None",colnames(rv.convert$dataIn))
    names(choices) <- c("None",colnames(rv.convert$dataIn))

    if (isTRUE(input$select_Identification)) {

      df <- data.frame(as.data.frame(input$choose_quanti_data_col),
                       shinyInput(selectInput,
                                  ns("colForOriginValue_"),
                                  nrow(as.data.frame(input$choose_quanti_data_col)),
                                  choices=choices)
      )
      colnames(df) <- c("Sample", "Identification method")
    } else {
      df <- data.frame(Sample = as.data.frame(input$choose_quanti_data_col))
      colnames(df) <- c("Sample")
    }

    df
  })
  
  observe({
    input$select_Identification
    if (isTRUE(input$select_Identification)){
      r.nav$isDone[3] <- length(input$choose_quanti_data_col)>0  && check_Identification_Tab()$isOk 
    } else {
      r.nav$isDone[3] <- length(input$choose_quanti_data_col)>0 
    }
  })
  
  
   observeEvent(shinyValue("colForOriginValue_",nrow(quantiDataTable())),{})
  

  ###---------------------------------------------###
  ###                 Screen 4                    ###
  ###---------------------------------------------###
   rv.convert$design <- callModule(mod_build_design_server, 'buildDesign', sampleNames=reactive({input$choose_quanti_data_col}))
   
  output$Convert_BuildDesign <- renderUI({
    mod_build_design_ui(ns('buildDesign'))
 })

  
  observeEvent(req(rv.convert$design()), {
    r.nav$isDone[4] <- TRUE
  })
  

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
 
