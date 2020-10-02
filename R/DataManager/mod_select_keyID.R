#' select_keyID UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom Biobase fData
#' @importFrom tibble as_tibble
#' 
mod_select_keyID_ui <- function(id){
  ns <- NS(id)
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
}

#' select_keyID Server Function
#'
#' @noRd 
mod_select_keyID_server <- function(id, dataIn, typeOfData){
  
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    
    rv <- reactiveValues(
      dataOut = NULL,
      out = NULL,
      typeOfData = NULL
    )
    
    observe({
      req(dataIn())
      rv$dataOut <- dataIn() 
      rv$typeOfData <-typeOfData()
    })
    
    mod_popover_for_help_server("modulePopover_convertIdType", 
                                data = list(title = HTML(paste0("<strong><font size=\"4\">Key ID definition</font></strong>")), 
                                            content="If you choose the automatic ID, Prostar will build an index."))
    
    
    
    output$choose_keyID_ui <- renderUI({
      req(rv$dataOut)
      isolate({
        .choices <- c("", "AutoID",colnames(rv$dataOut))
        names(.choices) <- c("None","-- Auto ID --",colnames(rv$dataOut))
        
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
        dat <- tibble::as_tibble(rv$dataOut)[ ,input$choose_keyID]
        t <- (length(dat) == length(unique(dat)))
        
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
    
    
    mod_popover_for_help_server("parentProtein", 
                                data = list(title = HTML(paste0("<strong><font size=\"4\">Select col for parent protein IDs</font></strong>")), 
                                            content="Select the column containing the parent protein IDs."))
    
    
    
    output$choose_col_Parent_Protein_ui <- renderUI({
      req(rv$typeOfData)
      if (rv$typeOfData != 'peptide') { return(NULL)}
      
      isolate({
        .choices <- c("",colnames(rv$dataOut))
        names(.choices) <- c("",colnames(rv$dataOut))
        tagList(
          mod_popover_for_help_ui(ns("parentProtein")),
          selectInput(ns("choose_col_Parent_Protein"),"",choices =  .choices , selected = character(0))
        )
      })
    })
    
    
    output$preview_col_Parent_Protein_ui <- renderTable({
      req(input$choose_col_Parent_Protein)
      
      if (rv$typeOfData != 'peptide' || is.null(input$choose_col_Parent_Protein) || input$choose_col_Parent_Protein == "") {
        return (NULL)
      } else{
        head(rv$dataOut[ ,input$choose_col_Parent_Protein])
      }
      #,colnames=FALSE
      
      
    })
    
    output$note_col_Parent_Protein_ui <- renderUI({
      req(rv$typeOfData)
      if (rv$typeOfData != 'peptide') { return(NULL)}
      
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
      req(rv$typeOfData)
      if (rv$typeOfData != 'peptide') { return(NULL)}
      req(input$choose_col_Parent_Protein)
      
      index <- which(is.na(rv$dataOut[,input$choose_col_Parent_Protein]))
      
      
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
      index <- which(is.na(rv$dataOut[,input$choose_col_Parent_Protein]))
      rv$dataOut <- rv$dataOut[-index,]
    })
    
    
    
    observe({
      req(rv$dataOut)
      req(input$choose_keyID)
      test_keyID <- test_parentProt <- TRUE
      
      if(rv$typeOfData == "peptide"){
        test_parentProt <- !(input$choose_col_Parent_Protein == "") && !is.null(input$choose_col_Parent_Protein) && isTRUE(input$confirm_separator)
      }
      
      
      if (input$choose_keyID =="AutoID") {
        test_keyID <- TRUE
      } else {
        test_keyID <- (length(tibble::as_tibble(rv$dataOut)[, input$choose_keyID])
                       == length(unique(tibble::as_tibble(rv$dataOut)[, input$choose_keyID])))
      }
      
      
      if (isTRUE(test_keyID && test_parentProt)){
        rv$out <- list(keyId = input$choose_keyID,
                       parentProtId = input$choose_col_Parent_Protein, 
                       data = rv$dataOut)
      } else {
        rv$out <- NULL
      }
      
      
    })
    
    return(reactive({rv$out}))
    
    
  })
  
  
}

## To be copied in the UI
# mod_select_keyID_ui("select_keyID_ui_1")

## To be copied in the server
# callModule(mod_select_keyID_server, "select_keyID_ui_1")

