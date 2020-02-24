# Module UI
  
#' @title   mod_import_file_from_ui and mod_import_file_from_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_import_file_from
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_import_file_from_ui <- function(id){
  ns <- NS(id)
  tagList(
    useShinyjs(),
    uiOutput(ns('chooseFileType')),
    uiOutput(ns('chooseFile')),
    uiOutput(ns("ChooseXlsSheets"))
  )
}
    
# Module Server
    
#' @rdname mod_import_file_from
#' @export
#' @keywords internal
#' @importFrom DAPAR readExcel listSheets
#' @importFrom shinyjs info
    
mod_import_file_from_server <- function(input, output, session){
  ns <- session$ns
  options(shiny.maxRequestSize=300*1024^2)
  rv.importFrom <- reactiveValues(
    extension = NULL,
    out = NULL,
    params = list(
      typeOfFile = 'None'
    )
    
  )
  
  
  observeEvent(input$importFileFrom,{
    rv.importFrom$typeOfFile <- input$importFileFrom
  })
  
  
  output$chooseFileType <- renderUI({
    selectInput(ns("importFileFrom"),
                "Import from",
                selected = rv.importFrom$typeOfFile,
                width=150,
                choices = names(list('None' = 'None', 'Maxquant (txt)'='Maxquant', 'Excel'='Excel'))
    )
  })
  
  output$chooseFile <- renderUI({
    req(input$importFileFrom)
    fluidRow(
      column(width=2, 
             mod_popover_for_help_ui(ns("modulePopover_convertChooseDatafile"))
             ),
      column(width = 10, 
             fileInput(ns("file2Convert"), 
                       "", 
                       multiple=FALSE, 
                       accept=c(".txt", ".tsv", ".csv",".xls", ".xlsx"))
             )
      )
  })
  
  observeEvent(req(input$file2Convert),{
    authorizedExts <- c("txt", "csv", "tsv", "xls", "xlsx")
    .ext <- strsplit(input$file2Convert$name, '.', fixed=TRUE)[[1]][2]
    if( is.na(match(.ext, authorizedExts))) {
      shinyjs::info("Warning : this file is not a text nor an Excel file !
                   Please choose another one.")
      return(NULL)
    } else {
    rv.importFrom$extension <- .ext
    }
  })
  
  output$ChooseXlsSheets <- renderUI({
    req(input$file2Convert)
    req(rv.importFrom$extension )
    if (!(rv.importFrom$extension %in% c("xls","xlsx"))){
      return(NULL)
      }
    
    
      selectInput(ns("XLSsheets"), 
                  "Select sheet with quant. data", 
                  choices = as.list(DAPAR::listSheets(input$file2Convert$datapath)),
                  width='200px')

    
  })
  
  

   ############ Read text file to be imported ######################
   observeEvent(c(input$file2Convert,input$XLSsheets),{
    req(rv.importFrom$extension)
      
     tryCatch(
       {
       if (rv.importFrom$extension %in% c("xls","xlsx")){
                if (is.null(input$XLSsheets)) {
                      return(NULL)
                } else {
                    rv.importFrom$out <- readExcel(input$file2Convert$datapath, ext, sheet=input$XLSsheets)
                } 
       } else {
         rv.importFrom$out <- read.csv(input$file2Convert$datapath,  header=TRUE, sep="\t", as.is=T)
       }
        },
       warning = function(w) {
           shinyjs::info(conditionMessage(w))
         }, 
       error = function(e) {
           shinyjs::info(paste("Read text file to convert",":",
                               conditionMessage(e),
                               sep=" "))
         }, 
       finally = {
           #cleanup-code
         }
       )


   })

  
  
  return(reactive({rv.importFrom$out}))
  
}
    
## To be copied in the UI
# mod_import_file_from_ui("import_file_from_ui_1")
    
## To be copied in the server
# callModule(mod_import_file_from_server, "import_file_from_ui_1")
 
