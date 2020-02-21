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
    selectizeInput(ns("importFileFrom"),
                   "Import from",
                   multiple = T,
                   options = list(maxItems = 1),
                   width=150,
                   choices = names(list('Maxquant'='Maxquant', 'Excel'='Excel'))),
    uiOutput(ns('chooseFile')),
    uiOutput(ns("ChooseXlsSheets"))
  )
}
    
# Module Server
    
#' @rdname mod_import_file_from
#' @export
#' @keywords internal
#' @importFrom DAPAR readExcel listSheets
    
mod_import_file_from_server <- function(input, output, session){
  ns <- session$ns
  
  rv.importFrom <- reactiveValues(
    
    out = NULL
  )
  
  
  
  
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
  
  
  
  output$ChooseXlsSheets <- renderUI({
    req(input$file2Convert)
    
    .ext <- GetExtension(input$file2Convert$name)
    if ((.ext == "xls") || (.ext == "xlsx")){
      selectInput(ns("XLSsheets"), 
                  "Select sheet with quant. data", 
                  choices = as.list(DAPAR::listSheets(input$file2Convert$datapath)),
                  width='200px')
    }
    
  })
  
  

   ############ Read text file to be imported ######################
   observeEvent(c(input$file2Convert,input$XLSsheets),{

     input$XLSsheets
     if (((GetExtension(input$file2Convert$name)== "xls")
          || (GetExtension(input$file2Convert$name) == "xlsx") )
         && is.null(input$XLSsheets)) {return(NULL)  }

     authorizedExts <- c("txt", "csv", "tsv", "xls", "xlsx")
     if( is.na(match(GetExtension(input$file2Convert$name), authorizedExts))) {
       shinyjs::info("Warning : this file is not a text nor an Excel file !
                   Please choose another one.")
     }
     else {
       # result = tryCatch(
       #   {
       #ClearUI()
       # ClearMemory()
       ext <- GetExtension(input$file2Convert$name)

       switch(ext,
              txt = { rv.convert$tab1 <- read.csv(input$file2Convert$datapath,  header=TRUE, sep="\t", as.is=T)},
              csv = { rv.convert$tab1 <- read.csv(input$file2Convert$datapath,  header=TRUE, sep="\t", as.is=T)},
              tsv = { rv.convert$tab1 <- read.csv(input$file2Convert$datapath,  header=TRUE, sep="\t", as.is=T)},
              xls = { rv.convert$tab1 <- readExcel(input$file2Convert$datapath, ext, sheet=input$XLSsheets)},
              xlsx = {rv.convert$tab1 <- readExcel(input$file2Convert$datapath, ext, sheet=input$XLSsheets)}
       )
       #   }
       #   , warning = function(w) {
       #     shinyjs::info(conditionMessage(w))
       #   }, error = function(e) {
       #     shinyjs::info(paste("Read text file to convert",":",
       #                         conditionMessage(e),
       #                         sep=" "))
       #   }, finally = {
       #     #cleanup-code
       #   })
     }

   })

  
  
  
  #  
  #  readTextFile <- reactive({
  #    rrv.importFrom$out <- read.csv(input$file2Convert$datapath,  
  #                                header=TRUE, 
  #                                sep="\t", 
  #                                as.is=T)
  #  })
  #  
  #  
  
  
  return(reactive({rv.importFrom$out}))
  
}
    
## To be copied in the UI
# mod_import_file_from_ui("import_file_from_ui_1")
    
## To be copied in the server
# callModule(mod_import_file_from_server, "import_file_from_ui_1")
 
