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
#' @importFrom shinyjs useShinyjs disabled
mod_import_file_from_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    uiOutput(ns('chooseFileType')),
    uiOutput(ns('chooseFile')),
    uiOutput(ns("ChooseXlsSheets")),
    actionButton(ns('import'), 'Import file')
    
  )
}

# Module Server

#' @rdname mod_import_file_from
#' @export
#' @keywords internal
#' @importFrom DAPAR2 readExcel listSheets
#' @importFrom shinyjs info disabled disable enable

mod_import_file_from_server <- function(id, reset=FALSE){
  
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    options(shiny.maxRequestSize=300*1024^2)
    
    rv.importFrom <- reactiveValues(
      current.extension = NULL,
      current.accepted = NULL,
      extension = list(maxquant = c("txt", "tsv", "csv"),
                       excel = c("xls", "xlsx")
      ),
      out = NULL
    )
    
    
    
    
    output$chooseFileType <- renderUI({
      selectInput(ns("importFileFrom"),
                  "Import from",
                  selected = NULL,
                  width=150,
                  choices = names(list('None' = 'None', 
                                       'Maxquant'='Maxquant', 
                                       'Excel'='Excel')
                  )
      )
    })
    
    
    
    observeEvent(input$importFileFrom,{
      switch(input$importFileFrom,
             Maxquant = rv.importFrom$current.accepted <-  rv.importFrom$extension$maxquant,
             Excel = rv.importFrom$current.accepted <- rv.importFrom$extension$excel
      )
      rv.importFrom$out <- NULL
      #shinyjs::disable('import')
    })
    
    
    output$chooseFile <- renderUI({
      req(input$importFileFrom)
      
      if (input$importFileFrom == "None"){ return(NULL)}
      
      fluidRow(
        column(width=2, 
               mod_popover_for_help_ui(ns("modulePopover_convertChooseDatafile"))
        ),
        column(width = 10, 
               fileInput(ns("file2Convert"), 
                         "", 
                         multiple=FALSE, 
                         accept=rv.importFrom$current.accepted
                         
               )
        )
      )
    })
    
    
    
    observeEvent(input$file2Convert,{
      #shinyjs::disable('import')
      rv.importFrom$out <- NULL
      rv.importFrom$current.extension <-  strsplit(input$file2Convert$name, '.', fixed=TRUE)[[1]][2]
      if( !(rv.importFrom$current.extension %in% rv.importFrom$current.accepted)) {
        shinyjs::info("Warning : this file is not a valid file !
                   Please choose another one.")
        #shinyjs::disable('import')
        return(NULL)
      } 
      # else {
      #   shinyjs::enable('import')
      # }
      
    })
    
    
    output$ChooseXlsSheets <- renderUI({
      req(input$file2Convert)
      req(rv.importFrom$current.extension )
      if (!(rv.importFrom$current.extension %in% rv.importFrom$extension$excel)){
        #shinyjs::disable('import')
        return(NULL)
      }
      
      selectInput(ns("XLSsheets"), 
                  "Select sheet with quant. data", 
                  choices = as.list(DAPAR2::listSheets(input$file2Convert$datapath)),
                  width='200px')
      
      
    })
    
    
    
    ############ Read text file to be imported ######################
    observeEvent(input$import,{
      req(rv.importFrom$current.extension)
      
      tryCatch(
        {
          switch(input$importFileFrom,
                 Excel = if (is.null(input$XLSsheets)) {
                   return(NULL)
                 } else {
                   rv.importFrom$out <- DAPAR2::readExcel(input$file2Convert$datapath, ext, sheet=input$XLSsheets)
                 },
                 Maxquant = rv.importFrom$out <- read.csv(input$file2Convert$datapath,  header=TRUE, sep="\t", as.is=T)
          )
          
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
    
    
  })
  
  
}

## To be copied in the UI
# mod_import_file_from_ui("import_file_from_ui_1")

## To be copied in the server
# callModule(mod_import_file_from_server, "import_file_from_ui_1")

