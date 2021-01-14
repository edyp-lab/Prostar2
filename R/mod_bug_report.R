# Module UI

#' @title   mod_bug_report_ui and mod_bug_report_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_bug_report
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_bug_report_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$p("If you encounter an issue with Prostar, you can send an email to the maintainer so he will have sufficient informations
                  to start identify the problem. In addition, you can add the MSnset as an attachment to the mail."),
    tags$br(),
    uiOutput(ns("BugReport_output")),
    tags$br(),
    tags$head(tags$style("#fileReaderText{font-size:12px; font-style:italic;overflow-y:scroll; max-height: 400px; background: ghostwhite;}")),
    verbatimTextOutput(ns("fileReaderText"))
  )
}

# Module Server

#' @rdname mod_bug_report
#' @export
#' @keywords internal

mod_bug_report_server <- function(id){
  
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    logfile <- tempfile(fileext=".log")
    
    if (isTRUE(getOption('golem.app.prod'))){
      print(logfile)
      con <- file(logfile, open="wt")
      sink(con, append=TRUE)
      sink(con, append=TRUE, type="message")
    } else {
      sink()
    }
    
    # ============================================================
    # This part of the code monitors the file for changes once per
    # 0.5 second (500 milliseconds).
    fileReaderData <- reactiveFileReader(500, session, logfile, readLines)
    
    output$fileReaderText <- renderText({
      # Read the text, and make it a consistent number of lines so
      # that the output box doesn't grow in height.
      text <- fileReaderData()
      paste(text, collapse = '\n')
    })
    
    
    
    output$BugReport_output <- renderUI({
      
      mail <- unlist(strsplit(maintainer("Prostar2"), "<"))[2]
      mail <- unlist(strsplit(mail, ">"))[1]
      
      tagList(
        a(actionButton(inputId = ns("email1"), label = "Contact maintainer", 
                       icon = icon("envelope", lib = "font-awesome"), class = actionBtnClass),
          href=paste0("mailto:", mail,"?subject=[Prostar2 bug report]&body=")
        )
      ) 
      
    })
    
    
  })
  
  
  
}

## To be copied in the UI
# mod_bug_report_ui("bug_report_ui_1")

## To be copied in the server
# callModule(mod_bug_report_server, "bug_report_ui_1")

