library(shiny)

ui <- fluidPage(
  titlePanel("Stream the system output"),
  sidebarLayout(
    sidebarPanel(
      actionButton("btn_start",label = "Let's stream"),
      actionButton("btn_stop",label = "Stop")
    ),
    mainPanel(
      htmlOutput("textstream_output")
    )
  )
)
server <- function(input, output, session) {
  cat(NULL,file="sink-steps.txt")
  rv <- reactiveValues(textstream = c(""),
                       timer = reactiveTimer(500),
                       started = FALSE)
  observeEvent(input$btn_start, { 
    rv$started <- TRUE
    #withProgress_appCalcul(connexion=T)
    system2("Rscript", "withProgress_Calcul.R", wait = FALSE)
    #system2("Rscript", "so_script.R", wait = FALSE)
  })
  observeEvent(input$btn_stop, { rv$started <- FALSE })
  observe({
    rv$timer()
    if (isolate(rv$started))
      rv$textstream <- paste(readLines("sink-steps.txt"), collapse = "<br/>")
  })
  output$textstream_output <- renderUI({
    HTML(rv$textstream)
  })
}
shinyApp(ui = ui, server = server)

# foo <- function() {
#   message("one")
#   Sys.sleep(0.5)
#   message("two")
# }
# 
# runApp(shinyApp(
#   ui = fluidPage(
#     shinyjs::useShinyjs(),
#     actionButton("btn","Click me"),
#     textOutput("text")
#   ),
#   server = function(input,output, session) {
#     observeEvent(input$btn, {
#       withCallingHandlers({
#         shinyjs::html("text", "")
#         foo()
#       },
#       message = function(m) {
#         shinyjs::html(id = "text", html = m$message, add = TRUE)
#       })
#     })
#   }
# ))



# ui <- fluidPage(
#   
#   titlePanel("reactivePoll and reactiveFileReader"),
#   fluidRow(
#     column(12,
#            p("This app has a log file which is appended to",
#              "every second.")
#     )
#   ),
#   fluidRow(
#     column(6, wellPanel(
#       "This side uses a reactiveFileReader, which is monitoring",
#       "the log file for changes every 0.5 seconds.",
#       verbatimTextOutput("fileReaderText")
#     )),
#     
#     column(6, wellPanel(
#       "This side uses a reactivePoll, which is monitoring",
#       "the log file for changes every 4 seconds.",
#       verbatimTextOutput("pollText")
#     ))
#   )
# )
# 
# 
# 
# server <- function(input, output, session) {
#   
#   # Create a random name for the log file
#   logfilename <- tempfile('logfile', fileext = '.txt')
#   print(logfilename)
#   
#   # ============================================================
#   # This part of the code writes to the log file every second.
#   # Writing to the file could be done by an external process.
#   # In this example, we'll write to the file from inside the app.
#   logwriter <- observe({
#     # Invalidate this observer every second (1000 milliseconds)
#     invalidateLater(1000, session)
#     
#     # Clear log file if more than 10 entries
#     if (file.exists(logfilename) &&
#         length(readLines(logfilename)) > 10) {
#       unlink(logfilename)
#     }
#     
#     # Add an entry to the log file
#     cat(as.character(Sys.time()), '\n', file = logfilename,
#         append = TRUE)
#   })
#   
#   # When the client ends the session, suspend the observer and
#   # remove the log file.
#   session$onSessionEnded(function() {
#     logwriter$suspend()
#     unlink(logfilename)
#   })
#   
#   # ============================================================
#   # This part of the code monitors the file for changes once per
#   # 0.5 second (500 milliseconds).
#   fileReaderData <- reactiveFileReader(500, session,
#                                        logfilename, readLines)
#   
#   output$fileReaderText <- renderText({
#     # Read the text, and make it a consistent number of lines so
#     # that the output box doesn't grow in height.
#     text <- fileReaderData()
#     length(text) <- 14
#     text[is.na(text)] <- ""
#     paste(text, collapse = '\n')
#   })
# }
# 
# shinyApp(ui, server)
#   