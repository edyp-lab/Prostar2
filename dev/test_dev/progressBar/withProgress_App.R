library(shiny)
source(file.path('.', 'withProgress_Calcul.R'), local=TRUE)$value

ui <- fluidPage(
  plotOutput("plot")
)

server <- function(input, output) {
  
  output$plot <- renderPlot({
    
    N=5
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    
    progress$set(message = "Calculation in progress", value = 0)
    
    withProgress_appCalcul(connexion=T)
    details <- reactiveFileReader(500,
                                  NULL,
                                  "sink-steps.txt",
                                  readLines)
    
    for (i in 1:N) {
      progress$inc(1/N, detail = gsub("^....", "", details()[i] ) )
      #Sys.sleep(5)
    }
    
  })
  
  # session$onSessionEnded(function() {
  #   system(paste("rm -f", ../../inst/app/www/sink-steps.txt))
  # })
  
}

shinyApp(ui, server)

# ##############################################################################
# library(magicfor)
# 
# magic_for(cat)
# 
# for (i in 1:10) {
#   squared <- i ^ 2
#   #put(squared)
#   cat(squared, " ")
#   Sys.sleep(2)
# }
# 
# ##############################################################################

# library(shiny)
# 
# progressBarTimer <- function(top = TRUE) {
#   progressBar <- div(
#     class = "progress progress-striped active",
#     # disable Bootstrap's transitions so we can use jQuery.animate
#     div(class = "progress-bar", style = "-webkit-transition: none !important;
#               transition: none !important;")
#   )
#   
#   containerClass <- "progress-timer-container"
#   
#   if (top) {
#     progressBar <- div(class = "shiny-progress", progressBar)
#     containerClass <- paste(containerClass, "shiny-progress-container")
#   }
#   
#   tagList(
#     tags$head(
#       tags$script(HTML("
#         $(function() {
#           Shiny.addCustomMessageHandler('progress-timer-start', function(message) {
#             var $progress = $('.progress-timer-container');
#             var $bar = $progress.find('.progress-bar');
#             $bar.css('width', '0%');
#             $progress.show();
#             $bar.animate({ width: '100%' }, {
#               duration: message.duration,
#               easing: message.easing,
#               complete: function() {
#                 if (message.autoClose) $progress.fadeOut();
#               }
#             });
#           });
# 
#           Shiny.addCustomMessageHandler('progress-timer-close', function(message) {
#             var $progress = $('.progress-timer-container');
#             $progress.fadeOut();
#           });
#         });
#       "))
#     ),
#     
#     div(class = containerClass, style = "display: none;", progressBar)
#   )
# }
# 
# startProgressTimer <- function(durationMsecs = 2000, easing = c("swing", "linear"),
#                                autoClose = FALSE, session = getDefaultReactiveDomain()) {
#   easing <- match.arg(easing)
#   session$sendCustomMessage("progress-timer-start", list(
#     duration = durationMsecs,
#     easing = easing,
#     autoClose = autoClose
#   ))
# }
# 
# closeProgressTimer <- function(session = getDefaultReactiveDomain()) {
#   session$sendCustomMessage("progress-timer-close", list())
# }
# 
# ui <- fluidPage(
#   numericInput("seconds", "how many seconds your calculation will last?", value = 6),
#   progressBarTimer(top = TRUE),
#   actionButton("go", "Compute")
# )
# 
# server <- function(input, output, session) {
#   observeEvent(input$go, {
#     startProgressTimer(input$seconds * 1000, easing = "swing")
#     Sys.sleep(input$seconds) # simulate computation
#     closeProgressTimer()
#     showNotification("Computation finished!", type = "error")
#   })
# }
# 
# shinyApp(ui, server)