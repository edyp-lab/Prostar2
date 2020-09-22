summaryUI <- function(id) {
  tagList(
    outputText(ns(id, "min")),
    outputText(ns(id, "mean")),
    outputText(ns(id, "max")),
  )
}
summaryServer <- function(id, var) {
  stopifnot(is.reactive(var))
  
  moduleServer(id, function(input, output, session) {
    range_val <- reactive(range(var(), na.rm = TRUE))
    output$min <- renderText(range_val()[[1]])
    output$max <- renderText(range_val()[[2]])
    output$mean <- renderText(mean(var()))
  })
}