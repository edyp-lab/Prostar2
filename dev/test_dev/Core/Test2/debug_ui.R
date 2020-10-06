output$show_currentPos <- renderUI({
  req(rv$current.pos)
  p(as.character(rv$current.pos))
})
output$show_dataIn <- renderUI({
  tagList(lapply(names(dataIn()), function(x){tags$p(x)}))
})
output$show_rv_dataIn <- renderPrint({names(rv$dataIn)})
output$show_rv_dataOut <- renderUI({
  tagList(
    lapply(names(rv$dataOut), function(x){tags$p(x)})
  )
})
output$show_isDone <- renderUI({
  req(config$isDone)
  tagList(lapply(1:nbSteps(), 
                 function(x){if (x == rv$current.pos) tags$p(tags$b(paste0('-> ',names(config$isDone)[x], ' - ', config$isDone[[x]])))
                   else tags$p(paste0(names(config$isDone)[x], ' - ', config$isDone[[x]]))
                 }))
})

