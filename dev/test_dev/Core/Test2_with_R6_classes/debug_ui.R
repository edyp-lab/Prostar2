output$show_currentPos <- renderUI({
  req(rv$current.pos)
  p(as.character(rv$current.pos))
})

output$show_dataIn <- renderUI({
  req(dataIn())
  tagList(lapply(names(dataIn()), function(x){tags$p(x)}))
})

output$show_rv_dataIn <- renderUI({
  tagList(lapply(names(rv$dataIn), function(x){tags$p(x)}))
  })

output$show_rv_dataOut <- renderUI({
  req(dataOut$trigger)
tagList(
    lapply(names(dataOut$obj), function(x){tags$p(x)})
  )
})


getStringStatus <- function(status){
  if (status==1) "Validated"
  else if (status==0) "Undone"
  else if (status==-1) 'Skipped'
}

output$show_status <- renderUI({
  req(config$status)
  # browser()
  
  tagList(lapply(1:nbSteps(), 
                 function(x){if (x == rv$current.pos) tags$p(tags$b(paste0('-> ',names(config$status)[x], ' - ', getStringStatus(config$status[[x]]))))
                   else tags$p(paste0(names(config$status)[x], ' - ', getStringStatus(config$status[[x]])))
                 }))
})


