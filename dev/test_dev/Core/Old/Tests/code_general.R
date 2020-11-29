
InitScreens <- reactive({
  # initialisation of the screens
  for (i in 1:length(rv.process_config$stepsNames))
    rv$screens[[i]] <- if (i == tl.update$current.pos) 
      div(id = ns(paste0("screen", i)),  rv$screens[[i]])
  else  
    shinyjs::hidden(div(id = ns(paste0("screen", i)),  rv$screens[[i]]))
  rv$screens
  })

CreateScreens <- reactive({
  rv$screens <- lapply(1:length(rv.process_config$stepsNames), function(x){
    do.call(uiOutput, list(outputId=ns(paste0("screen", x))))})
  rv$screens
})

ReinitScreens <- reactive({
  lapply(1:length(rv.process_config$stepsNames), 
         function(x){
           shinyjs::enable(paste0('screen', x))
           shinyjs::reset(paste0('screen', x))
         })
  rv.process_config$isDone <- c(TRUE, rep(FALSE, size()-1))
  tl.update$current.pos <- 1
})


output$show_dataIn <- renderPrint({dataIn()})
output$show_rv_dataIn <- renderPrint({rv$dataIn})
output$show_rv_dataOut <- renderPrint({rv$dataOut})
output$show_screens <- renderUI({tagList(rv$screens)})


navPage <- function(direction) {
  newval <- tl.update$current.pos + direction 
  newval <- max(1, newval)
  newval <- min(newval, length(rv.process_config$stepsNames))
  tl.update$current.pos <- newval
}
observeEvent(pos$prevBtn(), ignoreInit = TRUE, {navPage(-1)})
observeEvent(pos$nextBtn(), ignoreInit = TRUE, {navPage(1)})




DisableAllPrevSteps <- reactive({
  pos <- max(grep(TRUE, rv.process_config$isDone))
  lapply(1:pos, 
         function(x){ shinyjs::disable(paste0('screen', x))})
  
})

DisableAllSteps <- reactive({
  lapply(1:length(rv.process_config$isDone), 
         function(x){ shinyjs::disable(paste0('screen', x))})
  
})


DisplayCurrentStep <- reactive({
  lapply(1:length(rv.process_config$stepsNames), 
         function(x){shinyjs::toggle(paste0('screen', x),
                                     condition = x==tl.update$current.pos )}) 
})


HideAllSteps <- reactive({
  lapply(1:length(rv.process_config$stepsNames), 
         function(x){shinyjs::toggle(paste0('screen', x),
                                     condition = F)}) 
})

condNextBtn <- reactive({
  
  # # Conditional enabling of the next button
  end_of_tl <- tl.update$current.pos == length(rv.process_config$stepsNames)
  mandatory_step <- isTRUE(rv.process_config$mandatory[tl.update$current.pos])
  validated <- isTRUE(rv.process_config$isDone[tl.update$current.pos])
  cond.next.btn <-  !mandatory_step || validated
  cond.next.btn
})

condPrevBtn <- reactive({
  pos$skipBtn()
  start_of_tl <- tl.update$current.pos == 1
  cond.prev.btn <- !start_of_tl
  cond.prev.btn
})

size <- reactive({
  req(rv.process_config$stepsNames)
  length(rv.process_config$stepsNames)
})
