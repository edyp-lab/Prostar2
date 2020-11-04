list(
  config = list(process.name = 'ProcessA',
                steps = c('Description', 'Step1', 'Step2', 'Step3'),
                mandatory = setNames(c(F,F,F,F), c('Description', 'Step1', 'Step2', 'Step3'))
  ),
  RenderUIs = function(input, output){
    ns <- NS(self$id)
    output$Description <- renderUI({
      tagList(
        p(self$id),
        actionButton(ns('change'), "change")
      )})
    
    observeEvent(input$change,{
      self$PrintChange()})
  }
)
