observeEvent(input$ChooseFilters, ignoreInit=TRUE,{
rv.filter$widgets$ChooseFilters <- input$ChooseFilters
})

 observeEvent(input$seuilNA, ignoreInit=TRUE,{
rv.filter$widgets$seuilNA <- input$seuilNA
})

