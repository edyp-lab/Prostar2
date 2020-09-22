observeEvent(input$ChooseFilters, ignoreInit=TRUE,{
rv.filter$widgets$ChooseFilters <- input$ChooseFilters
})

 observeEvent(input$plop1, ignoreInit=TRUE,{
rv.filter$widgets$plop1 <- input$plop1
})

 observeEvent(input$seuilNA, ignoreInit=TRUE,{
rv.filter$widgets$seuilNA <- input$seuilNA
})

