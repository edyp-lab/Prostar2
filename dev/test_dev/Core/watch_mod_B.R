tmpB <- mod_B_server("mod_B", dataIn = reactive({input$n}))
observeEvent(tmpB(), {rv$current.obj <- tmpB()})