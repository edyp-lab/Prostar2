tmpC <- mod_C_server("mod_C", dataIn = reactive({input$n}))
observeEvent(tmpC(), {rv$current.obj <- tmpC()})