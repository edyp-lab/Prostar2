tmpC <- mod_process_C_server("mod_process_C", dataIn = reactive({input$n}))
observeEvent(tmpC(), {rv.core$current.obj <- tmpC()})