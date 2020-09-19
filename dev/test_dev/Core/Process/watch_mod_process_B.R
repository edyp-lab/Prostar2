tmpB <- mod_process_B_server("mod_process_B", dataIn = reactive({input$n}))
observeEvent(tmpB(), {rv.core$current.obj <- tmpB()})