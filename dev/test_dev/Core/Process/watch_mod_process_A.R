tmpA <- mod_process_A_server("mod_process_A", dataIn = reactive({input$n}))
observeEvent(tmpA(), {rv.core$current.obj <- tmpA()})