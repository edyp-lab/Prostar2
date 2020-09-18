tmpA <- mod_A_server("mod_A", dataIn = reactive({input$n}))
observeEvent(tmpA(), {rv$current.obj <- tmpA()})