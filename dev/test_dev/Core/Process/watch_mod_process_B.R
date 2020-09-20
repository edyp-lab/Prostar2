mod_process_B_server("mod_process_B", dataIn = reactive({rv.core$current.obj}))
#observeEvent(tmpB(), {rv.core$current.obj <- tmpB()})