
Watch_mod_pipe_protein_newModule_shinyTest <- mod_pipe_protein_newModule_shinyTest_server('mod_pipe_protein_newModule_shinyTest',  
                                                                                          obj = reactive({rv.core$current.obj}),
                                                                                          indice = reactive({rv.core$current.indice})
)




observeEvent(req(Watch_mod_pipe_protein_newModule_shinyTest()),{
  rv.core$current.obj <- Watch_mod_pipe_protein_newModule_shinyTest()
})
