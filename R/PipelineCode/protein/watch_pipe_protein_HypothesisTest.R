Watch_mod_pipe_protein_HypothesisTest <- mod_pipe_protein_HypothesisTest_server('mod_pipe_protein_HypothesisTest',  
                                                                                obj=reactive({rv.core$current.obj}),
                                                                                indice = reactive({rv.core$current.indice})
)




observeEvent(req(Watch_mod_pipe_protein_HypothesisTest()),{
  rv.core$current.obj <- WatchmoduleProtHypothesisTest()
})
