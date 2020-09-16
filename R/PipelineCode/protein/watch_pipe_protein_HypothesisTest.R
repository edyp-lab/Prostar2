Watch_mod_pipe_protein_HypothesisTest <- callModule(module=mod_pipe_protein_HypothesisTest_server,
                                            'mod_pipe_protein_HypothesisTest',  
                                            obj=reactive({rv.core$current.obj}),
                                            ind = reactive({rv.core$current.indice})
                                            )




observeEvent(req(Watch_mod_pipe_protein_HypothesisTest()),{
  
  rv.core$current.obj <- WatchmoduleProtHypothesisTest()
  
  # rv.core$current.obj@res_AllPairwiseComparisons <- WatchmoduleProtHypothesisTest()$res_AllPairwiseComparisons
  # 
  # ## doit avoir le meme nom que celui qui est indique dans la definition des modules pour le pipeline
  # ## (fichier pipelineDefinition.R)
  rv.core$current.indice <- length(names(rv.core$current.obj))
  # DeleteDatasetsAfter('moduleProtHypothesisTest')
})
