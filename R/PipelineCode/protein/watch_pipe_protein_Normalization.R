Watch_mod_pipe_protein_Normalization <- callModule(module=mod_pipe_protein_Normalization_server,
                                           'moduleProtNormalization',  
                                           obj = reactive({rv.core$current.obj}),
                                           ind = reactive({rv.core$current.indice})
                                           )




observeEvent(req(Watch_mod_pipe_protein_Normalization()),{
  
  rv.core$current.obj <- Watch_mod_pipe_protein_Normalization()
  
  # doit avoir le meme nom que celui qui est indique dans la definition des modules pour le pipeline
  ## (fichier pipelineDefinition.R)
  # rv.core$current.indice <- which(rv.core$current.obj@processes == 'moduleProtNormalization')
  # DeleteDatasetsAfter('moduleProtNormalization')
  # rvNav$Done[rv.core$current.indice-1] <- TRUE
})