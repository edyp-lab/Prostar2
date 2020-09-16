Watch_mod_pipe_protein_Normalization <- callModule(module=mod_pipe_protein_Normalization_server,
                                           'mod_pipe_protein_Normalization',  
                                           obj = reactive({rv.core$current.obj}),
                                           ind = reactive({rv.core$current.indice})
                                           )




observeEvent(req(Watch_mod_pipe_protein_Normalization()),{
  
  rv.core$current.obj <- Watch_mod_pipe_protein_Normalization()
  
  # doit avoir le meme nom que celui qui est indique dans la definition des modules pour le pipeline
  ## (fichier pipelineDefinition.R)
   rv.core$current.indice <- length(names(rv.core$current.obj))
  # DeleteDatasetsAfter('moduleProtNormalization')

})