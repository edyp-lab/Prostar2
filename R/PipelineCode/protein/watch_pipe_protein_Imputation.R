########
#### Watch code#####

### Pour le module Filtering, comme on doit également mettre a jour la matrice d'adjacence et les composantes
### connexes, on les donne en parametre du module
####################
Watch_mod_pipe_protein_Imputation <- callModule(module=mod_pipe_protein_Imputation_server,
                                                'moduleProtImputation',  
                                                obj = reactive({rv.core$current.obj})
                                                )




observeEvent(req(Watch_mod_pipe_protein_Imputation()),{
  
  rv.core$current.obj <- WatchmoduleProtImputation()
  
  ## doit avoir le meme nom que celui qui est indique dans la definition des modules pour le pipeline
  ## (fichier pipelineDefinition.R)
  # rv.core$current.indice <- which(rv.core$current.obj@processes == 'moduleProtImputation')
  # DeleteDatasetsAfter('moduleProtImputation')
  # rvNav$Done[rv.core$current.indice-1] <- TRUE
})