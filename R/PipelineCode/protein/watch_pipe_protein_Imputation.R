########
#### Watch code#####

### Pour le module Filtering, comme on doit également mettre a jour la matrice d'adjacence et les composantes
### connexes, on les donne en parametre du module
####################
Watch_mod_pipe_protein_Imputation <- callModule(module=mod_pipe_protein_Imputation_server,
                                                'mod_pipe_protein_Imputation',  
                                                obj = reactive({rv.core$current.obj})
                                                )




observeEvent(req(Watch_mod_pipe_protein_Imputation()),{
  
  rv.core$current.obj <- Watch_mod_pipe_protein_Imputation()
  
  ## doit avoir le meme nom que celui qui est indique dans la definition des modules pour le pipeline
  ## (fichier pipelineDefinition.R)
  rv.core$current.indice <- length(names(rv.core$current.obj))
  # DeleteDatasetsAfter('moduleProtImputation')
})