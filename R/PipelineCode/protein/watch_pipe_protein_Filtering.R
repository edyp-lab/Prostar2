########
#### Watch code#####

### Pour le module Filtering, comme on doit également mettre a jour la matrice d'adjacence et les composantes
### connexes, on les donne en parametre du module
####################
Watch_mod_pipe_protein_Filtering <- callModule(mod_pipe_protein_Filtering_server,
                                               'moduleProteinFiltering',  
                                                obj = reactive({rv.core$current.obj})
                                       )




observeEvent(req(Watch_mod_pipe_protein_Filtering()),{
  
  browser()
  rv.core$current.obj <- Watch_mod_pipe_protein_Filtering()
  
  ## doit avoir le meme nom que celui qui est indique dans la definition des modules pour le pipeline
  ## (fichier pipelineDefinition.R)
  #rv.core$current.indice <- which(pipeline$current.obj@processes == 'moduleProteinFiltering')
  #DeleteDatasetsAfter('moduleProteinFiltering')
  #rvNav$Done[rv.core$current.indice-1] <- TRUE
  
})