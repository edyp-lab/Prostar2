
Watch_mod_pipe_protein_Imputation <- callModule(module=mod_pipe_protein_Imputation_server,
                                                'mod_pipe_protein_Imputation',  
                                                obj = reactive({rv.core$current.obj}),
                                                indice = reactive({rv.core$current.indice})
                                                )




observeEvent(req(Watch_mod_pipe_protein_Imputation()),{
  rv.core$current.obj <- Watch_mod_pipe_protein_Imputation()
})