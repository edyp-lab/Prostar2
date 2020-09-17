Watch_mod_pipe_protein_Normalization <- callModule(module=mod_pipe_protein_Normalization_server,
                                           'mod_pipe_protein_Normalization',  
                                           obj = reactive({rv.core$current.obj}),
                                           indice = reactive({rv.core$current.indice})
                                           )




observeEvent(req(Watch_mod_pipe_protein_Normalization()),{
  
  rv.core$current.obj <- Watch_mod_pipe_protein_Normalization()
  Update_QF_indice()
})