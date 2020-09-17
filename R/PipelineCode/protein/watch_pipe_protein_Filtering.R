
Watch_mod_pipe_protein_Filtering <- callModule(mod_pipe_protein_Filtering_server,
                                               'mod_pipe_protein_Filtering',  
                                                obj = reactive({rv.core$current.obj}),
                                                indice = reactive({rv.core$current.indice})
                                       )




observeEvent(req(Watch_mod_pipe_protein_Filtering()),{
  rv.core$current.obj <- Watch_mod_pipe_protein_Filtering()
  Update_QF_indice()
})