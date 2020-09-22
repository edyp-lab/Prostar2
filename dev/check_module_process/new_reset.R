
observeEvent(req(r.nav$reset),{
    
    rv$widgets <- list(ChooseFilters = "None",seuilNA = 0)
    
    ## do not modify this part
    rv$dataIn <- obj()
    rv$i <- indice()
    
    r.nav$isDone <- rep(FALSE, 5)
    r.nav$reset <- FALSE
    ## end of no modifiable part
    
    
  })
