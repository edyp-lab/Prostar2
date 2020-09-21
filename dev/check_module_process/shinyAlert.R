
observe({
    req(input$shinyalert)
    rv$i
    
    c1 <- input$shinyalert
    c2 <- rv$i == length(rv$dataIn)
    c3 <- r.nav$isDone[length(r.nav$isDone)]
    if (c1 && !c2 && !c3){
      #Delete all assays after that one indicated by the indice given in parameter
      rv$dataIn <- rv$dataIn[ , , -((rv$i+1):length(rv$dataIn))]
      c1 <- input$shinyalert
      c2 <- rv$i == length(rv$dataIn)
      c3 <- r.nav$isDone[length(r.nav$isDone)]
    } else {
      # Do nothing, the module interface is still disabled
    }
    shinyjs::toggleState('div_nav_pipe_process', condition = !c3 && (c1||c2))
  })
