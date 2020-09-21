
#shinyalert modal asking if user wants to process a dataset with an index <i
  observeEvent(req(rv$dataIn, rv$i ), {
    
    a <- (length(rv$dataIn) != rv$i) && !r.nav$isDone[length(r.nav$isDone)]
    if (!a) return(NULL)
    
    shinyalert::shinyalert(
      title = 'title',
      text = 'This is a modal',
      size = 'xs', 
      closeOnEsc = TRUE,
      closeOnClickOutside = FALSE,
      html = FALSE,
      type = 'info',
      showConfirmButton = TRUE,
      showCancelButton = TRUE,
      confirmButtonText = 'OK',
      confirmButtonCol = '#15A4E6',
      cancelButtonText = 'Cancel',
      timer = 0,
      imageUrl = '',
      animation = FALSE
    )
  })
