
## reactive values for variables in the module
  rv <- reactiveValues(
    name = 'process_Filtering',
    dataIn = NULL,
    dataOut = NULL,
    i = NULL,
    settings = NULL,
    
    widgets = list(ChooseFilters = "None",seuilNA = 0)
  )
