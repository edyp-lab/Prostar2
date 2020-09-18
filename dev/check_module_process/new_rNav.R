
## Section navigation module
  # Variable to manage the different screens of the module
  r.nav <- reactiveValues(
    name = 'Filtering',
    stepsNames = c('MV filtering', 'Field filtering', 'Validate'),
    ll.UI = list( screenStep1 = uiOutput(ns('Screen_Filtering_1')),
                  screenStep2 = uiOutput(ns('Screen_Filtering_2')),
                  screenStep3 = uiOutput(ns('Screen_Filtering_3'))
    ),
    isDone =  rep(FALSE,3),
    mandatory =  rep(FALSE,3),
    reset = FALSE
  )
