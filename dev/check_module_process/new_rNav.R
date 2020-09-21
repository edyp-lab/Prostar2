
## Section navigation module
  # Variable to manage the different screens of the module
  r.nav <- reactiveValues(
    name = 'plop',
    stepsNames = c('screen1', 'table2', 'ok', 'toto', 'titi'),
    ll.UI = list(screenStep1 = uiOutput(ns('Screen_plop_1')),
screenStep2 = uiOutput(ns('Screen_plop_2')),
screenStep3 = uiOutput(ns('Screen_plop_3')),
screenStep4 = uiOutput(ns('Screen_plop_4')),
screenStep5 = uiOutput(ns('Screen_plop_5'))),
    isDone =  rep(FALSE,5),
    mandatory =  rep(FALSE,5),
    reset = FALSE
  )
