library(DAPAR2)
library(shiny)

options(shiny.fullstacktrace = T)


# Laucnh minimal necessary code to start the core server
dir <- './DataManager'
lapply(list.files(dir, pattern='.R'), 
       function(x) {source(file.path(dir,x), local=FALSE)$value })

#source(file.path('.', 'mod_change_dataset.R'), local=FALSE)$value


remove_shiny_inputs <- function(id, .input) {
  invisible(
    lapply(grep(id, names(.input), value = TRUE), function(i) {
      .subset2(.input, "impl")$.values$remove(i)
    })
  )
}


remove_all_module_observers <- function(session, pattern=''){
  cnames <- names(session$userData)
  ind <- grep(pattern, names(session$userData))
  lapply(cnames, function(x) {session$userData[[x]]$destroy()})
} 





# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  rv.core <- reactiveValues(
    type = NULL,
    current.obj = list(),
    current.indice = 0,
    tmp = reactive(NULL),
    load = NULL,
    load.tmp = reactive(NULL),
    current_wf_menu = NULL,
    current.workflow = NULL
  )
   
  
  
  # Just for the show absolutePanel
  output$activeTab <- renderUI({
    tags$p(paste0('input$navPage = ',input$navPage))
  })
  
  output$currentIndice <- renderUI({
    tags$p(paste0('rv.core$current.indice = ',rv.core$current.indice))
  })
  
  # Just for the show absolutePanel
  output$currentObj <- renderUI({
    req(length(rv.core$current.obj) > 0)

    tagList(
      tags$p('rv.core$current.obj : '),
      tags$ul(
        lapply(paste0(names(rv.core$current.obj), "=", unlist(rv.core$current.obj)), 
               function(x) tags$li(x))
    )
    )
  })
  
  # Just for the show absolutePanel
  output$names_Input <- renderUI({
    tagList(
      tags$p('List input = '),
      tags$ul(
      lapply(names(reactiveValuesToList(input)), function(x) tags$li(x)))
      )
    
  })
  

  # On a new page, on launch the corresponding server module
  observeEvent(input$navPage, ignoreInit = FALSE,{
    print('in observeEvent on input$navPage')
    # Delete the server part of all modules
    #remove_shiny_inputs(input$navPage, input)
    remove_all_module_observers(session, pattern='_obs_')
    
    #if the activePage is a process one, then launch the corresponding server
    # to ensure there is only one server at a time
    if(length(grep('wf_', input$navPage)) > 0)
      Launch_WF_server()

  
    # Launch the server parts of the corresponding data sources module 
    if(length(grep('datamanager_', input$navPage)) > 0)
      Launch_DataManager_server()

  })
    
  
  Launch_DataManager_server <- reactive({
    rv.core$load.tmp <- do.call(paste0(input$navPage, '_server'), 
                                list(id = input$navPage, 
                                     params = reactive({input$navPage})
                                    )
                                )
  })
  
  
  Launch_WF_server <- reactive({
    
    dir <- paste0('./Workflows/', rv.core$current.workflow)
    rv.core$tmp <- source(file.path(dir,
                                    paste0('watch_', input$navPage, '.R')), local=TRUE)$value
  })
 
  #Listen if a new dataset has been launched
  # the variable rv.core$tmp() is linked to the datamanager server modules
  observeEvent(rv.core$tmp(), ignoreInit=TRUE, ignoreNULL = T, {
   
    rv.core$current.obj <- rv.core$tmp()
    
    # By default, the last item in the dataset is the current one.
    # So, needs to update the current indice to point the last item
    rv.core$current.indice <- rv.core$current.indice + 1
  })
    
    # A new dataset has been loaded in Prostar
    # Creation of the ui parts of process modules
  observeEvent(rv.core$load.tmp(), ignoreInit = TRUE, {
    #Creation of the UI part of the process modules
    # here, one source the code for modules that there are to be used.
    # This simulates the choice of pipeline in Prostar: we do not source
    # all the modules available in Prostar but only those needed
    glue::glue('observeEvent(rv.core$load.tmp()')
      
      # Initialize the reactive value
      rv.core$current.obj <- rv.core$load.tmp()$obj
      rv.core$current.workflow <- rv.core$load.tmp()$workflow
      rv.core$current.indice <- 1
      
      Build_WF_Menu()
      
  })
    
    
    
    Build_WF_Menu <- reactive({
    
      # Remove previous workflow menu
      removeTab(inputId = 'navPage', target = rv.core$current_wf_menu)
      rv.core$current_wf_menu <- NULL
      
      #browser()
      dir <- paste0('Workflows/', rv.core$current.workflow)
      ll.process <- c('A', 'B', 'C')
      wf.files <- paste0('mod_wf_',rv.core$current.workflow,'_',ll.process, '.R')
      for (f in wf.files)
        source(file.path(dir, f), local = FALSE)$value
      
      # tabs <- list(
      #   tabPanel("mod_wf_A", value='mod_wf_A', mod_wf_A_ui('mod_wf_A')),
      #   tabPanel("mod_wf_B", value='mod_wf_B', mod_wf_B_ui('mod_wf_B')),
      #   tabPanel("mod_wf_C", value='mod_wf_C', mod_wf_C_ui('mod_wf_C'))
      # )
      
      # Dynamic creation of the wf UIs
      tabs <- lapply(ll.process, function(x){
        do.call(tabPanel, list(title=x,
                               value = paste0('mod_wf_', rv.core$current.workflow,'_',x),
                               do.call(paste0('mod_wf_', rv.core$current.workflow, '_', x, '_ui'), 
                                       list(paste0('mod_wf_', rv.core$current.workflow, '_', x)))
        )
        )
      })
      

      rv.core$current_wf_menu <- paste0('Menu for ', rv.core$current.workflow)
      insertTab(inputId = "navPage",
                do.call(navbarMenu, c(rv.core$current_wf_menu ,tabs)),
                target = 'data_manager',
                position=  "after")
      
    })
}

