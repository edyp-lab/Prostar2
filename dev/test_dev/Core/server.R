library(DAPAR2)
library(shiny)


source(file.path('../../../R', 'mod_navbar_menu.R'), local=TRUE)$value
dir <- './DataManager'
lapply(list.files(dir, pattern='.R'), 
       function(x) {source(file.path(dir,x), local=FALSE)$value })

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
    current.obj = 0,
    tmp = list(mod1=NULL,
               mod2=NULL,
               mod3=NULL)
  )
   
  
  
  output$activeTab <- renderUI({
    p(paste0('input$navPage = ',input$navPage))
  })
  
  output$currentObj <- renderUI({
    rv.core$current.obj
    p(paste0('rv.core$current.obj = ', rv.core$current.obj))
  })
  
  

  observeEvent(input$addButton, {
    # here, one source the code for modules that there are to be used.
    # This simulates the choice of pipeline in Prostar: we do not source
    # all the modules available in Prostar but only those needed

    dir <- 'Process'
    ll.modules <- c('A', 'B', 'C')
    process.files <- paste0('mod_process_',ll.modules, '.R')
    for (f in process.files)
      source(file.path(dir, f), local = FALSE)$value
    
    tabs <- list(
      tabPanel("mod_process_A", value='mod_process_A', mod_process_A_ui('mod_process_A')),
      tabPanel("mod_process_B", value='mod_process_B', mod_process_B_ui('mod_process_B')),
      tabPanel("mod_process_C", value='mod_process_C', mod_process_C_ui('mod_process_C'))
    )
    
    insertTab(inputId = "navPage",
              do.call(navbarMenu, c('Process' ,tabs)),
              target="manage_modules",
              position="after")
    
    })
  

  
  
  
  observeEvent(input$navPage,{
    
    # Delete the server part of all modules
    remove_all_module_observers(session, pattern='_obs_')

    #if the activePage is a process one, then launch the corresponding server
    if(length(grep('mod_process_', input$navPage)) > 0)
      source(file.path('./Process', paste0('watch_', input$navPage, '.R')), local=TRUE)$value

    
    # Launch the server parts of data sources module
    if('mod_source_1' == input$navPage)
      rv.core$current.obj <- mod_source_1_server('mod_source_1', dataIn = reactive('dataIn_1'))
    
    if('mod_source_2' == input$navPage)
      rv.core$current.obj <- mod_source_2_server('mod_source_2', dataIn = reactive('dataIn_2'))
    
    if('mod_source_3' == input$navPage)
      rv.core$current.obj <- mod_source_3_server('mod_source_3', dataIn = reactive('dataIn_3'))
    
  
    observeEvent(rv.core$tmp$mod1(), {rv.core$current.obj <- rv.core$tmp$mod1()})
    observeEvent(rv.core$tmp$mod2(), {rv.core$current.obj <- rv.core$tmp$mod2()})
    observeEvent(rv.core$tmp$mod3(), {rv.core$current.obj <- rv.core$tmp$mod3()})
    
  })
  
}

