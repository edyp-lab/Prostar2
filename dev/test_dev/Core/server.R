library(DAPAR2)
library(shiny)

options(shiny.fullstacktrace = F)


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
    current.obj = list(),
    current.indice = 0,
    tmp = NULL,
    tmp_indice = NULL
  )
   
  
  
  # Launch the module which manages the current dataset in the list of current.obj
  rv.core$tmp_indice <- callModule(mod_change_dataset_server, 
                                   'mod_change_dataset', 
                                   ll.se = reactive({names(rv.core$current.obj)}),
                                   indice = reactive({rv.core$current.indice})
  )
  
  observeEvent(rv.core$tmp_indice(),{ rv.core$current.indice <- rv.core$tmp_indice()  })
  
  output$activeTab <- renderUI({
    tags$p(tags$strong(paste0('input$navPage = ',input$navPage)))
  })
  
  output$currentObj <- renderUI({
    rv.core$current.obj

    tagList(
      tags$p(tags$strong('rv.core$current.obj : ')),
      tags$ul(
        lapply(paste0(names(rv.core$current.obj), "=", unlist(rv.core$current.obj)), function(x) tags$li(x))
    )
    )
  })
  
  output$names_Input <- renderUI({
    tagList(
      tags$p(tags$strong('List input = ')),
      tags$ul(
      lapply(names(reactiveValuesToList(input)), function(x) tags$li(x)))
      )
    
  })
  
 observeEvent(c(input$page, rv.core$current.obj), ignoreInit=TRUE, {
   if(length(grep('mod_source_', input$navPage)) == 0)
     return(NULL)

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
  

  observeEvent(input$navPage, ignoreInit = FALSE,{

    # Delete the server part of all modules
    remove_all_module_observers(session, pattern='_obs_')

    #if the activePage is a process one, then launch the corresponding server
    if(length(grep('mod_process_', input$navPage)) > 0)
      rv.core$tmp <- source(file.path('./Process', paste0('watch_', input$navPage, '.R')), local=TRUE)$value
    
    # Launch the server parts of the corresponding data sources module 
    if(length(grep('mod_source_', input$navPage)) > 0)
      rv.core$tmp <- do.call(paste0(input$navPage, '_server'), 
                             list(id=input$navPage, 
                                  params = reactive({input$navPage})
                                  )
                             )

    observeEvent(rv.core$tmp(), {rv.core$current.obj <- rv.core$tmp()})
    
  })
  
}

