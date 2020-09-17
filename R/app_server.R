options(shiny.maxRequestSize=300*1024^2) 
options(encoding = "UTF-8")
options(shiny.fullstacktrace = T)
require(compiler)
enableJIT(3)


lapply(list.files('R/DataManager/', pattern='.R'), 
       function(x) {source(file.path('R/DataManager',x), local=TRUE)$value })

lapply(list.files('R/Plots/', pattern='.R'), 
       function(x) {source(file.path('R/Plots', x), local=TRUE)$value })


#' @import shiny
#' @importFrom shinyjs hide show
#' 
app_server <- function(input, output,session) {
  
  
  
  
  
  # List the first level callModules here
  
  #callModule(mod_loading_page_server,'loadingPage')
  #callModule(mod_navbar_menu_server,'mainMenu')
  
 
  
  
  ## definition des variables globales liees a un pipeline
  rv.core <- reactiveValues(
    # current working data from current pipeline
    type = NULL,
    
    # Current QFeatures object in Prostar
    current.obj = NULL,
    
    ## indice of the current assay in current.obj, corresponding to a step in the pipeline
    current.indice = 1,
    
    # pipeline choosen by the user for its dataset
    current.pipeline = NULL,
    
    
    # objects returned by demode, openmode and convertmode
    tmp_dataManager = reactiveValues(convert = NULL,
                              openFile = NULL,
                              openDemo = NULL),
    
    # return value of the settings module
    settings = NULL,
    
    #
    tempplot = NULL,
    
    #
    loadData = NULL
    
  )
  
  observeEvent(input$ReloadProstar, { js$reset()})
  
  observeEvent(input$browser,{browser() })
  
  
  ## Get the return values of modules in charge of loading datasets
  rv.core$tmp_dataManager <- list(openFile = callModule(mod_open_dataset_server, 'moduleOpenDataset', 
                                                        pipeline.def=reactive({pipeline.defs})),
                                  convert = callModule(mod_convert_ms_file_server, 'moduleProcess_Convert', 
                                                       pipeline.def=reactive({pipeline.defs})),
                                  openDemo = callModule(mod_open_demo_dataset_server, 'mod_OpenDemoDataset', 
                                                        pipeline.def=reactive({pipeline.defs}))
                                  )
  
  observeEvent(rv.core$tmp_dataManager$openFile(),{ 
    rv.core$current.obj <- rv.core$tmp_dataManager$openFile()
    setCoreRV()
  })
  
  observeEvent(rv.core$tmp_dataManager$convert(),{ 
    rv.core$current.obj <- rv.core$tmp_dataManager$convert()
    setCoreRV()
    })
  
  
  observeEvent(rv.core$tmp_dataManager$openDemo(),{
    rv.core$current.obj <- rv.core$tmp_dataManager$openDemo()
    setCoreRV()
    })
  
  
  
  # Update several reactive variable once a dataset is loaded
  setCoreRV <- reactive({
    # We begins with the last SE in the QFeatures dataset
    rv.core$current.indice <- length(names(rv.core$current.obj))
    rv.core$current.pipeline <- metadata(rv.core$current.obj)$pipelineType
  })
  
  
  # Store the return value of the module settings
  rv.core$settings <- callModule(mod_settings_server, "modSettings", obj=reactive({rv.core$current.obj}))
  
  
  
  
  
  callModule(mod_infos_dataset_server, 
             'infos_demoDataset', 
             obj = reactive({
               req(rv.core$current.obj)
               rv.core$current.obj
             })
  )
  
  callModule(mod_infos_dataset_server, 
             'infos_openFile', 
             obj = reactive({
               req(rv.core$current.obj)
               rv.core$current.obj
             })
  )
  
  rv.core$tmp_indice <- callModule(mod_change_assay_server, 
             'change_assay', 
             ll.se = reactive({names(rv.core$current.obj)}),
             indice = reactive({rv.core$current.indice})
  )
  
  observeEvent(rv.core$tmp_indice(),{
    rv.core$current.indice <- rv.core$tmp_indice()
  })

   
   callModule(mod_homepage_server, "homepage")
   callModule(mod_release_notes_server, "modReleaseNotes")
   callModule(mod_check_updates_server, "modCheckUpdates")

   #callModule(mod_bug_report_server, "bugreport")
   callModule(mod_insert_md_server, "links_MD",URL_links)
   callModule(mod_insert_md_server, "FAQ_MD", URL_FAQ)
   
  
   #Once the type of pipeline is known (ie a dataset has been loaded),
   #call the server parts of the processing modules that belongs
   # to this pipeline
   observeEvent(req(rv.core$current.pipeline), {
     
     Build_DataMining_Menu()
     Build_DataProcessing_Menu()
   })
   
   

   # Builds the menu for data mining tools. This menu is dependent of the type of dataset.
   # This is why it is built dynamically
   Build_DataMining_Menu <- reactive({
    req(rv.core$current.obj)
     
     callModule(mod_all_plots_server, 'modAllPlots', 
                dataIn = reactive({
                  req(rv.core$current.obj)
                  rv.core$current.obj
                }),
                indice = reactive({rv.core$current.indice}),
                settings = reactive({rv.core$settings()}))
     
     # Default item is Descriptive statistics for every pipeline
     tabs <- list(
       tabPanel("Descriptive statistics", value='descriptiveStats', mod_all_plots_ui('modAllPlots'))
     )
     
     
      if (metadata(rv.core$current.obj)$pipelineType == 'peptide'){
     # callModule(module = mod_graph_pept_prot_server, "CC_Multi_Any",
     #            obj = reactive({rv.core$current.obj})
     #             )
     # 
     # 
     #   tabs <- append(tabs,
     #                tabPanel("Graph pept-prot", value='graphCC', mod_graph_pept_prot_ui('CC_Multi_Any'))
     #                )
      }
     
     # Add the data mining tab to the header menu of Prostar
     insertTab(inputId = "navPage",
               do.call(navbarMenu, c('Data mining' ,tabs)),
               target="Data manager",
               position="after")
   })
   
   
   
   ## Builds the menu for data processing tools
   Build_DataProcessing_Menu <- reactive({
     req(rv.core$current.pipeline)
     
     ## Get list des process du pipeline
     proc <- pipeline.defs[[rv.core$current.pipeline]]
     dir <- paste0('R/PipelineCode/',rv.core$current.pipeline)
   
     process.files <- paste0('mod_pipe_',rv.core$current.pipeline, '_', proc, '.R')
     
     ## Use here a for loop instead of a lapply, otherwise the functions are not
     ## correctly instantiated in environment
     #Get all code for processing tools
     for (f in process.files)
       source(file.path(dir, f), local = T)
     
     # Loads all server parts of the processing modules
     watchcode.files <- paste0('watch_pipe_',rv.core$current.pipeline, '_', proc, '.R')
     for (f in watchcode.files)
       source(file.path(dir, f), local = T)
     
     ll.modules <- paste0('mod_pipe_', rv.core$current.pipeline, '_', proc)
     
     tabs <- lapply(proc, function(x){
       do.call(tabPanel, list(title=x,
                              value=x,
                              do.call(paste0('mod_pipe_', rv.core$current.pipeline, '_', x, '_ui'), 
                                      list(paste0('mod_pipe_', rv.core$current.pipeline, '_', x)))
                              )
               )
     })
     
     # Add the data mining tab to the header menu of Prostar
     insertTab(inputId = "navPage",
               do.call(navbarMenu, c('Data processing' ,tabs)),
               target="Data manager",
               position="after")
   })
   
   
   
   
   # Update_QF_indice <- function(){
   #   nb_SE_diff <- length(names(rv.core$current.obj)) - rv.core$current.indice
   #   if ( nb_SE_diff > 1  ){
   #     to_keep <- c(1:rv.core$current.indice, length(names(rv.core$current.obj)))
   #     rv.core$current.obj[ , ,to_keep]
   #   }
   #   
   #   rv.core$current.indice <- length(names(rv.core$current.obj))
   # }
   # 
   
   
   
   
   # GetScreenId <- reactive({
   #   input$navPage
   #   req(rv.core$current.obj)
   #   
   #   screen <- NULL
   #   m <-  which(names(rv.core$current.obj@datasets)==input$navPage)
   #   n <-  which(unlist(lapply(GetCurrentMSnSet(), function(x) length(which(x==rv.core$current.obj@datasets))))==1)
   #   ## test if the navPage is one of a process one
   #   if (length(m) ==0 || length(n) ==0) {return(NULL)}
   #   
   #   if (m >= n) { screen <- 'Initial screen'}
   #   else {screen <- 'Final screen'}
   #   print(paste0("in GetScreenId(), n = ", n, ", m = ", m, ". screen = ", screen))
   #   screen
   # })
   
   
   
   
   
  
  #Once the server part is loaded, hide the loading page 
  # and show th main content
  shinyjs::hide(id = "loading_page", anim = FALSE)
  shinyjs::show("main_content", anim = TRUE, animType = "fade")

}
