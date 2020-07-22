options(shiny.maxRequestSize=300*1024^2) 
options(encoding = "UTF-8")
options(shiny.fullstacktrace = TRUE)
require(compiler)
enableJIT(3)


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
    
    ## indice du dataset courant dans la liste ll.process.
    current.indice = 1,
    
     
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
  rv.core$tmp_dataManager <- list(openFile = callModule(mod_open_dataset_server, 'moduleOpenDataset', pipeline.def=reactive({pipeline.defs})),
                                  convert = callModule(mod_convert_ms_file_server, 'moduleProcess_Convert', pipeline.def=reactive({pipeline.defs})),
                                  openDemo = callModule(mod_open_demo_dataset_server, 'mod_OpenDemoDataset', pipeline.def=reactive({pipeline.defs}))
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
    rv.core$current.indice <- 1
    rv.core$current.pipeline <- metadata(rv.core$current.obj)$pipeline
  })
  
  
  # Store the return value of the module settings
  rv.core$settings <- callModule(mod_settings_server, "modSettings", obj=reactive({rv.core$current.obj}))
  
  
  callModule(mod_all_plots_server, 'modAllPlots', 
             dataIn = reactive({
               req(rv.core$current.obj)
               rv.core$current.obj
             }),
             settings = reactive({rv.core$settings()}))
  
  
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
  
  

   
   callModule(mod_homepage_server, "homepage")
   callModule(mod_release_notes_server, "modReleaseNotes")
   callModule(mod_check_updates_server, "modCheckUpdates")

   #callModule(mod_bug_report_server, "bugreport")
   callModule(mod_insert_md_server, "links_MD",URL_links)
   callModule(mod_insert_md_server, "FAQ_MD", URL_FAQ)
   
  
  
  #Once the server part is loaded, hide the loading page 
  # and show th main content
  #shinyjs::hide(id = "loading_page", anim = FALSE)
  #shinyjs::show("main_content", anim = TRUE, animType = "fade")

}
