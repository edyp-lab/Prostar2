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
  
  #Global reactive variables for Prostar-core
  rv.prostar <- reactiveValues(
    obj = NULL,
    settings = NULL
  )
  
  
  ## definition des variables globales liees a un pipeline
  rv.core <- reactiveValues(
    # current working data from current pipeline
    type = NULL,
    
    ## indice du dataset courant dans la liste ll.process.
    current.indice = 1,
    
    ## liste qui contiendra les noms des différents datasets enregsitres au cours 
    ## de l'execution sdu module. Il est initialisé à Original car dans tous les cas,
    ## on démarre avec un dataset intitule original
    ll.process = c('original'),
    
    # object returned by demode, openmode and convertmode
    #object that is used for modules in pipeline. C'est une instance d'une classe Pipeline
    current.obj = NULL,
    tmp_dataManager = reactiveValues(convert = NULL,
                              openFile = NULL,
                              openDemo = NULL),
    
    tempplot = NULL,
    loadData = NULL
    
  )
  
  observeEvent(input$ReloadProstar, { js$reset()})
  
  observeEvent(input$browser,{
    browser()
  })
  
  
  rv.core$tmp_dataManager <- list(openFile = callModule(mod_open_dataset_server, 'moduleOpenDataset', pipeline.def=reactive({pipeline.defs})),
                                  convert = callModule(mod_convert_ms_file_server, 'moduleProcess_Convert', pipeline.def=reactive({pipeline.defs})),
                                  openDemo = callModule(mod_open_demo_dataset_server, 'mod_OpenDemoDataset', pipeline.def=reactive({pipeline.defs}))
                                  )
  
  observeEvent(rv.core$tmp_dataManager$openFile(),{ rv.core$current.obj <- rv.core$tmp_dataManager$openFile()})
  observeEvent(rv.core$tmp_dataManager$convert(),{ rv.core$current.obj <- rv.core$tmp_dataManager$convert()})
  observeEvent(rv.core$tmp_dataManager$openDemo(),{ rv.core$current.obj <- rv.core$tmp_dataManager$openDemo()  })
  
 rv.prostar$settings <- callModule(mod_settings_server, "modSettings", obj=reactive({rv.core$current.obj}))
  
  callModule(mod_all_plots_server, 'modAllPlots', 
             dataIn = reactive({
               req(rv.core$current.obj)
               rv.core$current.obj
             }),
             settings = reactive({rv.prostar$settings()}))
  
  
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

   callModule(mod_bug_report_server, "bugreport")
   callModule(mod_insert_md_server, "links_MD",URL_links)
   callModule(mod_insert_md_server, "FAQ_MD", URL_FAQ)
   
  
  
  #Once the server part is loaded, hide the loading page 
  # and show th main content
  #shinyjs::hide(id = "loading_page", anim = FALSE)
  #shinyjs::show("main_content", anim = TRUE, animType = "fade")

}
