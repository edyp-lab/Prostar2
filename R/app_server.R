options(shiny.maxRequestSize=300*1024^2) 
options(encoding = "UTF-8")
options(shiny.fullstacktrace = TRUE)
require(compiler)
enableJIT(3)


#' @import shiny
#' @importFrom shinyjs hide show
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
    
    tempplot = NULL,
    loadData = NULL
    
  )
  
  observeEvent(input$ReloadProstar, { js$reset()})
  
  observeEvent(input$browser,{
    browser()
  })
  
  
  print('debut de chargement des modules de data manager')
  callModule(mod_open_dataset_server, 'moduleOpenDataset', pipeline.def=reactive({pipeline.defs}))
  # callModule(mod_convert_ms_file_ui, 'moduleProcess_Convert')
   rv.prostar$current.obj <- callModule(mod_open_demo_dataset_server, 'mod_OpenDemoDataset', pipeline.def=reactive({pipeline.defs}))
  
  rv.prostar$settings <- callModule(mod_settings_server, "modSettings")
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
