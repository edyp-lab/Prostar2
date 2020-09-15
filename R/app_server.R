options(shiny.maxRequestSize=300*1024^2) 
options(encoding = "UTF-8")
options(shiny.fullstacktrace = T)
require(compiler)
enableJIT(3)


lapply(list.files('R/DataManager/', pattern='.R'), 
       function(x) {source(file.path('R/DataManager',x), local=TRUE)$value })

lapply(list.files('R/Plots/', pattern='.R'), 
       function(x) {source(file.path('R/Plots', x), local=TRUE)$value })

#source(file.path('R/Plots', 'mod_homepage.R'), local=TRUE)$value
#files <- list.files('R/DataManager', pattern='.R')
# files <- files[-which(files=='app_server.R')]
# files <- files[-which(files=='app_ui.R')]
# 
# for (f in files)
#   source(file.path('R',f), local=TRUE)$value

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
    rv.core$current.indice <- 1
    rv.core$current.pipeline <- metadata(rv.core$current.obj)$pipelineType
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
   
  
   
   
   
   
   
   
   #Once the type of pipeline is known (ie a dataset has been loaded),
   #call the server parts of the processing modules that belongs
   # to this pipeline
   observeEvent(req(rv.core$current.pipeline), {
     
     
     ## Get list des process du pipeline
     proc <- pipeline.defs[[rv.core$current.pipeline]]
     dir <- paste0('R/PipelineCode',rv.core$current.pipeline)
     process.files <- list.files(dir)[grep('mod_pipe', list.files(dir))]
     lapply(process.files, function(x) {source(file.path(dir,x), local=F)$value })
       
     watchcode.files <- list.files(dir)[grep('watch_', list.files(dir))]
     lapply(watchcode.files, function(x) {source(file.path(dir,x), local=F)$value })
     
     
     # BuildSidebarMenu()
     # # Load UI code for modules
     # rvNav$Done = rep(FALSE,length(def))
     # rvNav$def = list(name = type.pipeline,
     #                  stepsNames = def,
     #                  isMandatory = rep(TRUE,length(def)),
     #                  ll.UI = LoadModulesUI(def)
     #                  )
     # 
     # pipeline$current.indice <- 1
     # pipeline$current.obj <- obj.openDataset()
     # 
     # ## Lancement du module de navigation du pipeline pour suivre les différents process
     # ## de traitement liés au pipeline
     # pipeline$nav2 <- callModule(moduleNavigation2, "moduleGeneral",
     #                             isDone = reactive({rvNav$Done}),
     #                             pages = reactive({rvNav$def}),
     #                             rstFunc = resetNavPipeline,
     #                             type = reactive({'rectangle'})
     #                             )
     #BuildDataminingMenu("Data mining")
   })
   
   
   
   
   # 
   # LoadModulesUI <- function(ll.modules){
   #   ll <- lapply(ll.modules, function(i) {
   #     UIfunc <- paste0(i, "UI")
   #     do.call(UIfunc, list(i))
   #   })
   #   
   #   return(ll)
   # }
   
   
   
   
   
  
   
   
   
   
   
  
  #Once the server part is loaded, hide the loading page 
  # and show th main content
  shinyjs::hide(id = "loading_page", anim = FALSE)
  shinyjs::show("main_content", anim = TRUE, animType = "fade")

}
