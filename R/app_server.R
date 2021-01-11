library(fresh)
library(DAPAR2)
library(QFeatures)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(magrittr)
library(shinyjqui)
library(shinyBS)
library(highcharter)
library(DT)
library(shinyjs)
library(shinythemes)

options(shiny.maxRequestSize=300*1024^2) 
options(encoding = "UTF-8")
options(shiny.fullstacktrace = T)
require(compiler)
enableJIT(3)


library(QFeatures)

lapply(list.files('R/Prostar_UI/', pattern='.R'), 
       function(x) {source(file.path('R/Prostar_UI', x), local=TRUE)$value })

lapply(list.files('R/Tools/', pattern='.R'), 
       function(x) {source(file.path('R/Tools', x), local=TRUE)$value })

lapply(list.files('R/DataManager/', pattern='.R'), 
       function(x) {source(file.path('R/DataManager',x), local=TRUE)$value })

lapply(list.files('R/Plots/', pattern='.R'), 
       function(x) {source(file.path('R/Plots', x), local=TRUE)$value })



#' @import shiny
#' @importFrom shinyjs hide show
#' 
app_server <- function(input, output,session) {
  
  
  output$contenu_dashboardBody <- renderUI({
    
    # "data loading" button > body content extra vertical navigation timeline + stats descr modal button
    if (isFALSE(input$data)){
      col_left <- 1
      col_right <- 12
      display <- "none"
    } else{
      col_left <- 2
      col_right <- 10
      display <- "block"
    }
    
    # body content
    fluidPage(
      theme = shinythemes::shinytheme("cerulean"),
      
      # first column, showed if "data loaded"
      column(col_left, id = "v_timeline", style=paste0("display: ",display," ;"),
             br(),
             h4('Statistic Descriptive'),
             #mod_bsmodal_ui('statsDescriptive'),
             br(),
             # h4('Timeline')
             # , tags$img(src="timeline_v.PNG",
             #            title="General timeline",
             #            style="display:block ; height: 500px; margin: auto;")
      ),
      
      column(col_right,
             tabItems(
               tabItem(tabName = "ProstarHome", class="active", mod_homepage_ui('home')),
               tabItem(tabName = "openFile", h3("Open QFeature file"), mod_import_file_from_ui("open_file")),
               tabItem(tabName = "convert", h3("Convert data"), mod_convert_ms_file_ui("convert_data")),
               tabItem(tabName = "demoData", h3("Charge a demo dataset"), mod_open_demo_dataset_ui("demo_data")),
               tabItem(tabName = "export", h3("Export")), # export module not yet
               tabItem(tabName = "globalSettings", h3('Global settings'), mod_settings_ui('global_settings')),
               tabItem(tabName = "releaseNotes", h3('Release notes'), mod_release_notes_ui('rl')),
               tabItem(tabName = "checkUpdates", h3('Check for updates'), mod_check_updates_ui('check_updates')),
               tabItem(tabName = "usefulLinks", mod_insert_md_ui('links_MD')),
               tabItem(tabName = "faq", mod_insert_md_ui('FAQ_MD')),
               tabItem(tabName = "bugReport", h3('Bug report'), mod_bug_report_ui("bug_report"))
             )
      )
    )
    
  })
  
  # mimics loading data > body content and inactivation of import menus in sidebar
  observeEvent(input$data, { #https://stackoverflow.com/questions/48278111/disable-enable-click-on-dashboard-sidebar-in-shiny
    
    if(isFALSE(input$data)){
      # show sidebar and button sidebar
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
      shinyjs::runjs("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'visible';")
      
      # enable import menus
      shinyjs::removeCssClass(selector = "a[data-value='openFile']", class = "inactiveLink")
      shinyjs::removeCssClass(selector = "a[data-value='convert']", class = "inactiveLink")
      shinyjs::removeCssClass(selector = "a[data-value='demoData']", class = "inactiveLink")
    }
    else{ # "after data loaded"
      # hide sidebar/button sidebar
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
      shinyjs::runjs("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")
      
      # disable import menus 
      shinyjs::addCssClass(selector = "a[data-value='openFile']", class = "inactiveLink")
      shinyjs::addCssClass(selector = "a[data-value='convert']", class = "inactiveLink")
      shinyjs::addCssClass(selector = "a[data-value='demoData']", class = "inactiveLink")
    }
  })
  
  
  
  utils::data(Exp1_R25_prot, package="DAPARdata2")
  
  #---------------------------Server modules calls---------------------------------------------------#
  
  mod_homepage_server('home')
  
  mod_import_file_from_server("open_file")
  mod_convert_ms_file_server("convert_data")
  mod_open_demo_dataset_server("demo_data", pipeline.def=reactive({pipeline.defs}))
  
  mod_settings_server("global_settings", obj = reactive({Exp1_R25_prot}))
  
  mod_release_notes_server("rl")
  
  mod_check_updates_server("check_updates")
  
  mod_insert_md_server("links_MD", URL_links)
  
  mod_insert_md_server("FAQ_MD", URL_FAQ)
  
  mod_bug_report_server("bug_report")
  
  
  
  # # List the first level callModules here
  # 
  # #callModule(mod_loading_page_server,'loadingPage')
  # #callModule(mod_navbar_menu_server,'mainMenu')
  # 
  # 
  # 
  # 
  # ## definition des variables globales liees a un pipeline
  # rv.core <- reactiveValues(
  #   # current working data from current pipeline
  #   type = NULL,
  #   
  #   # Current QFeatures object in Prostar
  #   current.obj = NULL,
  #   
  #   ## indice of the current assay in current.obj, corresponding to a step in the pipeline
  #   current.indice = 1,
  #   
  #   # pipeline choosen by the user for its dataset
  #   current.pipeline = NULL,
  #   
  #   
  #   # objects returned by demode, openmode and convertmode
  #   tmp_dataManager = list(convert = NULL,
  #                             openFile = NULL,
  #                             openDemo = NULL),
  #   
  #   # return value of the settings module
  #   settings = NULL,
  #   
  #   #
  #   tempplot = NULL,
  #   
  #   #
  #   loadData = NULL
  #   
  # )
  # 
  # observeEvent(input$ReloadProstar, { js$reset()})
  # 
  # observeEvent(input$browser,{browser() })
  # 
  # 
  # ## Get the return values of modules in charge of loading datasets
  # rv.core$tmp_dataManager <- list(openFile = mod_open_dataset_server('moduleOpenDataset', 
  #                                                       pipeline.def=reactive({pipeline.defs})),
  #                                 convert = mod_convert_ms_file_server('moduleProcess_Convert', 
  #                                                      pipeline.def=reactive({pipeline.defs})),
  #                                 openDemo = mod_open_demo_dataset_server('mod_OpenDemoDataset', 
  #                                                       pipeline.def=reactive({pipeline.defs}))
  #                                 )
  # 
  # observeEvent(rv.core$tmp_dataManager$openFile(),{ 
  #   rv.core$current.obj <- rv.core$tmp_dataManager$openFile()
  #   setCoreRV()
  # })
  # 
  # observeEvent(rv.core$tmp_dataManager$convert(),{ 
  #   rv.core$current.obj <- rv.core$tmp_dataManager$convert()
  #   setCoreRV()
  #   })
  # 
  # 
  # observeEvent(rv.core$tmp_dataManager$openDemo(),{
  #   rv.core$current.obj <- rv.core$tmp_dataManager$openDemo()
  #   setCoreRV()
  #   })
  # 
  # 
  # 
  # # Update several reactive variable once a dataset is loaded
  # setCoreRV <- reactive({
  #   # We begins with the last SE in the QFeatures dataset
  #   rv.core$current.indice <- length(rv.core$current.obj)
  #   rv.core$current.pipeline <- MultiAssayExperiment::metadata(rv.core$current.obj)$pipelineType
  # })
  # 
  # 
  # # Store the return value of the module settings
  # rv.core$settings <- mod_settings_server("modSettings", obj=reactive({rv.core$current.obj}))
  # 
  # 
  # 
  # 
  # 
  # mod_infos_dataset_server('infos_demoDataset', 
  #            obj = reactive({
  #              req(rv.core$current.obj)
  #              rv.core$current.obj
  #            })
  # )
  # 
  # mod_infos_dataset_server('infos_openFile', 
  #            obj = reactive({
  #              req(rv.core$current.obj)
  #              rv.core$current.obj
  #            })
  # )
  # 
  # rv.core$tmp_indice <- mod_change_assay_server('change_assay', 
  #            ll.se = reactive({names(rv.core$current.obj)}),
  #            indice = reactive({rv.core$current.indice})
  # )
  # 
  # observeEvent(rv.core$tmp_indice(),{
  #   rv.core$current.indice <- rv.core$tmp_indice()
  # })
  # 
  #  
  # mod_homepage_server("homepage")
  # mod_release_notes_server("modReleaseNotes")
  # mod_check_updates_server("modCheckUpdates")
  # 
  # #callModule(mod_bug_report_server, "bugreport")
  # mod_insert_md_server("links_MD",URL_links)
  # mod_insert_md_server("FAQ_MD", URL_FAQ)
  #  
  # 
  #  #Once the type of pipeline is known (ie a dataset has been loaded),
  #  #call the server parts of the processing modules that belongs
  #  # to this pipeline
  #  observeEvent(req(rv.core$current.pipeline), {
  #    
  #    Build_DataMining_Menu()
  #    Build_DataProcessing_Menu()
  #  })
  #  
  #  
  # 
  #  # Builds the menu for data mining tools. This menu is dependent of the type of dataset.
  #  # This is why it is built dynamically
  #  Build_DataMining_Menu <- reactive({
  #   req(rv.core$current.obj)
  #    
  #    mod_all_plots_server('modAllPlots', 
  #               dataIn = reactive({
  #                 req(rv.core$current.obj)
  #                 rv.core$current.obj
  #               }),
  #               indice = reactive({rv.core$current.indice}),
  #               settings = reactive({rv.core$settings()}))
  #    
  #    # Default item is Descriptive statistics for every pipeline
  #    tabs <- list(
  #      tabPanel("Descriptive statistics", value='descriptiveStats', mod_all_plots_ui('modAllPlots'))
  #    )
  #    
  #    
  #     if (MultiAssayExperiment::metadata(rv.core$current.obj)$pipelineType == 'peptide'){
  #    # callModule(module = mod_graph_pept_prot_server, "CC_Multi_Any",
  #    #            obj = reactive({rv.core$current.obj})
  #    #             )
  #    # 
  #    # 
  #    #   tabs <- append(tabs,
  #    #                tabPanel("Graph pept-prot", value='graphCC', mod_graph_pept_prot_ui('CC_Multi_Any'))
  #    #                )
  #     }
  #    
  #    # Add the data mining tab to the header menu of Prostar
  #    insertTab(inputId = "navPage",
  #              do.call(navbarMenu, c('Data mining' ,tabs)),
  #              target="Data manager",
  #              position="after")
  #  })
  #  
  #  
  #  
  #  ## Builds the menu for data processing tools
  #  Build_DataProcessing_Menu <- reactive({
  #    req(rv.core$current.pipeline)
  #    
  #    ## Get list des process du pipeline
  #    proc <- pipeline.defs[[rv.core$current.pipeline]]
  #    dir <- paste0('R/PipelineCode/',rv.core$current.pipeline)
  #  
  #    process.files <- paste0('mod_pipe_',rv.core$current.pipeline, '_', proc, '.R')
  #    
  #    ## Use here a for loop instead of a lapply, otherwise the functions are not
  #    ## correctly instantiated in environment
  #    #Get all code for processing tools
  #    for (f in process.files)
  #      source(file.path(dir, f), local = T)
  #    
  #    # Loads all server parts of the processing modules
  #    watchcode.files <- paste0('watch_pipe_',rv.core$current.pipeline, '_', proc, '.R')
  #    for (f in watchcode.files)
  #      source(file.path(dir, f), local = T)
  #    
  #    ll.modules <- paste0('mod_pipe_', rv.core$current.pipeline, '_', proc)
  #    
  #    tabs <- lapply(proc, function(x){
  #      do.call(tabPanel, list(title=x,
  #                             value=x,
  #                             do.call(paste0('mod_pipe_', rv.core$current.pipeline, '_', x, '_ui'), 
  #                                     list(paste0('mod_pipe_', rv.core$current.pipeline, '_', x)))
  #                             )
  #              )
  #    })
  #    
  #    # Add the data mining tab to the header menu of Prostar
  #    insertTab(inputId = "navPage",
  #              do.call(navbarMenu, c('Data processing' ,tabs)),
  #              target="Data manager",
  #              position="after")
  #  })
  #  
  #  
  #  
  #  
  #  # Update_QF_indice <- function(){
  #  #   nb_SE_diff <- length(names(rv.core$current.obj)) - rv.core$current.indice
  #  #   if ( nb_SE_diff > 1  ){
  #  #     to_keep <- c(1:rv.core$current.indice, length(names(rv.core$current.obj)))
  #  #     rv.core$current.obj[ , ,to_keep]
  #  #   }
  #  #   
  #  #   rv.core$current.indice <- length(names(rv.core$current.obj))
  #  # }
  #  # 
  # 
  #  
  #  
  
  #Once the server part is loaded, hide the loading page 
  # and show th main content
  shinyjs::hide(id = "loading_page", anim = FALSE)
  shinyjs::show("main_content", anim = TRUE, animType = "fade")

}
