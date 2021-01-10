# This script is used to check the compatibility for a new module of process within the Prostar architecture
# It only checks xxxx of the code. The passed checks do not guarantee that there are no bugs inside the module's code


name <- 'toto'
pipeline <- 'protein'

# Check the presence of necessary files within Prostar R directory





# check the presence of key code inside the module's code




# Check the watch code of the module




# #### global
# 
# name : '...' 
# dans R/, R/DataManager/, mod_<name>
# dans R/Plots/, mod_plots_<name> sauf mod_all_plots.R
# dans R/PipelineCode/protein mod_pipe_<typeOfData>_<process> ;  watch_pipe_<typeOfData>_<process> # typeOfData could be <name_rep.> ?
# 

# 
# #### a int du file module
# 
# nom correspondant ui : mod_<nom module>_ui
# nom correspondant server : mod_<nom module>_server
# 
# mod_<nom module>_ui <- function(id){
#   # truc obligatoire dans ui
#   *ns <- NS(id)*
# 
# 
#   mod_navigation_ui(ns('nav_<nom module>'))
# }
# 
# 
# mod_<nom module>_server <- function(input, output, session, <param(s)>){
#   *ns <- session$ns*
# 
#     callModule(mod_navigation_server, 'nav_<nom module>', style=2, pages=r.nav)
#   # ui ns('name'), server callModule(...name...)
# 
# 
#   r.nav <- reactiveValues( # trucs obligatoires
#     name
#     stepsNames
#     ll.UI
#     isDone
#     mandatory
#     reset
# 
#     # trucs meme taille
#     stepsNames
#     ll.UI
#     isDone
#     mandatory
# 
#     rv.process <- reactiveValues(
#       # obligatoires
#       dataIn
#       dataOut
#       i
#       
#     fonction reset
#     observeEvent(req(r.nav$reset),{
#         
#         
#     definition des screens
#           ll.UI = list( screenStep1 = uiOutput(ns("Screen_Process_1")),
#                         screenStep2 = uiOutput(ns("Screen_Process_2")),
#                         screenStep3 = uiOutput(ns("Screen_Process_3"))
#           ),
#           Raccord avec suite module : 
#           output$Screen_Process_1
#           output$Screen_Process_2
#           output$Screen_Process_3
#      
#           
##   # pour chaque widget dans liste widgets :
#     correspondance observeEvent(input$<widget_name> avec rv.process RV spe au module $widgets$<widget_name>)
#     widgets = list(assay = 0,
#                     operator = NULL,
#                     operand = NULL)     
#     observeEvent(input$operator, ignoreInit=TRUE,{
#       rv.process$widgets$operator <- input$operator
#     })
# 
# 

# # check <param(s)> quelque soit endroit where module called -> nom param, nb de param, (class ?)


#mod_pipe_protein_Filtering
check_module(name='pipe_protein_Filtering',
             pipeline=TRUE,
             typeOfData='protein',
             process='Filtration')

check_module <- function(name, pipeline, typeOfData, process) {
  
  #setwd() in rep Prostar2/R/
  
  # find repository where module and watch files
  watch_file <- paste0('watch_', name, '.R')
  module_file <-paste0('mod_', name, '.R')
  
  if (pipeline) {
    rep <- paste0('PipelineCode/',typeOfData, '/')
    if (watch_file %in% list.files(rep) &&  module_file %in% list.files(rep)) {
      print('watch and mode files exist')
    }
  }
  
  
  # watch file
  A <- readLines(paste0(rep, watch_file)) #PipelineCode/protein/watch_pipe_protein_Filtering.R
  A <- A[which(A!="")] #remove blank lines
  
  if(length(grep(paste0('\\<mod_', name, '\\>'), A)) == 1) {print('id of callModule ok')}
  
  if(length(grep(paste0('Watch_mod_', name), A)) == 3) {print('dynamic change of rv.core$current.obj ok')} # Attention a Case !
  
  
  # module file
  B <- readLines(paste0(rep, module_file)) #PipelineCode/protein/mod_pipe_protein_Filtering.R
  B <- B[which(B!="")]
  
  
  # check if r.nav
  grep('r.nav', B)[1] #r.nav between line 33 and
  (grep('[[:space:]]){1}$',B))[grep('[[:space:]]){1}$',B)>33][1] #43
  
  C <- B[33:43]
  nav_rv <- c('name','stepsNames','ll.UI','isDone','mandatory','reset')
  nav_rv <- paste0('[[:space:]]',nav_rv,'\\>')
  if(length(grep(paste(nav_rv, collapse = '|'), C, value=T))==6) {print('r.nav variables ok')}
  
  
  
  
  # #----------------------------------------------------------------
  # r.nav <- reactiveValues( # line 33
  #   name = "Filtering",
  #   stepsNames = c("MV filtering", "Field filtering", "Validate"),
  #   ll.UI = list( screenStep1 = uiOutput(ns("Screen_Filtering_1")),
  #                 screenStep2 = uiOutput(ns("Screen_Filtering_2")),
  #                 screenStep3 = uiOutput(ns("Screen_Filtering_3"))
  #   ),
  #   isDone =  rep(FALSE,3),
  #   mandatory =  rep(FALSE,3),
  #   reset = FALSE
  # )
  
  
}

