
source("./module_chunks_in_variables.R") #names(.GlobalEnv)
# modalAlert and shinyAlert raw code

create_ui <- function(name) {
  new_ui <- gsub('name',name,ui)
  cat(new_ui, file="new_ui.R")
}


create_start_server <- function(name) {
  new_start_server <- gsub('name',name,start_server)
  cat(new_start_server, file="new_start_server.R")
}


create_rNav <- function(process, steps) {
  new_rNav <- gsub('process',process,rNav)
  new_rNav <- gsub('\\<steps\\>',steps,new_rNav)
  cat(new_rNav, file="new_rNav.R")
}


create_rvModule <- function(process, widgets_list, settings=TRUE) {
  new_rvModule <- gsub('process',paste0('process_',process),rvModule)
  new_rvModule <- gsub('widgets_list',widgets_list,new_rvModule)
  if (!settings){ new_rvModule <- gsub('settings = NULL,','',new_rvModule) }
  cat(new_rvModule, file="new_rvModule.R")
}


create_reset <- function(widgets_list) {
  new_reset <- gsub('widgets_list',widgets_list,reset)
  cat(new_reset, file="new_reset.R")
}


create_end_server <- function(name) {
  new_end_server <- gsub('name',name,end_server)
  cat(new_end_server, file="new_end_server.R")
}


#-----------------------------------------------------------#

create_ui(name='pipe_protein_Filtering')

create_start_server(name='pipe_protein_Filtering')

create_rNav(process='Filtering',
            steps = 'c(\'MV filtering\', \'Field filtering\', \'Validate\')'  )

create_rvModule(process='protein_Filtering',
                widgets_list='list(ChooseFilters = "None",seuilNA = 0)' )

create_reset(widgets_list='list(ChooseFilters = "None",seuilNA = 0)' )

create_end_server(name='protein_Filtering')







