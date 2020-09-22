
source("./module_chunks_in_variables.R") #names(.GlobalEnv)
# modalAlert and shinyAlert raw code

create_ui <- function(name, file, append = FALSE) {
  new_ui <- gsub('name',name,ui)
  cat(new_ui, file=file, append = append)#"new_ui.R"
}


create_start_server <- function(name, file, append = FALSE) {
  new_start_server <- gsub('name',name,start_server)
  cat(new_start_server, file=file, append = append)#"new_start_server.R"
}


create_rNav <- function(process, steps, file, append = FALSE) {
  
  ### meme taille : stepsNames, ll.UI, isDone, mandatory
  nb_screen <- length(unlist(strsplit(steps,',')))
  new_rNav <- gsub('nb_screen',nb_screen,rNav)
  
  ll.UI <- c()
  for (i in 1:nb_screen){
    ll.UI[i] <- paste0("screenStep",i," = uiOutput(ns('Screen_process_",i,'\'))')
  }
  ll.UI <- paste0(ll.UI,collapse = ",")
  new_rNav <- gsub('ll.UI_list',as.character(c(ll.UI)),new_rNav)
  
  new_rNav <- gsub('process',process,new_rNav)
  new_rNav <- gsub('\\<steps\\>',steps,new_rNav)
  
  
  
  cat(new_rNav, file=file, append = append)#"new_rNav.R"
  
}


create_rvModule <- function(process, widgets_list, settings=TRUE, file, append = FALSE) {
  new_rvModule <- gsub('process',paste0('process_',process),rvModule)
  new_rvModule <- gsub('widgets_list',widgets_list,new_rvModule)
  if (!settings){ new_rvModule <- gsub('settings = NULL,','',new_rvModule) }
  cat(new_rvModule, file=file, append = append)#"new_rvModule.R"
}


create_reset <- function(widgets_list, nb_screen, file, append = FALSE) {
  new_reset <- gsub('widgets_list',widgets_list,reset)
  new_reset <- gsub('nb_screen',nb_screen,new_reset)
  cat(new_reset, file=file, append = append)#"new_reset.R"
}

create_modalAlert <- function(file, append = FALSE){
  cat(modalAlert, file=file, append = append)#"modalAlert.R"
}
create_shinyAlert <- function(file, append = FALSE){
  cat(shinyAlert, file=file, append = append)#"shinyAlert.R"
}


create_end_server <- function(name, file, append = FALSE) {
  new_end_server <- gsub('name',name,end_server)
  cat(new_end_server, file=file, append = append)#"new_end_server.R"
}


create_screen <- function(nb_screen, process, file, append=FALSE){
  
  screen_list <- c()
  for (i in 1:nb_screen){
    screen_list[i] <- paste0('#Screen',i,'\noutput$Screen_process_',i,' <- renderUI({\n\n\n})')
  }
  screen_list <- paste0(screen_list,collapse = "\n")
  new_screen <- paste0('## Definitions of the screens\n\n', screen_list)
  new_screen <- gsub('process',process,new_screen)
  cat(new_screen, file=file, append = append)
}

#-----------------------------------------------------------#

create_ui(name='protein_Filtering',
          file="new_ui.R")

create_start_server(name='protein_Filtering',
                    file="new_start_server.R")

create_rNav(process='Filtering',
            steps = 'c(\'screen1\', \'table2\', \'ok\', \'toto\', \'titi\')',
            file="new_rNav.R")

create_rvModule(process='Filtering',
                widgets_list='list(ChooseFilters = "None",seuilNA = 0)',
                file="new_rvModule.R")

create_reset(widgets_list='list(ChooseFilters = "None",seuilNA = 0)',
             nb_screen=5,
             file="new_reset.R")

create_modalAlert(file="modalAlert.R")
create_shinyAlert(file="shinyAlert.R")

create_end_server(name='protein_Filtering',
                  file="new_end_server.R")

create_screen(nb_screen=4,
              process='plop',
              file="new_screen.R")

#-----------------------------------------------------------#
source("./copy_paste_test_wParam.R")
#chunk_order <- c('ui', 'start_server', 'rNav', 'rvModule', 'reset','modalAlert', 'shinyAlert', 'end_server')
createModule <- function(name, process, steps, widgets_list, file, append = TRUE){
  cat(NULL, file=file)
  
  create_ui(name=name,file=file, append = append)
  
  create_start_server(name=name,file=file, append = append)
  
  create_rNav(process=process,steps = steps,file=file, append = append)
  
  create_rvModule(process=process,widgets_list=widgets_list,file=file, append = append)
  
  create_reset(widgets_list=widgets_list,nb_screen=length(unlist(strsplit(steps,","))),file=file, append = append)
  
  create_modalAlert(file=file, append = append)
  
  create_shinyAlert(file=file, append = append)
  
  create_end_server(name=name,file=file, append = append)
  
  # for (i in chunk_order) {
  #   print(i)
  #   method <- grep(paste0('^create_',i), names(.GlobalEnv), value = TRUE)
  #   print(method)
  #   method(name=name, file=file)
  #   do.call(method, list())
  # }
}
createModule(name='protein_Filtering',
             process='Filtering',
             steps = 'c(\'screen1\', \'table2\', \'ok\', \'toto\', \'titi\')',
             widgets_list='list(ChooseFilters = "None",seuilNA = 0)',
             file="new_module.R",
             append=TRUE)



  