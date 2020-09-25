
source("./module_chunks_in_variables.R") #names(.GlobalEnv)


create_ui <- function(name, file, append = FALSE) {
  new_ui <- gsub('name',name,ui)
  cat(new_ui, file=file, append = append)
}


create_start_server <- function(name, file, append = FALSE) {
  new_start_server <- gsub('name',name,start_server)
  cat(new_start_server, file=file, append = append)
}


create_rNav <- function(process, steps, file, append = FALSE) {
  
  
  nb_screen <- length(unlist(strsplit(steps,',')))
  new_rNav <- gsub('nb_screen',nb_screen,rNav) # warning special case like isDone =  c(FALSE,TRUE,FALSE)
  
  ll.UI <- c()
  for (i in 1:nb_screen){
    ll.UI[i] <- paste0("screenStep",i," = uiOutput(ns('Screen_process_",i,'\'))')
  }
  ll.UI <- paste0(ll.UI,collapse = ",")
  new_rNav <- gsub('ll.UI_list',as.character(c(ll.UI)),new_rNav)
  
  new_rNav <- gsub('process',process,new_rNav)
  new_rNav <- gsub('\\<steps\\>',steps,new_rNav)
  
  
  
  cat(new_rNav, file=file, append = append)
  
}

create_rvModule <- function(process, widgets_list, settings=TRUE, file, append = FALSE) {
  new_rvModule <- gsub('process',paste0('process_',process),rvModule)
  new_rvModule <- gsub('widgets_list',widgets_list,new_rvModule)
  if (!settings){ new_rvModule <- gsub('settings = NULL,','',new_rvModule) }
  cat(new_rvModule, file=file, append = append)
}


create_reset <- function(widgets_list, nb_screen, file, append = FALSE) {
  new_reset <- gsub('widgets_list',widgets_list,reset)
  new_reset <- gsub('nb_screen',nb_screen,new_reset)
  cat(new_reset, file=file, append = append)
}

# create_modalAlert <- function(file, append = FALSE){
#   cat(modalAlert, file=file, append = append)
# }
# create_shinyAlert <- function(file, append = FALSE){
#   cat(shinyAlert, file=file, append = append)
# }


create_end_server <- function(name, file, append = FALSE) {
  new_end_server <- gsub('name',name,end_server)
  cat(new_end_server, file=file, append = append)
}


create_screen <- function(process, file, append=FALSE){
  
  screen_list <- c()
  nb_screen <- length(unlist(strsplit(screen_content_ui,',')))
  print(nb_screen)
  for (i in 1:nb_screen){
    screen_list[i] <- paste0('\n#Screen',i,
                             '\noutput$Screen_process_',i,' <- renderUI({\n\n',screen_content_ui[[i]],'\n\n})\n\n',
                             screen_content_server[[i]])
  }
  screen_list <- paste0(screen_list,collapse = "\n")
  new_screen <- paste0('\n\n## Definitions of the screens\n\n', screen_list)
  new_screen <- gsub('process',process,new_screen)
  cat(new_screen, file=file, append = append)
}


create_widgets_from_input <- function(widgets_list, file, append=FALSE){
  
  widgets_list <- unlist(strsplit(widgets_list,","))
  widgets_list <- sub("[[:space:]]*=.*", "", widgets_list)  
  widgets_list <- gsub(' ','',widgets_list)
  
  widgets_from_input <- c()
  for (i in 1:length(widgets_list)){
    widgets_from_input[i] <- paste0('\n\nobserveEvent(input$',
                                    widgets_list[i],
                                    ', ignoreInit=TRUE,{\nrv.filter$widgets$',
                                    widgets_list[i],' <- input$',
                                    widgets_list[i],'\n})\n')
  }
  
  cat(widgets_from_input, file=file, append = append)
}


#-----------------------------------------------------------#
create_watch_file <- function(name, process){
  new_watch_file <- gsub('name',name,watch_file)
  new_watch_file <- gsub('process',process,new_watch_file)
  file=paste0("watch_pipe_",name,".R")
  cat(new_watch_file, file=file)
}

# create_watch_file(name='protein_Filtering',
#                   process='Filtering')
# #-----------------------------------------------------------#
# 
# 
# create_ui(name='protein_Filtering',
#           file="new_ui.R")
# 
# create_start_server(name='protein_Filtering',
#                     file="new_start_server.R")
# 
# create_rNav(process='Filtering',
#             steps = 'c(\'screen1\', \'table2\', \'plop\', \'toto\', \'titi\')',
#             file="new_rNav.R")
# 
# create_rvModule(process='Filtering',
#                 widgets_list='ChooseFilters = "None",seuilNA = 0, seuil_plop=50',
#                 file="new_rvModule.R")
# 
# create_reset(widgets_list='ChooseFilters = "None",seuilNA = 0, seuil_plop=50',
#              nb_screen=5,
#              file="new_reset.R")
# 
# # create_modalAlert(file="modalAlert.R")
# # create_shinyAlert(file="shinyAlert.R")
# 
# create_end_server(name='protein_Filtering',
#                   file="new_end_server.R")
# 
# create_screen(process='plop',
#               file="new_screen.R")
# 
# create_widgets_from_input(widgets_list=' ChooseFilters= "None",  seuilNA   =   10',
#                           file="new_widgets_from_input.R")


#-----------------------------------------------------------#
#-----------------------------------------------------------#
#source("./copy_paste_test_wParam.R")

createModule <- function(name, process, steps, widgets_list, append = TRUE){
  
  file <- paste0("mod_pipe_",name,".R")
  
  cat(NULL, file=file)
  
  create_ui(name=name,file=file, append = append)
  
  create_start_server(name=name,file=file, append = append)
  
  create_rNav(process=process,steps = steps,file=file, append = append)
  
  create_rvModule(process=process,widgets_list=widgets_list,file=file, append = append)
  
  create_reset(widgets_list=widgets_list,nb_screen=length(unlist(strsplit(steps,","))),file=file, append = append)
  
  # create_modalAlert(file=file, append = append)
  
  # create_shinyAlert(file=file, append = append)
  
  create_screen(nb_screen=length(unlist(strsplit(steps,","))),process=process,file=file, append = append)
  
  create_widgets_from_input(widgets_list=widgets_list,file=file, append = append)
  
  create_end_server(name=name,file=file, append = append)
  
  
}

# createModule(name='protein_Filtering',
#              process='Filtering',
#              steps = 'c(\'screen1\', \'table2\', \'plop\', \'toto\', \'titi\')',
#              widgets_list='ChooseFilters = "None",seuilNA = 0, seuil_plop=50',
#              append=TRUE)



