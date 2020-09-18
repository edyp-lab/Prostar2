
source("./module_chunks_in_variables.R") #names(.GlobalEnv)

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


#-----------------------------------------------------------#

create_ui(name='pipe_protein_Filtering')

create_start_server(name='pipe_protein_Filtering')

create_rNav(process='Filtering',
            steps = 'c(\'MV filtering\', \'Field filtering\', \'Validate\')'  )



######################################################