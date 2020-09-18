# take variables from module_chunks_in_variables.R containing module piece by piece
# build a module "new_module.R" with the pieces one after the other

source("./module_chunks_in_variables.R")


cat(ui, file="new_module.R")
cat(start_server, file="new_module.R", append = TRUE)
cat(rNav, file="new_module.R", append = TRUE)
cat(rvModule, file="new_module.R", append = TRUE)
cat(modalAlert, file="new_module.R", append = TRUE)
cat(shinyAlert, file="new_module.R", append = TRUE)
cat(end_server, file="new_module.R", append = TRUE)



# createModule <- function(){
#   
#   cat(NULL, file="new_module.R")
#   
#   for (i in names(.GlobalEnv)){
#     
#     cat(i, file="new_module.R", append = TRUE)
#     
#   }
#   
# }