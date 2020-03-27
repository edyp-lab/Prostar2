# ouvre une fenetre, parametre renseigne quoi afficher dans la fenetre
library(shiny)
library(shinyjqui)
library(shinyBS)
library(DAPAR)


#### module fenetre modal ####
mod_bsmodal_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("bsmodal_ui"))
  )
}


mod_bsmodal_server <- function(input, output, session,
                               title=NULL,
                               mod_UI=NULL, width=NULL){ #height auto
  ns <- session$ns
  
  jqui_resizable(paste0("#",ns("fenetre")," .modal-content")
                 ,options = list(minHeight = 500, minWidth=500  ))
  
  jqui_draggable(paste0("#",ns("fenetre")," .modal-content")
                 , options = list(revert=TRUE) 
  )
  
  
  output$bsmodal_ui <- renderUI({
    
    tagList(
      tags$head(tags$style(paste0(".modal-dialog { width:",width," }"))),
      #actionButton(ns("button"), "Open Modal"),
      
      shinyBS::bsModal(ns("fenetre"),
                       title,
                       trigger = ns("button"),
                       uiOutput(ns("mod_content")) )
    )
    
  })
  
  
  output$mod_content <- renderUI({
    tagList(
      mod_UI  
    )
    
  })
  
}


# #### test modal ####
# ui <- fluidPage(
#   mod_bsmodal_ui('exemple')
# )
# 
# 
# server <- function(input, output, session) {
#   
#   # dans body de modal
#   library(DAPARdata)
#   data("Exp1_R25_prot")
#   #data("Exp1_R2_prot")
#   callModule(mod_plots_heatmap_server,'exemple_plot',
#              obj=reactive({Exp1_R25_prot})
#              ,width=800) # en px
#   
#   mod_UI <- mod_plots_heatmap_ui('exemple_plot')
#   title <- "Plot Heatmap"
#   
#   # module d'affichage modal contenant ci-dessus
#   callModule(mod_bsmodal_server,'exemple',
#              title = title,
#              mod_UI = mod_UI
#              # ,datasets = reactive({c(Exp1_R25_prot,Exp1_R2_prot)})
#              ,width="75%" # en px ou % de largeur
#   ) 
# }
# 
# shinyApp(ui=ui, server=server)