# ouvre une fenetre, parametre renseigne quoi afficher dans la fenetre
library(shiny)
library(shinyjqui)
library(shinyBS)
library(DAPAR)

source(file.path("../../R","mod_plots_heatmap.R"), local=TRUE)$value

#### module fenetre modal ####
mod_bsmodal_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(tags$style(".modal-dialog { width:75% }")),
    uiOutput(ns("bsmodal_ui"))
  )
}


mod_bsmodal_server <- function(input, output, session, title=NULL, mod_UI=NULL, width=NULL, height=NULL){
  ns <- session$ns
  
  jqui_resizable(paste0("#",ns("modal")," .modal-content"), options = list(minHeight = 200, minWidth=200))
  jqui_draggable(paste0("#",ns("modal")," .modal-content")#K, options = list(revert=TRUE) 
                 )
  
  observeEvent(input$button,{
    print("Open Modal")
  })
  
  output$bsmodal_ui <- renderUI({
    tagList(
      actionButton(ns("button"), "Open Modal"),
      
      shinyBS::bsModal(ns("modal"),
                       title,
                       trigger = ns("button"),
                       size = "small",
                       uiOutput(ns("mod_content")) )
    )
    
  })
  
  
  output$mod_content <- renderUI({ #mod_UI <=> *Output()
    tagList(
      mod_UI  
    )
    
  })
  
}


#### test modal ####
ui <- fluidPage(
  mod_bsmodal_ui('exemple')
)


server <- function(input, output, session) {
  
  # dans body de modal
  library(DAPARdata)
  data("Exp1_R25_prot")
  callModule(mod_plots_heatmap_server,'exemple_plot', obj=reactive({Exp1_R25_prot}))
  mod_UI <- mod_plots_heatmap_ui('exemple_plot')
  
  # module d'affichage modal contenant ci-dessus
  callModule(mod_bsmodal_server,'exemple', title = "Plot Intensity", mod_UI = mod_UI)
}

shinyApp(ui=ui, server=server)
