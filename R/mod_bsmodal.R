# ouvre une fenetre, parametre renseigne quoi afficher dans la fenetre


#### module fenetre modal ####
mod_bsmodal_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("bsmodal_ui"))
  )
}


mod_bsmodal_server <- function(id,
                               title=NULL,
                               mod_UI=NULL, 
                               width=NULL){ #height auto
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    jqui_resizable(paste0("#",ns("fenetre")," .modal-content")
                   ,options = list(minHeight = 500, minWidth=500  ))
    
    jqui_draggable(paste0("#",ns("fenetre")," .modal-content")
                   , options = list(revert=TRUE) 
    )
    
    
    output$bsmodal_ui <- renderUI({
      
      tagList(
        tags$head(tags$style(paste0(".modal-dialog { width:",width," }"))),
        actionButton(ns("button"), "Open Modal"),
        
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
    
    
  })
  
  
}