library(shiny)
library(shinyjqui)
library(shinyBS)
library(DAPAR)

source(file.path("../../R", "Drafts/test_modal.R"), local=TRUE)$value


#### test modal ####
ui <- fluidPage(
  mod_bsmodal_ui('exemple')
)


server <- function(input, output, session) {

  # dans body de modal
  library(DAPARdata)
  data("Exp1_R25_prot")
  #data("Exp1_R2_prot")
  callModule(mod_plots_heatmap_server,'exemple_plot',
             obj=reactive({Exp1_R25_prot})
             ,width=800) # en px

  mod_UI <- mod_plots_heatmap_ui('exemple_plot')
  title <- "Plot Heatmap"

  # module d'affichage modal contenant ci-dessus
  callModule(mod_bsmodal_server,'exemple',
             title = title,
             mod_UI = mod_UI
             # ,datasets = reactive({c(Exp1_R25_prot,Exp1_R2_prot)})
             ,width="75%" # en px ou % de largeur
  )
}

shinyApp(ui=ui, server=server)