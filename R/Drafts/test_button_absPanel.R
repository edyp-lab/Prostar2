
library(shiny)
library(shinyBS)
library(shinyjqui)
library(highcharter)
library(DAPAR)
library(DT)
library(shinyjs)

source(file.path("../../R", "Drafts/test_modal.R"), local=TRUE)$value
source(file.path("../../R", "global.R"), local = TRUE)$value
source(file.path("../../R", "mod_format_DT.R"), local = TRUE)$value
source(file.path("../../R", "mod_settings.R"), local = TRUE)$value
source(file.path("../../R", "mod_popover_for_help.R"), local = TRUE)$value
source(file.path("../../R", "mod_plots_legend_colored_exprs.R"), local = TRUE)$value
source(file.path("../../R", "mod_plots_tracking.R"), local = TRUE)$value
source(file.path("../../R", "mod_plots_intensity.R"), local = TRUE)$value
source(file.path("../../R", "mod_plots_corr_matrix.R"), local = TRUE)$value
source(file.path("../../R", "mod_plots_heatmap.R"), local = TRUE)$value
source(file.path("../../R", "mod_plots_group_mv.R"),  local = TRUE)$value
source(file.path("../../R", "mod_plots_msnset_explorer.R"),  local = TRUE)$value
source(file.path("../../R", "mod_plots_var_dist.R"), local = TRUE)$value
source(file.path("../../R", "mod_plots_pca.R"), local = TRUE)$value
source(file.path("../../R", "mod_all_plots.R"), local=TRUE)$value



ui <- absolutePanel(
    style= "text-align: center; color: grey; border-width:0px; z-index: 10;",
    top = 350, right = 50,
    draggable = TRUE,fixed = TRUE,
    cursor = "default",
    actionButton('button', 'Open Modal', 
                 style='color: white;background-color: lightgrey',
                 class = actionBtnClass),
    uiOutput("openModal")
  )




server <- function(input, output, session){
  
  
  output$openModal <- renderUI({
    req(input$button)
    
    #############################################
    ## a faire afficher dans le modal
    #############################################
    require(DAPARdata)
    
    r <- reactiveValues(
      settings = NULL
    )
    
    r$settings <- callModule(mod_settings_server, "settings")
    
    datasets <- utils::data(package="DAPARdata")$results[,"Item"]
    data('Exp1_R25_pept')
    data('Exp1_R25_prot')
    obj <- Exp1_R25_pept
    samples <- Biobase::pData(obj)
    mae <- PipelineProtein(analysis= 'test',
                           pipelineType = 'peptide', 
                           dataType = 'peptide',
                           processes='original',
                           experiments=list(original=Exp1_R25_prot,
                                            test = Exp1_R25_pept),
                           colData=samples)
    
    
    #############################################
    
    title <- "Exemple"
    mod_UI <- mod_all_plots_ui('exemple_plot')
    callModule(mod_all_plots_server,'exemple_plot',
               dataIn = reactive({mae}),
               settings = reactive({r$settings()}) ) 
    
    
    mod_bsmodal_ui('exemple')
    callModule(mod_bsmodal_server,'exemple',
               title = title,
               mod_UI = mod_UI
               ,width="75%" # en px ou en % de largeur
    ) 
    
    
  })
  
  
}

shinyApp(ui=ui, server=server)
