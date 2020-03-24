#setwd("~/TELETRAVAIL/github_2.0/Prostar2/R/Drafts/")

library(highcharter)
library(shinyjqui)
library(shiny)
library(shinyBS)
library(DAPAR)
library(DT)

source(file.path(".","module_plots.R"), local=TRUE)$value
source(file.path("..", "mod_plots_legend_colored_exprs.R"), local = TRUE)$value
source(file.path("..", "mod_plots_msnset_explorer.R"),  local = TRUE)$value
source(file.path("..", "global.R"), local = TRUE)$value
source(file.path("..", "mod_plots_corr_matrix.R"), local = TRUE)$value

#actionBtnClass <- "btn-primary"

####################################################

ui <- fluidPage(
  uiOutput('plotModule'),
  uiOutput('createVignettes_large')
)



server <- function(input, output, session) {
  
  
  #jqui_resizable("#modalquantiTable .modal-content")
  #jqui_resizable("#modalcorrMatrix .modal-content")
  
  .width <- 50
  .height <- 50
  llPlots <- c("corrMatrix", "quantiTable")
  
  output$plotModule <- renderUI({
    
    panelheight = 60*length(llPlots)
    absolutePanel(
      id  = "AbsolutePanelPlots",
      #style= "text-align: center; color: grey; border-width:0px; z-index: 10;",
      #top = 150, right = 50,
      width = "70px",
      height = paste0(as.character(panelheight), "px"),
      #draggable = TRUE,fixed = TRUE,
      cursor = "default",
      actionButton('plotBtn', 'Plots', "data-toggle"='collapse', "data-target"="#plotDiv", 
                   style='color: white;background-color: lightgrey',
                   class = actionBtnClass),
      tags$div(
        id = 'plotDiv',
        class="collapse",
        style='background-color: white',
        uiOutput('createVignettes_small')
        
      )
    )
    
  })
  
  #########################################
  ll <- list(NULL)
  
  vHeight <- 60
  vWidth <- 50
  
  for (i in 1:length(llPlots)) {
    switch(llPlots[i],
           quantiTable=ll[[i]] <-
             tags$div( style="display:inline-block;",imageOutput("plotquantiTablesmall"), height='60', width='50'),
           corrMatrix=ll[[i]] <-
             tags$div( style="display:inline-block;",imageOutput("plotcorrMatrixsmall"), height='60', width='50')
    )
  }
  
  for (i in 1:length(llPlots)) {
    n <- i + length(llPlots)
    switch(llPlots[i],
           quantiTable=
             ll[[n]] <- #jqui_draggable(
             shinyBS::bsModal("modalquantiTable", "Data explorer",trigger = "createVignettes_small",size = "large",
                              "Data explorer")
                              #uiOutput("plotquantiTablelarge"))#)
           ,
           corrMatrix=
             ll[[n]] <- shinyBS::bsModal("modalcorrMatrix", "Correlation matrix", trigger = "createVignettes_small",
                                         size = "large",
                                         "Correlation matrix")
                                         #uiOutput("plotcorrMatrixlarge")),
    )
  }
  
  
  output$createVignettes_small <- renderUI({
    print("### ll 1 ###")
    print(ll[1:length(llPlots)])
    ll[1:length(llPlots)]
  })
  
  output$createVignettes_large <- renderUI({
    #####################################
    ## faire varier les caracteristiques de modal-xxx
    #tags$head(tags$style(".modal-dialog{ width:75%}"))
    #####################################
    print("### ll 2 ###")
    print(ll[(length(llPlots)+1):length(ll)])   
    ll[(length(llPlots)+1):length(ll)]
  })
  
  
  
  require(DAPARdata)
  data('Exp1_R25_prot')
  ################### Plot for correlation matrix
  output$plotcorrMatrixsmall <- renderImage({
    filename <- normalizePath(file.path('./images','desc_corrmatrix.png'))
    list(src = filename,
         width = .width,
         height = .height)
  }, deleteFile = FALSE)
  
  
  callModule(mod_plots_corr_matrix_server, "corrMatrixPlot_AbsPanel", obj = Exp1_R25_prot)
  
  output$plotcorrMatrixlarge <- renderUI({
    mod_plots_corr_matrix_ui("corrMatrixPlot_AbsPanel")
    
  })
  
  ############# Plots for MSnSet explorer
  
  output$plotquantiTablesmall <- renderImage({
    filename <- normalizePath(file.path('./images','desc_quantiData.png'))
    list(src = filename,
         width = .width,
         height = .height)
  }, deleteFile = FALSE)
  
  
  callModule(module=mod_plots_msnset_explorer_server, 'msnsetExplorer', obj = Exp1_R25_prot)
  
  output$plotquantiTablelarge <- renderUI({
    mod_plots_msnset_explorer_ui("msnsetExplorer")
  })
  
  
}

shinyApp(ui, server)

