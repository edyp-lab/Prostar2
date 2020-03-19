setwd("~/TELETRAVAIL/github_2.0/Prostar2/R/Drafts/")

library(highcharter)
library(shinyjqui)
library(shiny)
library(shinyBS)
library(DAPAR)

source(file.path("..", "mod_plots_corr_matrix.R"), local = TRUE)$value

actionBtnClass <- "btn-primary"


ui <- fluidPage(
  uiOutput('plotModule')
)

server <- function(input, output, session) {
  
  
  .width <- 50
  .height <- 50
  #llPlots <- c("intensity", "pca", "varDist", "corrMatrix", "heatmap", "mv", "quantiTable")
  llPlots <- "corrMatrix"
  
  jqui_draggable("#modalintensity .modal-content")
  
  output$plotModule <- renderUI({
    
    panelheight = 60*length(llPlots)
    absolutePanel(
      id  = "#AbsolutePanelPlots",
      ###########################################
      #style= "text-align: center; color: grey; border-width:0px; z-index: 10;",
      top = 150, right = 50,
      width = "70px",
      height = paste0(as.character(panelheight), "px"),
      #draggable = TRUE,fixed = TRUE,
      cursor = "default",
      tags$head(tags$style(".modal-dialog{ width:95%}")),
      tags$head(tags$style(".modal-body{ min-height:50%}")),
      ###########################################
      actionButton('plotBtn', 'Plots', "data-toggle"='collapse', "data-target"=paste0('#','plotDiv'), 
                   style='color: white;background-color: lightgrey',
                   class = actionBtnClass),
      tags$div(
        id = 'plotDiv',  
        class="collapse", 
        style='background-color: white',
        uiOutput('createVignettes')
      )
    )
    
  })
  
  
  
  output$createVignettes <- renderUI({
    
    print("llPlots")
    print(llPlots)
    
    ll <- list(NULL)
    
    ll[[1]] <- tags$div( style="display:inline-block;",
                           imageOutput("plotcorrMatrixsmall"),
                           height='60',
                           width='50')
    
    n <- 1 + length(llPlots)
    ll[[n]] <- shinyBS::bsModal("modalcorrMatrix",
                                  "Correlation matrix",
                                  "plotcorrMatrixsmall",
                                  size = "large",
                                  uiOutput("plotcorrMatrixlarge")
                                  )
    
    
    
    ll
    
  })
  
  
  
  ################### Plot for correlation matrix
  output$plotcorrMatrixsmall <- renderImage({
    filename <- normalizePath(file.path('./images','desc_corrmatrix.png'))
    list(src = filename,
         width = .width,
         height = .height)
  }, deleteFile = FALSE)
  
  
  require(DAPARdata)
  data('Exp1_R25_prot')
  callModule(mod_plots_corr_matrix_server, "corrMatrixPlot_AbsPanel", obj = Exp1_R25_prot)
  
  output$plotcorrMatrixlarge <- renderUI({
    mod_plots_corr_matrix_ui("corrMatrixPlot_AbsPanel")
    
  })
  
  return(NULL)
  
}


shinyApp(ui, server)
