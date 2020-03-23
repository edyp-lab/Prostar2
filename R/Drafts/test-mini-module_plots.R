setwd("~/TELETRAVAIL/github_2.0/Prostar2/R/Drafts/")

library(highcharter)
library(shinyjqui)
library(shiny)
library(shinyBS)
library(DAPAR)

source(file.path("..", "mod_plots_corr_matrix.R"), local = TRUE)$value

actionBtnClass <- "btn-primary"


ui <- fluidPage(
  uiOutput('plotModule'),
  uiOutput('createVignettes_large')
)

server <- function(input, output, session) {
  
  jqui_resizable("#modal .modal-content" )
  
  .width <- 50
  .height <- 50
  llPlots <- "corrMatrix"
  
  
  
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
  
  
  
  # output$createVignettes <- renderUI({
  #   
  #   print("llPlots")
  #   print(llPlots)
  #   uiOutput('plotModule')
  #   
  #   ll <- list(NULL)
  #   
  #   ll[[1]] <- tags$div( style="display:inline-block;",
  #                        imageOutput("plotcorrMatrixsmall"),
  #                        height='60',
  #                        width='50')
  #   
  #   n <- 1 + length(llPlots)
  #   ll[[n]] <- jqui_draggable(
  #     shinyBS::bsModal("modal",
  #                      "Correlation matrix",
  #                      trigger = "plotcorrMatrixsmall",
  #                      size = "small",
  #                      "bar"
  #                      #uiOutput("plotcorrMatrixlarge")
  #     )
  #   )
  #   
  #   
  #   ll
  #   
  # })
  
  output$createVignettes_small <- renderUI({
    
    tagList(
      tags$div( style="display:inline-block;",
                imageOutput("plotcorrMatrixsmall"),
                height='60',
                width='50')
    )
    
    
  })
  
  output$createVignettes_large <- renderUI({
    
    tagList (   
      
      tags$head(tags$style(".modal-dialog{ width:75%}")),
      
      jqui_draggable(
        shinyBS::bsModal("modal",
                         "Correlation matrix",
                         trigger = "createVignettes_small",
                         size = "large",
                         uiOutput("plotcorrMatrixlarge")
        ), options = list(revert=TRUE)
      ) )
    
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
  
  
  
}


shinyApp(ui, server)
