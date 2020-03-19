######
modulePlotsUI <- function(id){
  ns <- NS(id)
  
  uiOutput(ns("plotModule"))
  
}




####-----------------------------------------------------------####

modulePlots <- function(input, output, session, dataIn, llPlots){
  ns <- session$ns
  
  
  .width <- 50
  .height <- 50
    
  
  jqui_draggable("#modalintensity .modal-content")
  
  output$plotModule <- renderUI({
    req(dataIn)
    
    panelheight = 60*length(llPlots())
    absolutePanel(
      id  = "#AbsolutePanelPlots",
      #style= "text-align: center; color: grey; border-width:0px; z-index: 10;",
      #top = 350, right = 50,
      width = "70px",
      height = paste0(as.character(panelheight), "px"),
      #draggable = TRUE,fixed = TRUE,
      cursor = "default",
      tags$head(tags$style(".modal-dialog{ width:95%}")),
      tags$head(tags$style(".modal-body{ min-height:50%}")),
      actionButton(ns('plotBtn'), 'Plots', "data-toggle"='collapse', "data-target"=paste0('#',ns('plotDiv')), 
                   style='color: white;background-color: lightgrey',
                   class = actionBtnClass),
      tags$div(
        id = ns('plotDiv'),  
        class="collapse", 
        style='background-color: white',
        uiOutput(ns('createVignettes'))
      )
    )
    
  })
  
  
  
  output$createVignettes <- renderUI({
    
    ll <- list(NULL)
    
    vHeight <- 60
    vWidth <- 50
    
    for (i in 1:length(llPlots())) {
      switch(llPlots()[i],
             quantiTable=ll[[i]] <- tags$div( style="display:inline-block;",imageOutput(ns("plotquantiTablesmall"), height='60', width='50')),
             intensity=ll[[i]] <- tags$div( style="display:inline-block;",imageOutput(ns("plotintensitysmall"), height='60', width='50')),
             pca = ll[[i]] <- tags$div( style="display:inline-block;",imageOutput(ns("plotpcasmall"), height='60', width='50')),
             varDist = ll[[i]] <- tags$div( style="display:inline-block;",imageOutput(ns("plotvarDistsmall"), height='60', width='50')),
             corrMatrix=ll[[i]] <- tags$div( style="display:inline-block;",imageOutput(ns("plotcorrMatrixsmall"), height='60', width='50')),
             heatmap = ll[[i]] <- tags$div( style="display:inline-block;",imageOutput(ns("plotheatmapsmall"), height='60', width='50')),
             mv = ll[[i]] <- tags$div( style="display:inline-block;",imageOutput(ns("plotmvsmall"), height='60', width='50'))
      )
    }
    
    
    for (i in 1:length(llPlots())) {
      n <- i + length(llPlots())
      switch(llPlots()[i],
             quantiTable=
               ll[[n]] <- shinyBS::bsModal("modalquantiTable", "Data explorer", ns("plotquantiTablesmall"), size = "small",uiOutput(ns("plotquantiTablelarge"))),
             intensity=
               ll[[n]] <- shinyBS::bsModal("modalintensity", "Intensities distribution", ns("plotintensitysmall"), size = "small",uiOutput(ns("plotintensitylarge"))),
             pca = 
               ll[[n]] <- shinyBS::bsModal("modalpca", "PCA", ns("plotpcasmall"), size = "small",uiOutput(ns("plotpcalarge"))),
             varDist = 
               ll[[n]] <- shinyBS::bsModal("modalvarDist", "Variance distribution", ns("plotvarDistsmall"), size = "small",uiOutput(ns("plotvarDistlarge"))),
             corrMatrix=
               ll[[n]] <- shinyBS::bsModal("modalcorrMatrix", "Correlation matrix", ns("plotcorrMatrixsmall"), size = "small", uiOutput(ns("plotcorrMatrixlarge"))),
             heatmap = 
               ll[[n]] <- shinyBS::bsModal("modalheatmap", "Heatmap", ns("plotheatmapsmall"), size = "small",uiOutput(ns("plotheatmaplarge"))),
             mv = 
               ll[[n]] <- shinyBS::bsModal("modalmv", "Missing values statistics", ns("plotmvsmall"), size = "large",uiOutput(ns("plotmvlarge")))
      )
    }
    
    print("ll")
    print(length(ll))
    print(ll)
    
    # jqui_resizable(paste0("#",ns("modalmv")," .modal-content"), options = list(minHeight = 100, maxHeight = 300,
    #                                                          minWidth = 200, maxWidth = 400))
    # jqui_resizable("#modalintensity .modal-content", options = list(handles = 'e'))
    # jqui_resizable("#modalquantiTable .modal-content", options = list(handles = 'e'))
    # jqui_resizable("#modalpca .modal-content", options = list(handles = 'e'))
    # jqui_resizable("#modalvarDist .modal-content", options = list(handles = 'e'))
    # jqui_resizable("#modalcorrMatrix .modal-content", options = list(handles = 'e'))
    # jqui_resizable("#modalheatmap .modal-content", options = list(handles = 'e'))
    
    
    ll 
    
  })
  
  
  
  
  
  ################### Plot for correlation matrix
  output$plotcorrMatrixsmall <- renderImage({
    filename <- normalizePath(file.path('./images','desc_corrmatrix.png'))
    list(src = filename,
         width = .width,
         height = .height)
  }, deleteFile = FALSE)
  
  
  
  callModule(mod_plots_corr_matrix_server, "corrMatrixPlot_AbsPanel", obj = dataIn)
  
  output$plotcorrMatrixlarge <- renderUI({
    mod_plots_corr_matrix_ui(ns("corrMatrixPlot_AbsPanel"))
    
  })
  
  
  
  
  
  ##### Plots for missing values
  
  
  output$plotmvsmall <- renderImage({
    filename <- normalizePath(file.path('./images','desc_mv.png'))
    list(src = filename,
         width = .width,
         height = .height)
  }, deleteFile = FALSE)
  
  callModule(mod_plots_corr_matrix_server, "MVPlots_AbsPanel", obj = dataIn)
  
  output$plotmvlarge <- renderUI({
    tagList(
      helpText("These barplots display the distribution of missing values in the dataset."),
      mod_plots_corr_matrix_ui(ns("MVPlots_AbsPanel"))
    )
  })
  
  
  
  
  ############# Plots for MSnSet explorer
  
  output$plotquantiTablesmall <- renderImage({
    filename <- normalizePath(file.path('./images','desc_quantiData.png'))
    list(src = filename,
         width = .width,
         height = .height)
  }, deleteFile = FALSE)
  
  
  
  
  callModule(module=mod_plots_msnset_explorer_server, 'msnsetExplorer',
             obj = dataIn)
  
  output$plotquantiTablelarge <- renderUI({
    mod_plots_msnset_explorer_ui(ns('msnsetExplorer'))
  })
  
  
  
  
  ##### Code for heatmap
  
  output$plotheatmapsmall <- renderImage({
    filename <- normalizePath(file.path('./images','desc_heatmap.png'))
    list(src = filename,
         width = .width,
         height = .height)
  }, deleteFile = FALSE)
  
  callModule(mod_plots_heatmap_server, "heatmap_AbsPanel", obj = dataIn)
  
  output$plotheatmaplarge <- renderUI({
    mod_plots_heatmap_ui(ns('heatmap_AbsPanel'))
  })
  
  
  #### Code for PCA
  output$plotpcasmall <- renderImage({
    filename <- normalizePath(file.path('./images','desc_pca.png'))
    list(src = filename,
         width = .width,
         height = .height)
  }, deleteFile = FALSE)
  
  
  
  callModule(module=mod_plots_corr_matrix_server, 'pcaPlots_AbsPanel', obj=dataIn)
  
  output$plotpcalarge <- renderUI({
    mod_plots_corr_matrix_ui(ns('pcaPlots_AbsPanel'))
  })
  
  
  
  ################################################
  #### Code for intensity plots
  output$plotintensitysmall <- renderImage({
    filename <- normalizePath(file.path('./images','desc_intdistrib.png'))
    list(src = filename,
         width = .width,
         height = .height)
  }, deleteFile = FALSE)
  
  callModule(module=mod_plots_corr_matrix_server, 'intensityPlots_AbsPanel', 
             obj=dataIn)
  
  output$plotintensitylarge <- renderUI({
    mod_plots_corr_matrix_ui(ns("intensityPlots_AbsPanel"))
  })
  
  
  
  ############ Module for variance distribution plots
  
  output$plotvarDistsmall <- renderImage({
    filename <- normalizePath(file.path('./images','desc_varDist.jpg'))
    list(src = filename,
         width = .width,
         height = .height)
  }, deleteFile = FALSE)
  
  callModule(mod_plots_var_dist_server, 'varDist_AbsPanel', obj=dataIn)
  
  output$plotvarDistlarge <- renderUI({
    mod_plots_var_dist_ui(ns('varDist_AbsPanel'))
  })
  
  
  return(NULL)
}