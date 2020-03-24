######
modulePlotsUI <- function(id){
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("plotModule")),
    uiOutput(ns("createVignettes_large"))
    )
  
}



####-----------------------------------------------------------####

modulePlots <- function(input, output, session, dataIn, llPlots){
  ns <- session$ns
  
  #####################
  #jqui_resizable("#modalquantiTable .modal-content")
  #jqui_resizable("#modalcorrMatrix .modal-content")
  #jqui_resizable(paste0("#",ns("modalquantiTable")," .modal-content" ))
  #jqui_resizable(paste0("#",ns("modalcorrMatrix")," .modal-content" ))
  #####################
  
  .width <- 50
  .height <- 50
  
  
  
  output$plotModule <- renderUI({
    
    req(dataIn)
    
    panelheight = 60*length(llPlots())
    absolutePanel(
      id  = "AbsolutePanelPlots",
      #style= "text-align: center; color: grey; border-width:0px; z-index: 10;",
      #top = 150, right = 50,
      width = "70px",
      height = paste0(as.character(panelheight), "px"),
      #draggable = TRUE,fixed = TRUE,
      cursor = "default",
      actionButton(ns("plotBtn"), 'Plots', "data-toggle"='collapse', "data-target"=paste0("#",ns("plotDiv")), 
                   style='color: white;background-color: lightgrey',
                   class = actionBtnClass),
      tags$div(
        id = ns("plotDiv"),
        class="collapse",
        style='background-color: white',
        uiOutput(ns("createVignettes_small"))
      )
    )
  })
  
  
  
  
  output$createVignettes_small <- renderUI({
    
    req(dataIn)
    
    ll.1 <- list(NULL)
    
    vHeight <- 60
    vWidth <- 50
    
    print("llPlots")
    print(llPlots())
    
    for (i in 1:length(llPlots())) {
      switch(llPlots()[i],
             quantiTable=ll.1[[i]] <-
               tags$div( style="display:inline-block;",imageOutput(ns("plotquantiTablesmall")), height='60', width='50'),
             intensity=ll.1[[i]] <-
               tags$div( style="display:inline-block;",imageOutput(ns("plotintensitysmall")), height='60', width='50'),
             pca = ll.1[[i]] <-
               tags$div( style="display:inline-block;",imageOutput(ns("plotpcasmall")), height='60', width='50'),
             varDist = ll.1[[i]] <-
               tags$div( style="display:inline-block;",imageOutput(ns("plotvarDistsmall")), height='60', width='50'),
             corrMatrix=ll.1[[i]] <-
               tags$div( style="display:inline-block;",imageOutput(ns("plotcorrMatrixsmall")), height='60', width='50'),
             heatmap = ll.1[[i]] <-
               tags$div( style="display:inline-block;",imageOutput(ns("plotheatmapsmall")), height='60', width='50'),
             mv = ll.1[[i]] <- 
               tags$div( style="display:inline-block;",imageOutput(ns("plotmvsmall")), height='60', width='50')
      )
    }
    print("ll-1")
    print(ll.1)
    ll.1 
  })
  
  
  
  output$createVignettes_large <- renderUI({
    
    req(dataIn)
    #req(createVignettes_small)
    
    #####################################
    ## faire varier les caracteristiques de modal-xxx
    #tags$head(tags$style(".modal-dialog{ width:75%}"))
    #####################################
    ll <- list(NULL)
    
    for (i in 1:length(llPlots())) {
      
      switch(llPlots()[i],
             quantiTable=
               ll[[i]] <-
               #jqui_draggable(
               shinyBS::bsModal("modalquantiTable", "Data explorer", trigger = ns("createVignettes_small"), size = "large",uiOutput(ns("plotquantiTablelarge")))#)
      ,
             intensity=
               ll[[i]] <-
               shinyBS::bsModal("modalintensity", "Intensities distribution", ns("plotintensitysmall"), size = "large",uiOutput(ns("plotintensitylarge"))),
             pca = 
               ll[[i]] <- 
               shinyBS::bsModal("modalpca", "PCA", ns("plotpcasmall"), size = "large",uiOutput(ns("plotpcalarge"))),
             varDist = 
               ll[[i]] <-
               shinyBS::bsModal("modalvarDist", "Variance distribution", ns("plotvarDistsmall"), size = "large",uiOutput(ns("plotvarDistlarge"))),
             corrMatrix=
               ll[[i]] <- 
               #jqui_draggable(
        shinyBS::bsModal("modalcorrMatrix", "Correlation matrix", trigger = ns("createVignettes_small"), size = "large", uiOutput(ns("plotcorrMatrixlarge")))#)
      ,
             heatmap = 
               ll[[i]] <-
               shinyBS::bsModal("modalheatmap", "Heatmap", ns("plotheatmapsmall"), size = "large",uiOutput(ns("plotheatmaplarge"))),
             mv = 
               ll[[i]] <-
               shinyBS::bsModal("modalmv", "Missing values statistics", ns("plotmvsmall"), size = "large",uiOutput(ns("plotmvlarge")))
             
      )
    }
    print("ll-2")
    print(ll)
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
  
  
  
  
  #return(NULL)
}
  