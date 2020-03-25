######
module_plots_ui <- function(id){
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),
     uiOutput(ns("viewVignettes")),
      br(),br(),br(),
    shinyjs::hidden(div(id=ns('div_plot_quanti_large'),mod_plots_msnset_explorer_ui(ns('plot_quanti_large')))),
    shinyjs::hidden(div(id=ns('div_plot_intensity_large'),mod_plots_intensity_ui(ns('plot_intensity_large')))),
    #shinyjs::hidden(div(id=ns('div_plot_pca_large'),mod_plots_pca_ui(ns('plot_pca_large')))),
    shinyjs::hidden(div(id=ns('div_plot_var_dist_large'),mod_plots_var_dist_ui(ns('plot_var_dist_large')))),
    shinyjs::hidden(div(id=ns('div_plot_corrMatrix_large'),mod_plots_corr_matrix_ui(ns('plot_corrMatrix_large')))),
    shinyjs::hidden(div(id=ns('div_plot_heatmap_large'),mod_plots_heatmap_ui(ns('plot_heatmap_large')))),
    shinyjs::hidden(div(id=ns('div_plot_group_mv_large'),mod_plots_group_mv_ui(ns('plot_group_mv_large'))))
      )

}




####-----------------------------------------------------------####

module_plots_server <- function(input, output, session, dataIn, llPlots,base_palette){
  ns <- session$ns
  
  jqui_resizable(paste0("#",ns("modalcorrMatrix")," .modal-content" ))
  jqui_draggable(paste0("#",ns("modalcorrMatrix")," .modal-content"), options = list(revert=TRUE))
  jqui_resizable(paste0("#",ns("modalquantiTable")," .modal-content" ))
  jqui_draggable(paste0("#",ns("modalquantiTable")," .modal-content"), options = list(revert=TRUE))
  
  
  .width <- 50
  .height <- 50

  

  rv <- reactiveValues(
    current.plot = NULL
  )
  
  output$viewVignettes <- renderUI({
   
    fluidPage(
      tags$style(".topimg {
                            margin-left:-20px;
                            margin-right:-20px;
                            margin-top:-20px;
                            margin-bottom:-20px;
                          }"),
      tags$button(
        id = ns("btn_quanti"),
        class = "btn action-button",
        div(class="topimg",imageOutput(ns('plot_quanti_small'), height=40, width=40))
      ),
      tags$button(
        id = ns("btn_intensity"),
        class = "btn action-button",
        div(class="topimg",imageOutput(ns('plot_intensity_small'), height=40, width=40))
      ),
      tags$button(
        id = ns("btn_pca"),
        class = "btn action-button",
        div(class="topimg",imageOutput(ns('plot_pca_small'), height=40, width=40))
      ),
      tags$button(
        id = ns("btn_var_dist"),
        class = "btn action-button",
        div(class="topimg",imageOutput(ns('plot_var_dist_small'), height=40, width=40))
      ),
      tags$button(
        id = ns("btn_corr_matrix"),
        class = "btn action-button",
        div(class="topimg",imageOutput(ns('plot_corr_matrix_small'), height=40, width=40))
      ),
      tags$button(
        id = ns("btn_heatmap"),
        class = "btn action-button",
        div(class="topimg",imageOutput(ns('plot_heatmap_small'), height=40, width=40))
      ),
      tags$button(
        id = ns("btn_group_mv"),
        class = "btn action-button",
        div(class="topimg",imageOutput(ns('plot_mv_small'), height=40, width=40))
      )
    )

  })
  
  callModule(mod_plots_group_mv_server, 
             "plot_group_mv_large", 
             obj = reactive({dataIn()}),
             base_palette = reactive({base_palette()}))
  
  callModule(mod_plots_var_dist_server, 'plot_var_dist_large', obj=reactive({dataIn()}))
  callModule(mod_plots_msnset_explorer_server, 'plot_quanti_large', obj = reactive({dataIn()}))
  callModule(mod_plots_corr_matrix_server, "plot_corr_matrix_large", obj = reactive({dataIn()}))
  callModule(mod_plots_heatmap_server, "plot_heatmap_large", obj = reactive({dataIn()}))
  #callModule(module=mod_plots_pca_server, 'plot_pca_large', obj=reactive({dataIn()}))
  callModule(module=mod_plots_intensity_server, 'plot_intensity_large', 
             dataIn=reactive({dataIn()}),
             params = reactive({NULL}),
             reset = reactive({FALSE}),
             base_palette = reactive({base_palette()})
  )
  

  
  observeEvent(input$btn_quanti,{rv$current.plot <- 'quanti'})
  observeEvent(input$btn_intensity,{rv$current.plot <- 'intensity'})
  observeEvent(input$btn_pca,{rv$current.plot <- 'pca'})
  observeEvent(input$btn_var_dist,{rv$current.plot <- 'var_dist'})
  observeEvent(input$btn_corr_matrix,{rv$current.plot <- 'corr_matrix'})
  observeEvent(input$btn_heatmap,{rv$current.plot <- 'heatmap'})
  observeEvent(input$btn_group_mv,{rv$current.plot <- 'group_mv'})
  
  
  
  observeEvent(rv$current.plot,{
    print(paste0('current.plot = ', rv$current.plot))
    ll <- c('quanti', 'intensity', 'pca', 'var_dist', 'corr_matrix', 'heatmap', 'group_mv')
    
    for (i in ll){
      if (i == rv$current.plot) {
        shinyjs::show(paste0('div_plot_',rv$current.plot,'_large'))
      } else {
        shinyjs::hide(paste0('div_plot_',i,'_large'))
      }
    }
    
  })
  

  
  

  ################### Plot for correlation matrix
  output$plot_corr_matrix_small <- renderImage({
    filename <- normalizePath(file.path('./images','desc_corrmatrix.png'))
    list(src = filename,
         width = .width,
         height = .height)
  }, deleteFile = FALSE)
  
  
  
  
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
  output$plot_quanti_small <- renderImage({

    filename <- normalizePath(file.path('./images','desc_quantiData.png'))
    list(src = filename,
         width = .width,
         height = .height)
  }, deleteFile = FALSE)



  ###############################
  
  
  output$plot_group_mv_small <- renderImage({
    filename <- normalizePath(file.path('./images','desc_mv.png'))
    list(src = filename,
         width = .width,
         height = .height)
  }, deleteFile = FALSE)
  
 
   ##### Code for heatmap
  
  output$plot_heatmap_small <- renderImage({
    filename <- normalizePath(file.path('./images','desc_heatmap.png'))
    list(src = filename,
         width = .width,
         height = .height)
  }, deleteFile = FALSE)
  
  output$plot_heatmap_large <- renderUI({
    mod_plots_heatmap_ui(ns('heatmap_AbsPanel'))
  })
  
  
  #### Code for PCA
  # output$plotpcasmall <- renderImage({
  #   filename <- normalizePath(file.path('./images','desc_pca.png'))
  #   list(src = filename,
  #        width = .width,
  #        height = .height)
  # }, deleteFile = FALSE)
  # 
  # 
  # 
  # 
  # 
  # output$plotpcalarge <- renderUI({
  #   mod_plots_pca_ui(ns('pcaPlots_AbsPanel'))
  # })
  
  
  
  ################################################
  #### Code for intensity plots
  output$plot_intensity_small <- renderImage({
    filename <- normalizePath(file.path('./images','desc_intdistrib.png'))
    list(src = filename,
         width = .width,
         height = .height)
  }, deleteFile = FALSE)
  

  
  ############ Module for variance distribution plots
  
  output$plot_var_dist_small <- renderImage({
    filename <- normalizePath(file.path('./images','desc_varDist.jpg'))
    list(src = filename,
         width = .width,
         height = .height)
  }, deleteFile = FALSE)
  

  

  return(NULL)
}
  