# Module UI

#' @title   mod_settings_ui and mod_settings_server
#' 
#' @description  A shiny Module.
#'
#' @param id shiny id
#' 
#' @param input internal
#' 
#' @param output internal
#' 
#' @param session internal
#'
#' @rdname mod_settings
#'
#' @keywords internal
#' 
#' @export 
#' 
#' @importFrom shiny NS tagList 
#' 
mod_settings_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Miscallenous",
               div(
                 div(
                   style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                   mod_popover_for_help_ui(ns("modulePopover_numPrecision"))
                 ),
                 div(
                   style="display:inline-block; vertical-align: middle;",
                   uiOutput(ns("settings_nDigits_UI"))
                 )
               ),
               br(),hr(),
               tags$p(style="font-size: 18px;", tags$b("Figure export options")),
               tagList(
                 tags$div( style="display:inline-block; vertical-align: middle; padding-right: 40px;",
                           selectInput(ns("sizePNGplots"), "Size of images (PNG)", choices = c("1200 * 800"), width='150px')),
                 tags$div( style="display:inline-block; vertical-align: middle; padding-right: 40px;",
                           selectInput(ns("resoPNGplots"), "Resolution", choices = c(150), width='100px'))
               )),
      tabPanel("Colors",
               div(id = 'showInfoColorOptions', tags$p("Color customization is available after data loading only.")),
               uiOutput(ns("defineColorsUI"))
      ),
      tabPanel("Plots", uiOutput(ns('gradient_ui'))
               
      )
    )
  )
  #)
}



# Module Server

#' @rdname mod_settings
#' 
#' @export
#' 
#' @keywords internal
#' 
#' @import shiny
#' 
#' @import highcharter
#' 
mod_settings_server <- function(input, output, session, obj){
  ns <- session$ns
  
  
  grey <- "#FFFFFF"
  orangeProstar <- "#E97D5E"
  
  observe({
    req(obj())
    rv.settings$conditions <- colData(obj())[['Condition']]
    print(rv.settings$conditions)
  })


  rv.settings <- reactiveValues(
    nDigits = 10,
    conditions = c('A', 'B', 'A', 'B', 'B', 'B'),
    colorsVolcanoplot = reactiveValues(In=orangeProstar, 
                                       Out='lightgrey'
                                        ),
    colorsTypeMV = reactiveValues(MEC=orangeProstar,
                                  POV='lightblue'
                                  ),
    choosePalette = 'Dark2',
    typeOfPalette = 'predefined',
    examplePalette = NULL,
    defaultGradientRate = 0.9,
    legDS = NULL,
    corrMatrixGradient = 0.9,
    legDS_Violinplot = NULL,
    legendForSamples = NULL
    
  )
  
  
  listBrewerPalettes <- c("Dark2 (qualit.)" = "Dark2",
                          "Accent (qualit.)"="Accent",
                          "Paired (qualit.)" = "Paired",
                          "Pastel1 (qualit.)" = "Pastel1",
                          "Pastel2 (qualit.)" = "Pastel2",
                          "Set1 (qualit.)" = "Set1",
                          "Set2 (qualit.)" = "Set2", 
                          "Set3 (qualit.)" = "Set3",
                          "BrBG (diverging)"="BrBG",
                          "PiYG (diverging)"=  "PiYG",
                          "PRGn (diverging)" ="PRGn",
                          "PuOr (diverging)" ="PuOr",
                          "RdBu (diverging)"="RdBu",
                          "RdGy (diverging)" ="RdGy",
                          "RdYlBu (diverging)" ="RdYlBu",
                          "RdYlGn (diverging)" ="RdYlGn",
                          "Spectral (diverging)"="Spectral")
  
  
  output$gradient_ui <- renderUI({
    
    sliderInput(ns("defaultGradientRate"),
                "Default color gradient for correlation matrix",
                min = 0,max = 1,value = rv.settings$defaultGradientRate, step=0.01)
  })
  
  # GetTest <- reactive({
  #   rv.settings$whichGroup2Color
  #   
  #   nbConds <- length(unique(example.Conditions))
  #   pal <- rep('#000000', length(example.Conditions))
  #   
  #   
  #   nbColors <- NULL
  #   temp <- NULL
  #   if (is.null(rv.settings$whichGroup2Color) || (rv.settings$whichGroup2Color=="Condition")){
  #     nbColors <- length(unique(example.Conditions))
  #     nbColors <-  RColorBrewer::brewer.pal.info[listBrewerPalettes[1],]$mincolors
  #     nbColors <- max(nbColors,nbConds)
  #     palette <- NULL
  #     for(i in 1:nbConds){palette <- c(palette,input[[paste0("customColorCondition_",i)]])}
  #     for (i in 1:length(example.Conditions)){
  #       temp[i] <- palette[ which(example.Conditions[i] == unique(example.Conditions))]
  #     }
  #     
  #   } else if (rv.settings$whichGroup2Color=="Replicate"){
  #     nbColors <- length(example.Conditions)
  #     for(i in 1:nbColors){temp <- c(temp,input[[paste0("customColorCondition_",i)]])}
  #   }
  #   
  #   temp
  # })
  # 
  
  ############
  GetExamplePalette <- reactive({
    rv.settings$choosePalette
    rv.settings$typeOfPalette
    nbConds <- length(unique(rv.settings$conditions))
    
    
    palette <- NULL
    nbColors <- max(3,nbConds)
    switch(rv.settings$typeOfPalette,
           predefined={
             palette <- RColorBrewer::brewer.pal(nbColors,rv.settings$choosePalette)[1:nbConds]
             
             for (i in 1:nbConds){
               rv.settings$examplePalette[ which(rv.settings$conditions == unique(rv.settings$conditions)[i])] <- palette[i]
             }
           },
           custom = {
             for(i in 1:nbConds){
               palette <- c(palette,input[[paste0("customColorCondition_",i)]])
             }
             if (is.null(palette)){return(NULL)}
             for (i in 1:nbConds){
               rv.settings$examplePalette[ which(rv.settings$conditions == unique(rv.settings$conditions)[i])] <- palette[i]
             }
           }
    )
    
    rv.settings$examplePalette
  })
  
  
  
  callModule(mod_popover_for_help_server,"modulePopover_numPrecision", data = list(title=HTML(paste0("<strong><font size=\"4\">Numerical precisions</font></strong>")),
                                                                                   content= "Set the number of decimals to display for numerical values."))
  
  output$settings_nDigits_UI <- renderUI({
    numericInput(ns("settings_nDigits"), "", value=rv.settings$nDigits, min=0, width="100px")
  })
  
  observeEvent(input$settings_nDigits,{ rv.settings$nDigits <- input$settings_nDigits })
  
  
  # observeEvent(input$shinythemeSelector,{
  #   tags$script("$('#shinythemeSelector')\n  .on('change', function(el) {\n      curThemePath = 'shinythemes/css/' + curTheme + '.min.css';\n    }\n\n    // Find the <link> element with that has the bootstrap.css\n    var $link = $('link').filter(function() {\n      var theme = $(this).attr('href');\n      theme = theme.replace(/^.*\\//, '').replace(/(\\.min)?\\.css$/, '');\n      return $.inArray(theme, allThemes) !== -1;\n    });\n\n    // Set it to the correct path\n    $link.attr('href', curThemePath);\n  });")
  #   #theme = shinytheme(input$shinythemeSelector)
  # })
  
  observe({
    shinyjs::onclick("btn_configConditionsColors",{
      shinyjs::toggle(id = "defineColorsForConditionsUI", anim = TRUE)}
    )
    
    shinyjs::onclick("btn_configMVColors",{
      shinyjs::toggle(id = "defineColorsForMVUI", anim = TRUE)}
    )
    
    
    shinyjs::onclick("btn_configVolcanoColors",{
      shinyjs::toggle(id = "defineColorsForVolcanoUI", anim = TRUE)}
    )
    
    
    shinyjs::onclick("btn_configFCColors",{
      shinyjs::toggle(id = "defineColorsForFCUI", anim = TRUE)}
    )
  })
  
  
  
  ##########
  output$defineColorsUI <- renderUI({
    
    
    shinyBS::bsCollapse(id = "collapseExample", open = "",
                        shinyBS::bsCollapsePanel("Colors for conditions", uiOutput(ns("defineColorsForConditionsUI")), style = "primary"),
                        shinyBS::bsCollapsePanel("Colors for missing values", tagList(
                          colourpicker::colourInput(ns("colMEC"), "Select colour for MEC", orangeProstar,showColour = "background"),
                          colourpicker::colourInput(ns("colPOV"), "Select colour for POV", "lightblue", showColour = "background")
                        ), style = "primary"),
                        shinyBS::bsCollapsePanel("Colors for volcanoplots", 
                                                 colourpicker::colourInput(ns("colVolcanoIn"), "Select colour for selected entities", 
                                                                           rv.settings$colorsVolcanoplot$In,
                                                                           showColour = "background"),
                                                 colourpicker::colourInput(ns("colVolcanoOut"), "Select colour for filtered out entities", rv.settings$colorsVolcanoplot$Out, showColour = "background"), 
                                                 style = "primary"),
                        shinyBS::bsCollapsePanel("logFC distribution", "todo", style = "primary")
                        
    )
    
    
  })
  
  
  
  
  output$defineColorsForConditionsUI <- renderUI({
    
    tagList(
      fluidRow(
        column(width=3,radioButtons(ns("typeOfPalette"), "Type of palette for conditions",
                                    choices=c("predefined"="predefined", "custom"="custom"), selected=GetTypeOfPalette())),
        column(width=6,highcharter::highchartOutput(ns("displayPalette")))
      ),
      
      uiOutput(ns("predefinedPaletteUI")),
      uiOutput(ns("customPaletteUI")),
      hr()
    )
  })
  
  observeEvent(input$choosePalette, {rv.settings$choosePalette <-input$choosePalette })
  
  
  observeEvent(GetExamplePalette(), {
    rv.settings$examplePalette <- GetExamplePalette()
  })
  
  
  
  observeEvent(input$typeOfPalette,{
    rv.settings$typeOfPalette <- input$typeOfPalette
  })
  
  
  GetTypeOfPalette <- reactive({rv.settings$typeOfPalette})
  
  
  
  output$predefinedPaletteUI <- renderUI({
    rv.settings$typeOfPalette
    
    if (rv.settings$typeOfPalette == 'custom') {return(NULL)}
    selectInput(ns("choosePalette"), "Predefined palettes", 
                choices=listBrewerPalettes,
                selected=rv.settings$choosePalette,width='200px')
  })
  
  output$customPaletteUI <- renderUI({
    rv.settings$typeOfPalette
    if (rv.settings$typeOfPalette == 'predefined') {return(NULL)}
    
    ll <- list()
    nbColors <- NULL
    nbColors <- length(unique(rv.settings$conditions))
    labels <- unique(rv.settings$conditions)
    
    for (i in 1:nbColors) {
      ll <- list(ll,
                 colourpicker::colourInput(ns(paste0("customColorCondition_",i)), 
                                           labels[i],
                                           '#000000',
                                           showColour = "background"))
    }
    
    ll
  })
  
  
  observeEvent(input$colMEC, {rv.settings$colorsTypeMV$MEC <- input$colMEC})
  
  observeEvent(input$colPOV, { rv.settings$colorsTypeMV$POV <- input$colPOV})
  
  observeEvent(input$colVolcanoIn, {rv.settings$colorsVolcanoplot$In <- input$colVolcanoIn})
  
  observeEvent(input$colVolcanoOut, {rv.settings$colorsVolcanoplot$Out <- input$colVolcanoOut})
  
  
  
  output$displayPalette <- highcharter::renderHighchart({
    rv.settings$examplePalette
    nbConds <- length(unique(rv.settings$conditions))
    df <- data.frame(y= abs(10+rnorm(length(rv.settings$conditions))))

    highcharter::highchart() %>%
      DAPAR2::dapar_hc_chart(chartType = "column") %>%
      highcharter::hc_add_series(data = df, type="column", colorByPoint = TRUE) %>%
      highcharter::hc_colors(rv.settings$examplePalette) %>%
      highcharter::hc_plotOptions( column = list(stacking = "normal"), animation=list(duration = 1)) %>%
      highcharter::hc_legend(enabled = FALSE) %>%
      highcharter::hc_yAxis(labels=FALSE,title = list(text = "")) %>%
      highcharter::hc_xAxis(categories = 1:nbConds, title = list(text = "")
      )
  })
  
  return(reactive({rv.settings}))
  
}

## To be copied in the UI
# mod_settings_ui("settings_ui_1")

## To be copied in the server
# callModule(mod_settings_server, "settings_ui_1")

