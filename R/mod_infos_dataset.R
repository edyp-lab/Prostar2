# Module UI

#' @title   mod_infos_dataset_ui and mod_infos_dataset_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' @param obj object mae
#'
#' @rdname mod_infos_dataset
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' 
mod_infos_dataset_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    htmlOutput(ns('title')),
    
    fluidRow(
      column(width=6,
             mod_format_DT_ui(ns('dt')),
             br(),
             br(),
             h4("Samples"),
             mod_format_DT_ui(ns('samples_tab'))
      ),
       column(width=6,
              uiOutput(ns('choose_SE_ui')),
              uiOutput(ns('show_SE_ui'))
             # uiOutput(ns('properties_ui')),
             # verbatimTextOutput(ns('properties'))

       )
    )
  )
}





# Module Server

#' @rdname mod_infos_dataset
#' 
#' @export
#' 
#' @keywords internal
#' 
#' @importFrom MultiAssayExperiment experiments colData
#' 
mod_infos_dataset_server <- function(input, output, session, obj=NULL){
  ns <- session$ns
  
  observe({
    obj()
    if (class(obj()) != 'Features')
      warning("'obj' is not of class 'Features'")
    return(NULL)
  })
  
  
    callModule(mod_format_DT_server,'dt',
             table2show = reactive({as.data.frame(Get_Features_summary())}),
             style=reactive({NULL}))
             
    
    callModule(mod_format_DT_server,'samples_tab',
               table2show = reactive({data.frame(MultiAssayExperiment::colData(obj()))}),
               style = reactive({list(cols = colnames(MultiAssayExperiment::colData(obj())),
                                      vals = colnames(MultiAssayExperiment::colData(obj()))[2],
                                      unique = unique(MultiAssayExperiment::colData(obj())$Condition),
                                      pal = RColorBrewer::brewer.pal(3,'Dark2')[1:2])
               })
    )
    
    
  output$title <- renderUI({
    req(obj())
    title <- metadata(obj())$analysis
    tagList(
      h3("Dataset summary"),
      p(paste0("Name of analysis:",title))
    )
  })
  
  
  
  output$choose_SE_ui <- renderUI({
    req(obj())
    selectInput(ns("selectInputSE"),
                "Select a dataset for further information",
                choices = c("None",names(MultiAssayExperiment::experiments(obj())))
    )
  })


  Get_Features_summary <- reactive({
    req(obj())
    
    nb_assay <- length(obj())
    names_assay <- as.vector(names(obj()))
    pipeline <- metadata(obj())$pipelineType
    nSamples <- nrow(colData(obj()))
    nConds <- length(unique(colData(obj())$Condition))
    eqConds <- TRUE
    
     
    columns <- c("Number of assay(s)",
                 "List of assay(s)",
                 "Pipeline Type")

    vals <- c( if(is.null(metadata(obj())$pipelineType)) '-' else metadata(obj())$pipelineType,
               length(obj()),
               if (length(obj())==0) '-' else HTML(paste0('<ul>', paste0('<li>', names_assay, "</li>", collapse=""), '</ul>', collapse=""))
    )
    
    
    
    do <- data.frame(Definition= columns,
                     Value=vals
                     )
    
    
    do
  })




  Get_SE_Summary <- reactive({
    req(obj())
    req(input$selectInputSE)
    data <- obj()[[input$selectInputSE]]
    
    columns <- c("Type of data",
                 "Number of lines",
                 "% of missing values",
                 "Number of empty lines")
                 
   
   
    typeOfData <- metadata(data)$typeOfData
    nLines <- nrow(obj()[[input$selectInputSE]])
    percentMV <- nNA(obj()[[input$selectInputSE]])$nNA
    nEmptyLines <-nNA(obj()[[input$selectInputSE]])$nNArows[as.character(ncol(obj()[[input$selectInputSE]]))]

    val <- c(typeOfData,
             nLines,
             percentMV,
             nEmptyLines
             )
    
    
    
    if (typeOfData=='Peptide'){
      columns <- c(columns,
                   "Adjacency matrices",
                   "Connex components")
      
      if(length(metadata(obj()[[input$selectInputSE]])$list.matAdj) > 0){
        adjMat.txt <- "Adjacency matrices <span style=\"color: lime\">OK</span>"
      } else{
        adjMat.txt <- "Adjacency matrices <span style=\"color: red\">Missing</span>"
      }
      
      if(length(metadata(obj()[[input$selectInputSE]])$list.cc) > 0){
        cc.txt <- "Comp Connex <span style=\"color: lime\">OK</span>"
      } else{
        cc.txt <- "Comp Connex <span style=\"color: red\">Missing</span>"
      }
      
      val <- c(val, adjMat.txt, cc.txt)
      
    }
    
    do <- data.frame(Definition= columns,
                     Value=val)
    do
  })



  # output$properties_ui <- renderUI({
  #   req(input$selectInputSE)
  #   req(obj())
  # 
  #   if (input$selectInputSE != "None") {
  #     checkboxInput(ns('properties_button'), "Display details?", value=FALSE)
  #   }
  # })



  # observeEvent(input$selectInputSE,{
  # 
  #   if (isTRUE(input$properties_button)) {
  #     output$properties_ui <- renderUI({
  #       checkboxInput(ns('properties_button'), "Display details?", value=TRUE)
  #     })
  #   }
  #   else{ return(NULL)}
  # })


  # output$properties <- renderPrint({
  #   req(input$properties_button)
  # 
  #   if (input$selectInputSE != "None" && isTRUE(input$properties_button)) {
  # 
  #     data <- MultiAssayExperiment::experiments(obj())[[input$selectInputSE]]
  #     metadata(data)
  #   }
  # })



  output$show_SE_ui <- renderUI({
    req(input$selectInputSE)
    req(obj())


    if (input$selectInputSE != "None") {

      data <- MultiAssayExperiment::experiments(obj())[[input$selectInputSE]]

      callModule(mod_format_DT_server,'dt2',
                 table2show = reactive({Get_SE_Summary()}),
                 style=reactive({NULL}))



      tagList(
        # h3(paste0("MSnSet \"",input$selectInputSE,"\":")),
        # br(),
        # h4("SE summary"),
        mod_format_DT_ui(ns('dt2')),
        br(),
        uiOutput(ns('info'))
      )
    }
    else {
      return(NULL)
    }

  })



  output$info <- renderUI({
    req(input$selectInputSE)
    req(obj())

    if (input$selectInputSE != "None") {

      data <- MultiAssayExperiment::experiments(obj())[[input$selectInputSE]]

      typeOfDataset <- metadata(obj()[[input$selectInputSE]])$typeOfData

      pourcentage <- nNA(obj()[[input$selectInputSE]])$nNA

      nb.empty.lines <- nNA(obj()[[input$selectInputSE]])$nNArows[as.character(ncol(obj()[[input$selectInputSE]]))]

      tagList(
        tags$h4("Info"),
        if (typeOfDataset == "protein"){
          tags$p("The aggregation tool
                 has been disabled because the dataset contains
                 protein quantitative data.")
        },

        if (pourcentage > 0){
          tags$p("As your dataset contains missing values, you should
                 impute them prior to proceed to the differential analysis.")
        },
        if (nb.empty.lines > 0){
          tags$p("As your dataset contains lines with no values, you
                 should remove them with the filter tool
                 prior to proceed to the analysis of the data.")
        }

      )
    }
  })

  
  

  NeedsUpdate <- reactive({

    PROSTAR.version <- metadata(MultiAssayExperiment::experiments(obj()))$versions$Prostar_Version
    
    if(compareVersion(PROSTAR.version,"1.12.9") != -1 && !is.na(PROSTAR.version) && PROSTAR.version != "NA") {
      return (FALSE)
      } else {
      return(TRUE)
    }
  })

  
}

## To be copied in the UI
# mod_infos_dataset_ui("infos_dataset_ui_1")

## To be copied in the server
# callModule(mod_infos_dataset_server, "infos_dataset_ui_1")

