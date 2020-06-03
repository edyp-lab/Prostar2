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
    fluidRow(
      column(width=4,
             htmlOutput(ns('title')),
             br(),
             mod_format_DT_ui(ns('dt'))
      ),
       column(width=4,
              uiOutput(ns('choose_SE_ui'))
    #          uiOutput(ns('properties_ui')),
    #          verbatimTextOutput(ns('properties'))
    #              
       )
    #   column(width=4,
    #          uiOutput(ns('select_SE_ui'))   
    # )
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
             table2show = reactive({as.data.frame(Get_Features_summary())}))
             
  
  output$title <- renderUI({
    print("1")
    req(obj())
    title <- metadata(obj())$analysis
    tagList(
      h4("Assay summary"),
      h3(paste0("Analysis \"",title,"\":"))
    )
  })
  
  
  
  output$choose_SE_ui <- renderUI({
    print("2")
    req(obj())
    selectInput(ns("selectInputSE"),
                "Select a dataset for further information",
                choices = c("None",names(MultiAssayExperiment::experiments(obj())))
    )
  })


  Get_Features_summary <- reactive({
    print("3")

    columns <- c("Number of assay", "List of assays", "Pipeline Type")
    do <- data.frame(Definition= columns,
                     Value=rep(0, length(columns)))
    
    if (!is.null(obj())){
    nb_assay <- paste0(length(obj()), " assay(s)")
    names_assay <- as.list(names(obj()))

    typeOfData <- metadata(obj())[[input$selectInputSE]]$typeOfData
  
    if (typeOfData == "Peptide") {

      if(length(metadata(obj()[[input$selectInputSE]])$list.matAdj) > 0){
        txt <- "Adjacency matrices <span style=\"color: lime\">OK</span>"
      }
      else{
        txt <- "Adjacency matrices <span style=\"color: red\">Missing</span>"
      }

      if(length(metadata(obj()[[input$selectInputSE]])$list.cc) > 0){
        txt <- paste0(txt, " ; Comp Connex <span style=\"color: lime\">OK</span>")
      }
      else{
        txt <- paste0(txt, " ; Comp Connex <span style=\"color: red\">Missing</span>") }

      columns <- c("Number of assay", "List of assays", "Pipeline Type", "Specific to Peptide Pipeline")
      val <- c(nb_assay,
               names_assay,
               pipeline,
               HTML(txt)
      )
    }
    else{
      
      val <- c(nb_assay,
               names_assay,
               pipeline
      )
    }

   
    do$Value <- val
    }
    
    do
  })




  # Get_SE_Summary <- reactive({
  #   print("4")
  #   req(obj())
  #   data <- obj()[[input$selectInputSE]]
  #   columns <- c("Number of samples",
  #                "Number of conditions",
  #                "Number of lines",
  #                "% of missing values", 
  #                "Number of empty lines")
  #   
  #   do <- data.frame(Definition= columns,
  #                    Value=rep(0,length(columns)))
  #   if (is.null(obj())){
  #     do
  #   }
  #   
  #   nb.empty.lines <- nNA(obj()[[input$selectInputSE]])$nNArows[as.character(ncol(obj()[[input$selectInputSE]]))]
  #   
  #   
  #   val <- c(obj()[[input$selectInputSE]],
  #            length(unique(colData(obj())$Condition)),
  #            nrow(obj()[[input$selectInputSE]]),
  #            nNA(obj()[[input$selectInputSE]])$nNA,
  #            nNA(obj()[[input$selectInputSE]])$nNArows[as.character(ncol(obj()[[input$selectInputSE]]))]
  #            )
  #   do$Value <- val
  #   
  #   do
  # })
  # 
  # 
  # 
  # output$properties_ui <- renderUI({
  #   print("5")
  #   req(input$selectInputSE)
  #   req(obj())
  #   
  #   if (input$selectInputSE != "None") {
  #     checkboxInput(ns('properties_button'), "Display details?", value=FALSE)
  #   }
  # })
  # 
  # 
  # 
  # observeEvent(input$selectInputSE,{
  #   
  #   if (isTRUE(input$properties_button)) {
  #     output$properties_ui <- renderUI({
  #       checkboxInput(ns('properties_button'), "Display details?", value=TRUE)
  #     })
  #   }
  #   else{ return(NULL)}
  # })
  # 
  # 
  # output$properties <- renderPrint({
  #   print("6")
  #   req(input$properties_button)
  #   
  #   if (input$selectInputSE != "None" && isTRUE(input$properties_button)) {
  #     
  #     data <- MultiAssayExperiment::experiments(obj())[[input$selectInputSE]]
  #     #DAPAR::properties(data)
  #   }
  # })
  # 
  # 
  # 
  # output$select_SE_ui <- renderUI({
  #   print("7")
  #   req(input$selectInputSE)
  #   req(obj())
  #   
  #   
  #   if (input$selectInputFeatures != "None") {
  # 
  #     data <- MultiAssayExperiment::experiments(obj())[[input$selectInputSE]]
  #     
  #     callModule(mod_format_DT_server,'dt2',
  #                table2show = reactive({Get_SE_Summary()}))
  #                
  #     
  #     callModule(mod_format_DT_server,'dt3',
  #                table2show = reactive({
  #                  data.frame(MultiAssayExperiment::colData(obj()))})
  #                ) 
  #     
  #     tagList(
  #       h3(paste0("MSnSet \"",input$selectInputSE,"\":")),
  #       br(),
  #       h4("SE summary"),
  #       mod_format_DT_ui(ns('dt2')),
  #       br(),
  #       uiOutput(ns('info')),
  #       br(),
  #       h4("pData"),
  #       mod_format_DT_ui(ns('dt3'))
  #       
  #     )
  #   }
  #   else {
  #     return(NULL)
  #   }
  #   
  # })
  # 
  # 
  # 
  # output$info <- renderUI({
  #   print('8')
  #   req(input$selectInputSE)
  #   req(obj())
  #   
  #   if (input$selectInputSE != "None") {
  #     
  #     data <- MultiAssayExperiment::experiments(obj())[[input$selectInputSE]]
  #     
  #     typeOfDataset <- metadata(obj())$typeOfData
  #     
  #     pourcentage <- nNA(obj()[[input$selectInputSE]])$nNA
  #     
  #     nb.empty.lines <- nNA(obj()[[input$selectInputSE]])$nNArows[as.character(ncol(obj()[[input$selectInputSE]]))]
  #     
  #     tagList(
  #       tags$h4("Info"),
  #       if (typeOfDataset == "protein"){
  #         tags$p("The aggregation tool
  #                has been disabled because the dataset contains
  #                protein quantitative data.")
  #       },
  #       
  #       if (pourcentage > 0){
  #         tags$p("As your dataset contains missing values, you should
  #                impute them prior to proceed to the differential analysis.")
  #       },
  #       if (nb.empty.lines > 0){
  #         tags$p("As your dataset contains lines with no values, you
  #                should remove them with the filter tool
  #                prior to proceed to the analysis of the data.")
  #       }
  #       
  #     )
  #   }
  # })
  # 
  
  
  # 
  # NeedsUpdate <- reactive({
  #   
  #   data <- MultiAssayExperiment::experiments(obj())[[input$selectInputMsnset]]
  #   PROSTAR.version <- data@experimentData@other$Prostar_Version
  # 
  #   re(compareVersion(PROSTAR.version,"1.12.9") != -1))
  #   {return (FALSE)}
  # 
  #   else {
  #     return(TRUE)
  #   }
  # })
  # 
  
}

## To be copied in the UI
# mod_infos_dataset_ui("infos_dataset_ui_1")

## To be copied in the server
# callModule(mod_infos_dataset_server, "infos_dataset_ui_1")

