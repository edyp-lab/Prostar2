# Module UI

#' @title   mod_infos_dataset_ui and mod_infos_dataset_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' @param obj xxxx
#'
#' @rdname mod_infos_dataset
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_infos_dataset_ui <- function(id){
  ns <- NS(id)

  tagList(
    fluidRow(
      column(width=6,
             h4("MAE summary"),
             mod_format_DT_ui(ns('dt'))
      ),
      column(width=6,
             uiOutput(ns('choose_msnset_ui')),
             uiOutput(ns('selectMsnset_ui'))     
      )
    )
  )
}





# Module Server

#' @rdname mod_infos_dataset
#' @export
#' @keywords internal
#' @import MultiAssayExperiment

mod_infos_dataset_server <- function(input, output, session, obj=NULL){
  ns <- session$ns
  
  
  callModule(mod_format_DT_server,'dt',
             table2show = reactive({Get_mae_summary()}))
  
  
  output$choose_msnset_ui <- renderUI({
    req(obj())
    selectInput(ns("selectInputMsnset"),
                "Select a dataset for further information",
                choices = c("None",names(MultiAssayExperiment::experiments(obj())))
                )
  })
  
  Get_mae_summary <- reactive({

    req(obj())
    print(obj())
    # pour nb_msnset, rajouter distinction singulier/pluriel ?
    nb_msnset <- paste0(length(names(MultiAssayExperiment::experiments(obj()))), " MsnSet")
    names_msnset <- list(names(MultiAssayExperiment::experiments(obj())))
    pipeline <- gsub("Pipeline","",DAPAR::pipelineType(obj()))
    
    if (pipeline == "Peptide") {
      
      if(length(DAPAR::matAdj(obj()))!=0){
        txt <- "matAdj <span style=\"color: lime\">OK</span>"
      }
      else{ 
        txt <- "matAdj <span style=\"color: red\">Missing</span>"
      }
      
      if(length(DAPAR::CC(obj()))!=0){
        txt <- paste0(txt, " ; Comp Connex <span style=\"color: lime\">OK</span>")
      }
      else{
        txt <- paste0(txt, " ; Comp Connex <span style=\"color: red\">Missing</span>") }
      
      columns <- c("Number of msnset", "List of msnset", "Pipeline Type", "Specific to Peptide Pipeline")
      val <- c(nb_msnset,
               names_msnset,
               pipeline,
               HTML(txt)
      )
    }
    else{
      columns <- c("Number of msnset", "List of msnset", "Pipeline Type")
      val <- c(nb_msnset,
               names_msnset,
               pipeline
      )
    }
    
    do <- data.frame(Definition= columns,
                     Value=rep(0,length(columns)))
    
    if (is.null(obj())){
      return(do)
    }
    
    do$Value <- val
    
    do
  })
  
  
  
  
  Get_MSnSet_Summary <- reactive({
    data <- MultiAssayExperiment::experiments(obj())[[input$selectInputMsnset]]
    columns <- c("Number of samples",
                 "Number of conditions",
                 "Number of lines",
                 "Number of missing values",
                 "% of missing values", 
                 "Number of empty lines")
    
    do <- data.frame(Definition= columns,
                     Value=rep(0,length(columns)))
    if (is.null(obj())){
      do
    }
    
    NA.count<- length(which(is.na(Biobase::exprs(data)==TRUE)))
    pourcentage <- 100 * round(NA.count/(ncol(data)*nrow(data)), digits=4)
    nb.empty.lines <- sum(apply(
      is.na(as.matrix(Biobase::exprs(data))), 1, all))
    
    
    val <- c(ncol(Biobase::exprs(data)),
             length(unique(Biobase::pData(data)$Condition)),
             nrow(Biobase::exprs(data)),
             NA.count,
             pourcentage,
             nb.empty.lines)
    do$Value <- val
    
    do
  })
  
  
  output$selectMsnset_ui <- renderUI({
    req(input$selectInputMsnset)
    req(obj())
    
    
    if (input$selectInputMsnset != "None") {
      
      data <- MultiAssayExperiment::experiments(obj())[[input$selectInputMsnset]]
      
      callModule(mod_format_DT_server,'dt2',
                 table2show = reactive({Get_MSnSet_Summary()}))
      
      callModule(mod_format_DT_server,'dt3',
                 table2show = reactive({Biobase::pData(data)}))
     
      tagList(
        h4(paste0("Type of Data: ", data@experimentData@other$typeOfData)),
        br(),
        h4("MsnSet summary"),
        mod_format_DT_ui(ns('dt2')),
        uiOutput(ns('info')),
        br(),
        h4("pData"),
        mod_format_DT_ui(ns('dt3'))
        
        )
    }
    else {
      return(NULL)
    }
    
  })
  

  
  output$info <- renderUI({
    req(input$selectInputMsnset)
    req(obj())
    
    if (input$selectInputMsnset != "None") {
     
      data <- MultiAssayExperiment::experiments(obj())[[input$selectInputMsnset]]
      
      typeOfDataset <-  data@experimentData@other$typeOfData
      
      
        
        NA.count <- length(which(is.na(Biobase::exprs(data))))
        nb.empty.lines <- sum(apply(is.na(as.matrix(Biobase::exprs(data))), 1, all))
        tagList(
          tags$h4("Info"),
          if (typeOfDataset == "protein"){
            tags$p("The aggregation tool
                 has been disabled because the dataset contains
                 protein quantitative data.")
          },
          
          if (NA.count > 0){
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

