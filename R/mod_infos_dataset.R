# Module UI

#' @title   mod_infos_dataset_ui and mod_infos_dataset_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_infos_dataset
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_infos_dataset_ui <- function(id){
  ns <- NS(id)
  # tagList(
  #   h3(style='color: red;',"TODO list"),
  #   h4(style='color: red;',"faire le tableau de resume pour les mae avec eventuellement lien vers le tableau des msnset"),
  #   mod_format_DT_ui(ns('dt'))
  #   ,uiOutput(ns('info'))
  # )
  # 
  
  tagList(
    fluidRow(
      column(width=6,
             h4("MAE summary"),
             mod_format_DT_ui(ns('dt'))),
      column(width=6,
             selectInput(ns("selectInputMsnset"),
                         "Select a dataset for further information",
                         choices = c("None",names(dat@ExperimentList@listData))),
             uiOutput(ns('selectMsnset'))
             
             )
      # ,uiOutput(ns('info')))
    )
  )
}

# Module Server

#' @rdname mod_infos_dataset
#' @export
#' @keywords internal

mod_infos_dataset_server <- function(input, output, session, obj=NULL){
  ns <- session$ns
  
  
  callModule(mod_format_DT_server,'dt',
             table2show = reactive({Get_mae_summary(dat)}))
  
  
  Get_mae_summary <- function(dat){
    
    # pour nb_msnset, rajouter distinction singulier/pluriel ?
    nb_msnset <- paste0(length(names(dat@ExperimentList@listData)), " MsnSet")
    names_msnset <- list(names(dat@ExperimentList@listData))
    pipeline <- gsub("Pipeline","",dat@pipelineType)
    
    if (pipeline == "Peptide") {
      
      if(length(dat@matAdj)!=0){
        txt <- "matAdj <span style=\"color: lime\">OK</span>"
      }
      else{ 
        txt <- "matAdj <span style=\"color: red\">Missing</span>"
      }
      
      if(length(dat@CC)!=0){
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
    
    if (is.null(dat)){
      return(do)
    }
    
    do$Value <- val
    
    return(do)
  }
  
  
  
  
  Get_MSnSet_Summary <- function(dat){
    #req(dat)
    
    columns <- c("Number of samples","Number of conditions",
                 "Number of lines", "Number of missing values", "% of missing values", 
                 "Number of empty lines")
    
    do <- data.frame(Definition= columns,
                     Value=rep(0,length(columns)))
    if (is.null(dat)){
      return(do)
    }
    NA.count<- length(which(is.na(Biobase::exprs(dat)==TRUE)))
    pourcentage <- 100 * round(NA.count/(ncol(dat)*nrow(dat)), digits=4)
    nb.empty.lines <- sum(apply(
      is.na(as.matrix(Biobase::exprs(dat))), 1, all))
    
    
    val <- c(ncol(Biobase::exprs(dat)),
             length(unique(Biobase::pData(dat)$Condition)),
             nrow(Biobase::exprs(dat)),
             NA.count,
             pourcentage,
             nb.empty.lines)
    do$Value <- val
    
    return(do)
  }
  
  
  output$selectMsnset <- renderUI({
    req(input$selectInputMsnset)
    
    
    if (input$selectInputMsnset != "None") {
      
      data <- get(input$selectInputMsnset)
      
      callModule(mod_format_DT_server,'dt2',
                 table2show = reactive({Get_MSnSet_Summary(data)}))
      
      callModule(mod_format_DT_server,'dt3',
                 table2show = reactive({data@phenoData@data}))
     
      tagList(
        h4(paste0("Type of Data: ", data@experimentData@other[["typeOfData"]] )),
        br(),
        h4("MsnSet summary"),
        mod_format_DT_ui(ns('dt2')),
        br(),
        h4("pData"),
        mod_format_DT_ui(ns('dt3'))
        
        )
    }
    else {
      return(NULL)
    }
    
  })
  
  # observeEvent(req(input$selectInputMsnset),{
  # 
  #   if (input$selectInputMsnset != "None") {
  # 
  #     data <- get(input$selectInputMsnset)
  # 
  #     callModule(mod_format_DT_server,'dt2',
  #                table2show = reactive({Get_MSnSet_Summary(data)}))
  #   }
  # })
  

  
    
  
  # output$info <- renderUI({
  #   
  #   req(input$selectInputMsnset)
  #   dataset <- input$selectInputMsnset
  #   
  #   typeOfDataset <- dataset@experimentData@other$typeOfData
  #   
  #   if (NeedsUpdate())
  #   {    
  #     tags$div(
  #       tags$div(style="display:inline-block; vertical-align: top;",
  #                tags$img(src = "www/images/Problem.png", height=25)),
  #       tags$div(style="display:inline-block; vertical-align: top;",
  #                HTML("The dataset was created with a former version of ProStaR, which experimental design is not compliant with the current
  #                     software functionalities. Please update the design below"))
  #     )
  #   } else{
  #     
  #     NA.count <- length(which(is.na(Biobase::exprs(dataset))))
  #     nb.empty.lines <- sum(apply(is.na(as.matrix(Biobase::exprs(dataset))), 1, all))
  #     tagList(
  #       tags$h4("Info"),
  #       if (typeOfDataset == "protein"){
  #         tags$p("The aggregation tool
  #                has been disabled because the dataset contains 
  #                protein quantitative data.")
  #       },
  #       
  #       if (NA.count > 0){
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
  #     
  #   }
  # })
  # 
  
  
  
  
  
  ########################################################### 
  # NeedsUpdate <- reactive({
  #   req(input$selectMsnset)
  #   
  #   PROSTAR.version <- dataset@experimentData@other$Prostar_Version
  #   
  #   if (!is.null(PROSTAR.version) && (compareVersion(PROSTAR.version,"1.12.9") != -1)
  #       && (check.design(Biobase::pData(dataset))$valid))
  #   {return (FALSE)}
  #   
  #   else {
  #     return(TRUE)
  #   }
  # })
  
  
}

## To be copied in the UI
# mod_infos_dataset_ui("infos_dataset_ui_1")

## To be copied in the server
# callModule(mod_infos_dataset_server, "infos_dataset_ui_1")

