# Module UI
  
#' @title   mod_msnset_explorer_ui and mod_msnset_explorer_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_msnset_explorer
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_plots_msnset_explorer_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("DS_sidebarPanel_tab")),
    uiOutput(ns("tabToShow"))
  )
}
    
# Module Server
    
#' @rdname mod_msnset_explorer
#' @export
#' @keywords internal
    
mod_plots_msnset_explorer_server <- function(input, output, session, obj=NULL){ # obj est un msnset
  ns <- session$ns
  
  
  
  output$DS_sidebarPanel_tab <- renderUI({
    
    typeOfDataset <- obj()@experimentData@other$typeOfData
    .choices<- NULL
    
    
    switch(typeOfDataset,
           protein = {
             .choices <- list( "Quantitative data" = "tabExprs",
                               "Proteins metadata" = "tabfData",
                               "Experimental design" = "tabpData")
           },
           peptide = {
             .choices <- list("Quantitative data" = "tabExprs",
                              "Peptides metadata" = "tabfData",
                              "Experimental design" = "tabpData")
           },
           {
             .choices <- list("Quantitative data" = "tabExprs",
                              "Analyte metadata" = "tabfData",
                              "Experimental design" = "tabpData")
           }
    )
    
    tagList(
      tags$div(
        tags$div( style="display:inline-block; vertical-align: middle; padding-right: 40px;",
                  radioButtons(ns("DS_TabsChoice"), "Table to display",
                               choices = .choices,
                               inline = TRUE,
                               selected = character(0))
        ),
        tags$div( style="display:inline-block; vertical-align: middle;",
                  uiOutput(ns("legendForExprsData"))
        )
      )
    )
  })
  
  
  
  
  callModule(mod_plots_legend_colored_exprs_server,'legend_colored_exprs')
  
  output$legendForExprsData <- renderUI({
    req(input$DS_TabsChoice)
    
    if (input$DS_TabsChoice != "tabExprs"){return(NULL)}
    #mod_legend_colored_exprs_ui("legend_colored_exprs",  settings()$colorsTypeMV)
    #moduleSettings.R de prostar 2.0
    mod_plots_legend_colored_exprs_ui("legend_colored_exprs")
    
  })
  
  
  #----------------------------------------------
  output$tabToShow <- renderUI({
    req(input$DS_TabsChoice)
    req(obj())
    print(paste0('input$DS_TabsChoice', input$DS_TabsChoice))
    switch(input$DS_TabsChoice,
           None = {return(NULL)},
           tabExprs = DT::dataTableOutput(ns("table")),
           tabfData = DT::dataTableOutput(ns("viewfData")),
           tabpData = DT::dataTableOutput(ns("viewpData"))
    )
    
  })
  
  
  
  ##' show pData of the MSnset object
  ##' @author Samuel Wieczorek
  output$viewpData <- DT::renderDataTable({
    req(obj())
    
    data <- as.data.frame(Biobase::pData(obj()))
    #pal <- unique(rv.prostar$settings()$examplePalette)
    #moduleSettings.R de prostar 2.0
    pal <- unique(RColorBrewer::brewer.pal(8,"Dark2"))
    
    dt <- DT::datatable(  data,
                          extensions = c('Scroller', 'Buttons'),
                          rownames=  FALSE,
                          options=list(initComplete = initComplete(),
                                       dom = 'Brtip',
                                       #pageLength=DT_pagelength,
                                       #moduleCC.R de prostar 2.0
                                       pageLength=10,
                                       orderClasses = TRUE,
                                       autoWidth=TRUE,
                                       deferRender = TRUE,
                                       bLengthChange = FALSE,
                                       scrollX = 200,
                                       scrollY = 500,
                                       scroller = TRUE,
                                       #columnDefs = list(
                                       #list(columns.width=c("60px"), columnDefs.targets= c(list(0),list(1),list(2))))
                                       columnDefs = list(list(width='60px',targets= "_all"))
                          )) %>%
      DT::formatStyle(
        columns = colnames(data)[1:2],
        valueColumns = colnames(data)[2],
        backgroundColor = DT::styleEqual(unique(data$Condition), pal[1:length(unique(data$Condition))])
      )
    
  })
  
  
  ##' show fData of the MSnset object in a table
  ##' @author Samuel Wieczorek
  output$viewfData <- DT::renderDataTable({
    req(obj())
    
    
    if ('Significant' %in% colnames(Biobase::fData(obj()))){
      dat <- DT::datatable(as.data.frame(Biobase::fData(obj())),
                           rownames = TRUE,
                           extensions = c('Scroller', 'Buttons', 'FixedColumns'),
                           options=list(initComplete = initComplete(),
                                        dom='Bfrtip',
                                        pageLength=10,
                                        orderClasses = TRUE,
                                        autoWidth=FALSE,
                                        deferRender = TRUE,
                                        bLengthChange = FALSE,
                                        scrollX = 200,
                                        scrollY = 200,
                                        scroller = TRUE,
                                        columns.searchable=F,
                                        fixedColumns = list(leftColumns = 1),
                                        columnDefs = list(list(columns.width=c("60px"),
                                                               columnDefs.targets=c(list(0),list(1),list(2)))))) %>%
        DT::formatStyle(columns = 'Significant',
                        target = 'row',
                        background = DT::styleEqual(1, 'lightblue'))
    } else {
      dat <- DT::datatable(as.data.frame(Biobase::fData(obj())),
                           rownames = TRUE,
                           extensions = c('Scroller', 'Buttons', 'FixedColumns'),
                           options=list(initComplete = initComplete(),
                                        dom='Bfrtip',
                                        pageLength=10,
                                        deferRender = TRUE,
                                        bLengthChange = FALSE,
                                        scrollX = 200,
                                        scrollY = 600,
                                        scroller = TRUE,
                                        orderClasses = TRUE,
                                        autoWidth=FALSE,
                                        columns.searchable=F,
                                        fixedColumns = list(leftColumns = 1),
                                        columnDefs = list(list(columns.width=c("60px"),
                                                               columnDefs.targets=c(list(0),list(1),list(2))))))
    }
    
    return(dat)
  })
  
  
  
  #################
  output$table <- DT::renderDataTable({
    req(obj())
    df <- getDataForExprs(obj())
    print(head(df))
    dt <- datatable( df,
                     rownames=TRUE,
                     extensions = c('Scroller', 'Buttons', 'FixedColumns'),
                     options = list(
                       dom = 'Bfrtip',
                       initComplete = initComplete(),
                       displayLength = 20,
                       deferRender = TRUE,
                       bLengthChange = FALSE,
                       scrollX = 200,
                       scrollY = 600,
                       scroller = TRUE,
                       ordering=FALSE,
                       server = TRUE,
                       fixedColumns = list(leftColumns = 1),
                       columnDefs = list(list(targets = c(((ncol(df)/2)+1):ncol(df)), visible = FALSE)))) %>%
      DT::formatStyle(
        colnames(df)[1:(ncol(df)/2)],
        colnames(df)[((ncol(df)/2)+1):ncol(df)],
        #backgroundColor = DT::styleEqual(c("POV", "MEC"), c(rv.prostar$settings()$colorsTypeMV$POV, rv.prostar$settings()$colorsTypeMV$MEC)),
        backgroundColor = DT::styleEqual(c("POV", "MEC"), c("lightblue", orangeProstar)),
        backgroundSize = '98% 48%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
    
    
    dt
  })
  
  
  
  
  
}
    
## To be copied in the UI
# mod_plots_msnset_explorer_ui("msnset_explorer_ui_1")
    
## To be copied in the server
# callModule(mod_plots_msnset_explorer_server, "msnset_explorer_ui_1")
 
