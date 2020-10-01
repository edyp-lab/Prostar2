# Module UI

#' @title   mod_build_design_example_ui and mod_build_design_example_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' @param designLevel An integer
#'
#' @rdname mod_build_design_example
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' @importFrom  rhandsontable rHandsontableOutput 
mod_build_design_example_ui <- function(id){
  ns <- NS(id)
  tagList(
    #uiOutput(ns('title')),
    shinyBS::bsCollapse(id = "collapseFormerReleases", open = NULL,multiple = FALSE,
                        shinyBS::bsCollapsePanel("Level 1", rhandsontable::rHandsontableOutput(ns('showLevel1')),style = "info"),
                        shinyBS::bsCollapsePanel("Level 2", rhandsontable::rHandsontableOutput(ns('showLevel2')),style = "info"),
                        shinyBS::bsCollapsePanel("Level 3", rhandsontable::rHandsontableOutput(ns('showLevel3')),style = "info")
    )
    
    
    #rHandsontableOutput(ns("nlevelsExample"))
  )
}

# Module Server

#' @rdname mod_build_design_example
#' @export
#' @keywords internal
#' @importFrom  rhandsontable renderRHandsontable hot_context_menu hot_cols hot_rows
#' @importFrom  RColorBrewer brewer.pal

mod_build_design_example_server <- function(id){
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    
    pal <- reactiveVal(RColorBrewer::brewer.pal(3,'Dark2'))
    # 
    # observeEvent(designLevel(),{
    #   print(paste0('design level = ', designLevel()))
    # })
    
    
    # output$title <- renderUI({
    #   req(designLevel())
    #   if(designLevel() %in% 1:3){
    #     h4(paste0("Example for a ",designLevel(),"-levels design"))
    #    } else {
    #            warning("This level is not implemented.")
    #   }
    # })
    
    
    
    GetExample_Level1 <- reactive({
      ll <- list()
      ll$df <- data.frame(Sample.name= paste0("Sample ",as.character(1:6)),
                          Condition = c(rep("A", 2), rep("B", 2), rep("C", 2)),
                          Bio.Rep = 1:6,
                          stringsAsFactors = FALSE)
      
      
      ll$color_rend <- paste0("function (instance, td, row, col, prop, value, cellProperties) {
                         Handsontable.renderers.TextRenderer.apply(this, arguments);
                         
                         if(col==1 && (row>=0 && row<=1)) {td.style.background = '",pal()[1], "';}
                         if(col==1 && (row>=2 && row<=3)) {td.style.background = '",pal()[2], "';}
                         if(col==1 && (row>=4 && row<=5)) {td.style.background = '",pal()[3], "';}
                         
                    }")
      ll
    })
    
    
    GetExample_Level2 <- reactive({
      ll <- list()
      
      ll$df <- data.frame(Sample.name= paste0("Sample ",as.character(1:14)),
                          Condition = c(rep("A", 4), rep("B", 4), rep("C", 6)),
                          Bio.Rep = as.integer(c(1,1,2,2,3,3,4,4,5,5,6,6,7,7)),
                          Tech.Rep = c(1:14),
                          stringsAsFactors = FALSE)
      
      
      ll$color <- paste0("function (instance, td, row, col, prop, value, cellProperties) {
                         Handsontable.renderers.TextRenderer.apply(this, arguments);
                         
                         if(col==1 && (row>=0 && row<=3)) {td.style.background = '",pal()[1], "';}
                         if(col==1 && (row>=4 && row<=7)) {td.style.background = '",pal()[2], "';}
                         if(col==1 && (row>=8 && row<=14)) {td.style.background = '",pal()[3], "';}
                         
                         
                         if(col==2 && (row==0||row==1||row==4||row==5||row==8||row==9||row==12||row==13)) 
                         {td.style.background = 'lightgrey';}
                         
                         if(col==3 && (row==0||row==2||row==4||row==6||row==8||row==10||row==12)) 
                         {td.style.background = 'lightgrey';}
                    }")
      ll
    })
    
    
    
    GetExample_Level3 <- reactive({
      ll <- list()
      ll$df <- data.frame(Sample.name= paste0("Sample ",as.character(1:16)),
                          Condition = c(rep( "A", 8), rep("B", 8)),
                          Bio.Rep = as.integer(c(rep(1,4),rep(2,4),rep(3,4),rep(4,4))),
                          Tech.Rep = as.integer(c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8)),
                          Analyt.Rep = c(1:16),
                          stringsAsFactors = FALSE)
      
      ll$color <- paste0("function (instance, td, row, col, prop, value, cellProperties) {
                           Handsontable.renderers.TextRenderer.apply(this, arguments);
                           
                           if(col==1 && (row>=0 && row<=7)) {td.style.background = '",pal()[1], "';}
                           
                           if(col==1 && (row>=8 && row<=15))  {td.style.background = '",pal()[2], "';}
                           
                           if(col==2 && (row==0||row==1||row==2||row==3||row==8||row==9||row==10||row==11)) 
                           {td.style.background = 'lightgrey';}
                           
                           if(col==3 && (row==0||row==1||row==4||row==5|| row==8||row==9||row==12||row==13)) 
                           {td.style.background = 'lightgrey';}
                           
                           
                           if(col==4 && (row==0||row==2||row==4||row==6|| row==8||row==10||row==12||row==14)) 
                           {td.style.background = 'lightgrey';}
                            }")
      
      ll
    })
    
    
    BuildHot <- function(df, color_rend){
      table <- rhandsontable::rhandsontable(df,
                                            rowHeaders=NULL,
                                            fillHandle = list(direction='vertical', 
                                                              autoInsertRow=FALSE,
                                                              maxRows=16 )
      ) %>%
        rhandsontable::hot_rows(rowHeights = 30) %>%
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, 
                                        allowColEdit = FALSE,
                                        allowInsertRow = FALSE,
                                        allowInsertColumn = FALSE,
                                        allowRemoveRow = FALSE,
                                        allowRemoveColumn = FALSE,
                                        autoInsertRow=FALSE) %>%
        rhandsontable::hot_cols(readOnly = TRUE,
                                renderer = color_rend,
                                halign = 'htCenter')
      
      return(table)
    }
    
    
    output$showLevel1 <- rhandsontable::renderRHandsontable({
      df <- GetExample_Level1()$df
      color <- GetExample_Level1()$color
      BuildHot(df, color)
    })
    
    output$showLevel2 <- rhandsontable::renderRHandsontable({
      df <- GetExample_Level2()$df
      color <- GetExample_Level2()$color
      BuildHot(df, color)
    })
    
    output$showLevel3 <- rhandsontable::renderRHandsontable({
      df <- GetExample_Level3()$df
      color <- GetExample_Level3()$color
      BuildHot(df, color)
    })
    
    
    output$nlevelsExample <- rhandsontable::renderRHandsontable({
      req(designLevel())
      #pal <- RColorBrewer::brewer.pal(3,'Dark2')
      if (designLevel() == 1){
        df <- GetExample_Level1()$df
        color_rend <- GetExample_Level1()$color
      } else if (designLevel() == 2){
        df <- GetExample_Level2()$df
        color_rend <- GetExample_Level2()$color
      } else if (designLevel() == 3){
        df <- GetExample_Level3()$df
        color_rend <- GetExample_Level3()$color
      }
      
      rhandsontable::rhandsontable(df,
                                   rowHeaders=NULL,
                                   fillHandle = list(direction='vertical', 
                                                     autoInsertRow=FALSE,
                                                     maxRows=16 )
      ) %>%
        rhandsontable::hot_rows(rowHeights = 30) %>%
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, 
                                        allowColEdit = FALSE,
                                        allowInsertRow = FALSE,
                                        allowInsertColumn = FALSE,
                                        allowRemoveRow = FALSE,
                                        allowRemoveColumn = FALSE,
                                        autoInsertRow=FALSE) %>%
        rhandsontable::hot_cols(readOnly = TRUE,
                                renderer = color_rend,
                                halign = 'htCenter')
      
    })
    
    
  })
  
  
}


## To be copied in the UI
# mod_build_design_example_ui("build_design_example_ui_1")

## To be copied in the server
# callModule(mod_build_design_example_server, "build_design_example_ui_1")

