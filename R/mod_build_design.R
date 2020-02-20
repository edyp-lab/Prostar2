# Module UI
  
#' @title   mod_build_design_ui and mod_build_design_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_build_design
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_build_design_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$p("If you do not know how to fill the experimental design, you can click
           on the '?' next to each design in the list that appear once the conditions 
           are checked or got to the ", 
           actionLink(ns("linkToFaq1"), "FAQ", style="background-color: white"), 
           " page."),
    shinyBS::bsModal("modalLinkFAQ", "NULL", ns("linkToFaq1"), size = "large", moduleInsertMarkdownUI(ns('FAQ_MD2')),
                     tags$head(tags$style("#window .modal-footer{display:none}
                                             .modal-header{display:none}"))),
    fluidRow(
      column(width=6,tags$b("1 - Fill the \"Condition\" column to identify the conditions to compare.")),
      column(width=6,uiOutput(ns("UI_checkConditions"))  )
    ),
    fluidRow(
      column(width=6,uiOutput(ns("UI_hierarchicalExp"))),
      column(width=6,uiOutput(ns("checkDesign") ))
    ),
    hr(),
    selectInput(ns("convert_reorder"), "Order by conditions ?",
                choices=c("No"="No", "Yes"="Yes"),
                width="100px"),
    tags$div(
      
      tags$div(style="display:inline-block; vertical-align: top;",
               uiOutput(ns("viewDesign"),width="100%")
      ),
      tags$div(style="display:inline-block; vertical-align: top;",
               #shinyjs::hidden(div(id = ns("showExamples"), uiOutput("designExamples") ))
               shinyjs::hidden(uiOutput(ns("designExamples")))
      )
    )
   )
}
    
# Module Server
    
#' @rdname mod_build_design
#' @export
#' @keywords internal
    
mod_build_design_server <- function(input, output, session, table){
  ns <- session$ns
  
  rv.buildDesign <- reactiveValues(
    hot = NULL,
    designChecked = NULL,
    newOrder = NULL,
    conditionsChecked = NULL,
    designSaved = NULL
  )
  
  
  
  
  
  color_renderer <- reactive({
    conds <- rv.buildDesign$hot$Condition
    pal <- rv.prostar$settings()$examplePalette
    txt <- "function (instance, td, row, col, prop, value, cellProperties) {
  Handsontable.renderers.TextRenderer.apply(this, arguments);"
    c <- 1
    for (i in 1:length(conds)){
      if (conds[i] != "")
        txt <- paste0(txt, "if(row==",(i-1)," && col==",c, ") {td.style.background = '",pal[which(conds[i] == unique(conds))],"';}")
    }
    txt <- paste0(txt,"}")
    
    return (txt)
  })
  
  
  
  
  
  #----------------------------------------------------------
  observeEvent(input$btn_checkConds,{
    input$convert_reorder
    
    if (length(grep("Bio.Rep", colnames(rv.buildDesign$hot))) > 0)  { return(NULL)}
    
    if (isTRUE(input$convert_reorder)) {
      rv.buildDesign$newOrder <- order(rv.buildDesign$hot["Condition"])
      rv.buildDesign$hot <- rv.buildDesign$hot[rv.buildDesign$newOrder,]
    }
    
    rv.buildDesign$conditionsChecked <- check.conditions(rv.buildDesign$hot$Condition)
    
  })
  
  
  
  #----------------------------------------------------------
  observeEvent(input$eData.box,{
    rv.buildDesign$hot  <- data.frame(Sample.name = as.character(input$eData.box),
                                      Condition = rep("",length(input$eData.box)),
                                      stringsAsFactors = FALSE)
    
    
  })
  
  #-------------------------------------------------------------
  output$hot <- rhandsontable::renderRHandsontable({
    rv.buildDesign$hot
    input$chooseExpDesign
    
    if (is.null(rv.buildDesign$hot)){
      rv.buildDesign$hot  <- data.frame(Sample.name = as.character(input$eData.box),
                                        Condition = rep("",length(input$eData.box)),
                                        stringsAsFactors = FALSE)
    }
    
    hot <- rhandsontable::rhandsontable(rv.buildDesign$hot,rowHeaders=NULL, 
                                        fillHandle = list(direction='vertical', 
                                                          autoInsertRow=FALSE,
                                                          maxRows=nrow(rv.buildDesign$hot))) %>%
      rhandsontable::hot_rows(rowHeights = 30) %>%
      rhandsontable::hot_context_menu(allowRowEdit = TRUE, 
                                      allowColEdit = FALSE,
                                      allowInsertRow = FALSE,
                                      allowInsertColumn = FALSE,
                                      allowRemoveRow = TRUE,
                                      allowRemoveColumn = FALSE,
                                      autoInsertRow=FALSE     ) %>%
      rhandsontable:: hot_cols(renderer = color_renderer()) %>%
      rhandsontable::hot_col(col = "Sample.name", readOnly = TRUE)
    
    if (!is.null(input$chooseExpDesign)) {
      switch(input$chooseExpDesign,
             FlatDesign = {
               if ("Bio.Rep" %in% colnames(rv.buildDesign$hot))
                 hot <- hot %>% rhandsontable::hot_col(col = "Bio.Rep", readOnly = TRUE)
             },
             twoLevelsDesign = {
               if ("Tech.Rep" %in% colnames(rv.buildDesign$hot))
                 hot <- hot %>% rhandsontable::hot_col(col =  "Tech.Rep", readOnly = TRUE)
             } ,
             threeLevelsDesign = {
               if ("Analyt.Rep" %in% colnames(rv.buildDesign$hot))
                 hot <- hot %>% rhandsontable::hot_col(col = "Analyt.Rep", readOnly = TRUE)
             }
      )
    }
    hot
    
  })
  
  
  
  
  
  #----------------------------------------------------------
  output$UI_checkConditions  <- renderUI({
    
    req(rv.buildDesign$hot)
    rv.buildDesign$conditionsChecked
    input$convert_reorder
    
    if ((sum(rv.buildDesign$hot$Condition == "")==0) && (input$convert_reorder != "None")){
      tags$div(
        tags$div(style="display:inline-block;",
                 actionButton(ns("btn_checkConds"), "Check conditions", class = actionBtnClass)
        ),
        
        tags$div(style="display:inline-block;",
                 if(!is.null(rv.buildDesign$conditionsChecked)){
                   
                   if (isTRUE(rv.buildDesign$conditionsChecked$valid)){
                     img <- "images/Ok.png"
                     txt <- "Correct conditions"
                   }else {
                     img <- "images/Problem.png"
                     txt <- "Invalid conditions"
                   }
                   tagList(
                     tags$div(
                       tags$div(style="display:inline-block;",tags$img(src = img, height=25)),
                       tags$div(style="display:inline-block;",tags$p(txt))
                     ),
                     if(!isTRUE(rv.buildDesign$conditionsChecked$valid)){
                       tags$p(rv.buildDesign$conditionsChecked$warn)
                     }
                   )
                 }
        )
      )
    } else {
      tagList(
        br(),
        br()
      )
      
    }
  })
  
  
  
  #------------------------------------------------------------------------------
  output$UI_hierarchicalExp <- renderUI({
    req(rv.buildDesign$conditionsChecked)
    if (!isTRUE(rv.buildDesign$conditionsChecked$valid)){return(NULL)
    } else {
      tagList(
        div(
          div(
            # edit1
            style="display:inline-block; vertical-align: middle;",
            tags$b("2 - Choose the type of experimental design and complete it accordingly")
          ),
          div(
            # edit2
            style="display:inline-block; vertical-align: middle;",
            tags$button(id=ns("btn_helpDesign"), tags$sup("[?]"), class="Prostar_tooltip")
          )
        ),
        
        radioButtons(ns("chooseExpDesign"), "",
                     choices = c("Flat design (automatic)" = "FlatDesign" ,
                                 "2 levels design (complete Bio.Rep column)" = "twoLevelsDesign" ,
                                 "3 levels design (complete Bio.Rep and Tech.Rep columns)" = "threeLevelsDesign" ),
                     selected=character(0))
      )
    }
    
  })
  
  
  
  
  
  
  #------------------------------------------------------------------------------
  output$viewDesign <- renderUI({
    
    rv.buildDesign$designSaved
    if (isTRUE(rv.buildDesign$designSaved)){return(NULL)}
    
    tagList(
      h4("Design"),
      rHandsontableOutput(ns("hot"))
    )
  })
  
  
  callModule(moduleDesignExample,"buildDesignExampleThree", reactive({3}), n_rows = reactive({nrow(rv.buildDesign$hot)}))
  callModule(moduleDesignExample,"buildDesignExampleTwo", reactive({2}), n_rows = reactive({nrow(rv.buildDesign$hot)}))
  
  
  #------------------------------------------------------------------------------
  output$designExamples <- renderUI({
    input$chooseExpDesign
    print(input$chooseExpDesign)
    switch(input$chooseExpDesign,
           FlatDesign = 
             {
               tags$p("There is nothing to do for the flat design: the 'Bio.Rep' column is already filled.")
             },
           twoLevelsDesign =  {
             tagList(
               h4("Example for a 2-levels design"),
               moduleDesignExampleUI(ns("buildDesignExampleTwo"))
             )
           },
           threeLevelsDesign =  {
             tagList(
               h4("Example for a 3-levels design"),
               moduleDesignExampleUI(ns("buildDesignExampleThree"))
             )
           }
    )
  })
  
  
  #------------------------------------------------------------------------------
  observe({
    shinyjs::onclick("btn_helpDesign",{
      print("onClick : btn_helpDesign")
      if (!is.null(input$chooseExpDesign))
      {
        shinyjs::toggle(id = "designExamples", anim = TRUE)
      }
    }
    )
  })
  
  
  
  
  #------------------------------------------------------------------------------
  observeEvent(input$chooseExpDesign, {
    rv.buildDesign$hot
    rv.buildDesign$designChecked <- NULL
    switch(input$chooseExpDesign,
           FlatDesign = {
             rv.buildDesign$hot  <- data.frame(rv.buildDesign$hot[,1:2],
                                               Bio.Rep = seq(1:nrow(rv.buildDesign$hot)),
                                               stringsAsFactors = FALSE)
           },
           twoLevelsDesign = {
             rv.buildDesign$hot  <- data.frame(rv.buildDesign$hot[,1:2],Bio.Rep = rep("",nrow(rv.buildDesign$hot)),
                                               Tech.Rep = seq(1:nrow(rv.buildDesign$hot)),
                                               stringsAsFactors = FALSE)
           },
           threeLevelsDesign = {
             #if (length(grep("Tech.Rep", colnames(rv.buildDesign$hot))) > 0) { return(NULL)}
             rv.buildDesign$hot  <- data.frame(rv.buildDesign$hot[,1:2],
                                               Bio.Rep = rep("",nrow(rv.buildDesign$hot)),
                                               Tech.Rep = rep("",nrow(rv.buildDesign$hot)),
                                               Analyt.Rep = seq(1:nrow(rv.buildDesign$hot)),
                                               stringsAsFactors = FALSE)
           }
    )
  })
  
  
  
  
  #------------------------------------------------------------------------------
  observeEvent(input$hot,{ rv.buildDesign$hot <-  rhandsontable::hot_to_r(input$hot)})
  
  
  
  #------------------------------------------------------------------------------
  observeEvent(input$btn_checkDesign,{ 
    rv.buildDesign$designChecked <- check.design(rv.buildDesign$hot)
    print(rv.buildDesign$designChecked)
    
  })
  
  #------------------------------------------------------------------------------
  output$checkDesign <- renderUI({
    req(input$chooseExpDesign)
    rv.buildDesign$designChecked
    req(rv.buildDesign$conditionsChecked)
    
    if(!isTRUE(rv.buildDesign$conditionsChecked$valid)){return(NULL)}
    switch(isolate({input$chooseExpDesign}),
           FlatDesign = {},
           twoLevelsDesign = { if (sum(rv.buildDesign$hot$Bio.Rep == "") > 0) {return(NULL)}},
           threeLevelsDesign = {if ((sum(rv.buildDesign$hot$Bio.Rep == "")+sum(rv.buildDesign$hot$Tech.Rep == "")) > 0) {return(NULL)}}
    )
    
    
    tags$div(
      tags$div(
        style="display:inline-block;",
        actionButton(ns("btn_checkDesign"), "Check design", class = actionBtnClass)
      ),
      
      tags$div(
        style="display:inline-block;",
        if(!is.null(rv.buildDesign$designChecked)){
          
          if (isTRUE(rv.buildDesign$designChecked$valid)){
            shinyjs::enable("createMSnsetButton")
            img <- "images/Ok.png"
            txt <- "Correct design"
            #rvNavProcess$Done[4] <- TRUE
          }else {
            img <- "images/Problem.png"
            txt <- "Invalid design"}
          tagList(
            tags$div(
              tags$div(style="display:inline-block;",tags$img(src = img, height=25)),
              tags$div(style="display:inline-block;",tags$p(txt))
            ),
            if(!isTRUE(rv.buildDesign$designChecked$valid)){
              shinyjs::disable("createMSnsetButton")
              tags$p(rv.buildDesign$designChecked$warn)
            } else {
              shinyjs::enable("createMSnsetButton")
              #rvNavProcess$Done[4] <- TRUE
            }
          )
        } else {
          shinyjs::disable("createMSnsetButton")
        }
      )
      
    )
    
    
  })
  
  
  
  
  
  
  return(reactive({rhandsontable::hot_to_r(input$hot)}))
}
    
## To be copied in the UI
# mod_build_design_ui("build_design_ui_1")
    
## To be copied in the server
# callModule(mod_build_design_server, "build_design_ui_1")
 
