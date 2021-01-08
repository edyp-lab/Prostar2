# Module UI
  
#' @title   mod_homepage_ui and mod_homepage_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_homepage
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_homepage_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("citationText")),
    tags$hr(),
    tags$div(
      style="padding: 0 50px; float: left;",
      img(src='www/images/LogoProstarComplet.png', width='150px', height='150px')
    ),
    tags$div(
      style="margin-top: 50px;",
      tags$p("")
    ),
    uiOutput(ns("versionsText")),
    tags$br(), tags$br(),
    uiOutput(ns('NoteForNewVersion')),
    
    #uiOutput("descriptionText")
    #includeMarkdown(URL_ProstarPresentation)
    mod_insert_md_ui(ns("ProstarPresentation_MD"))
  )
}
    
# Module Server
    
#' @rdname mod_homepage
#' @export
#' @keywords internal
    
mod_homepage_server <- function(id){
  
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    mod_insert_md_server("ProstarPresentation_MD",URL_ProstarPresentation)
    
    output$citationText <- renderUI({
      tagList(
        tags$div(style="background-color: lightgrey;",
                 tags$p(class="body",tags$b("Maintaining ProStaR as free software is a heavy and time-consuming
                                        duty. If you use it, please cite the following reference:")),
                 tags$p(tags$i("S. Wieczorek, F. Combes, C. Lazar, Q. Giai-Gianetto, 
                           L. Gatto, A. Dorffer, A.-M. Hesse, Y. Coute, M. Ferro, 
                           C. Bruley and T. Burger."),
                        tags$u("DAPAR & ProStaR: software to perform statistical 
                           analyses in quantitative discovery."),
                        tags$b("Bioinformatics"),", 33(1), 135-136, 2017.",
                        tags$a("http://doi.org/10.1093/bioinformatics/btw580", href="http://doi.org/10.1093/bioinformatics/btw580", target="_blank")
                 )
        )
      )
    })
    
    
    output$versionsText <- renderUI({
      #t <- sessionInfo()
      ll.packages <- installed.packages()[,"Version"]
      
      daparVersion <- if (!is.null(match('DAPAR2', names(ll.packages)))) 
        ll.packages[match('DAPAR2', names(ll.packages))] 
      else '-'
      ProstarVersion <- if (!is.null(match('Prostar2', names(ll.packages)))) 
        ll.packages[match('Prostar2', names(ll.packages))] 
      else '-'
      
      tagList(
        tags$p(class="body",
               tags$b("DAPAR"),
               " and ", 
               tags$b("Prostar"), 
               " form a software suite devoted to the differential analysis of 
           quantitative data resulting from discovery proteomics experiments.", 
               tags$br(),
               "It is composed of two distinct ",
               tags$b("R"),
               " packages:",
               tags$ul(
                 tags$li(tags$p(tags$a("Prostar", href="http://www.bioconductor.org/packages/release/bioc/html/Prostar.html", target="_blank"),paste0(" (version ",
                                                                                                                                                      ProstarVersion,"), which proposes a web-based graphical user interface to DAPAR."))),
                 tags$li(tags$p(tags$a("DAPAR", href="http://www.bioconductor.org/packages/release/bioc/html/DAPAR.html", target="_blank"),paste0(" (version ",
                                                                                                                                                  daparVersion,"), which contains all the routines to analyze and visualize proteomics data.")))
               )
        )
      )
    })
    
    
    output$NoteForNewVersion <- renderUI({
      
      tags$div(
        style="font-size: 16px",
        tags$div( style="display:inline-block; vertical-align: top;",
                  p(style="color: red",'Newer versions of Prostar and/or DAPAR packages have been released. For more information, please go to the page Prostar/Check for updates')
        )
      )
      
      # }
    })
    
    
  })
  
  
}
    
## To be copied in the UI
# mod_homepage_ui("homepage_ui_1")
    
## To be copied in the server
# callModule(mod_homepage_server, "homepage_ui_1")
 
