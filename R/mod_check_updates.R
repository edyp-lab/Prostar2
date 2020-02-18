# Module UI
  
#' @title   mod_check_updates_ui and mod_check_updates_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @return xxxx
#' @rdname mod_check_updates
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_check_updates_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("baseVersions")),
    mod_format_DT_ui(ns('tab_versions')),
    br(), br(),
    uiOutput(ns("infoForNewVersions"))
    
  )
}
    
# Module Server
    
#' @rdname mod_check_updates
#' @export
#' @keywords internal
#' @import DT
#' @importFrom BiocManager version
#' @importFrom utils compareVersion    
mod_check_updates_server <- function(input, output, session){
  ns <- session$ns
  
  output$baseVersions <- renderUI({
    
    tagList(
      tags$p(R.version.string, style="font-size: 16px"),
      tags$p(paste0("Bioconductor Release ",as.character(BiocManager::version())), style="font-size: 16px"),
      tags$br()
    )
  })
  
  
  callModule(mod_format_DT_server,'tab_versions', table2show=reactive({getPackagesVersions2()}))
  
  
  ##
  ##
  output$infoForNewVersions <- renderUI({
    df <- getPackagesVersions2()
    if (sum(grepl("(Out of date)",df[,1])) >= 1) {
      
      tagList(
        p(style="font-size: 16px", "Even though it remains possible to work with the current package versions, updates are advised. 
         If you use the server or the stand-alone versions, please proceed via the Bioconductor."),
        
        zipVersion <- substr(GetOnlineZipVersion(), 9, regexpr(".zip",GetOnlineZipVersion())[1] -1),
        prostarVersion <- installed.packages(lib.loc=Prostar.loc)["Prostar","Version"],
        if (compareVersion(zipVersion,prostarVersion) == 1){
          p(style="font-size: 16px", "If you use the Zero-install version, please download the latest zip file on our website ",
            tags$a("(www.prostar-proteomics.org)", href="http://www.prostar-proteomics.org", target="_blank")
          )
        } else {
          p(style="font-size: 16px", "If you use the Zero-install version, the new zip file (Prostar Zero Install) will be available soon on our website ",
            tags$a("(www.prostar-proteomics.org)", href="http://www.prostar-proteomics.org", target="_blank")
          )
        }
      )
    }
  })
  
  
  
  GetBioconductorVersions <- function(){
    ll.versions <- list(Prostar = "NA",
                        DAPAR = "NA",
                        DAPARdata = "NA")
    
    DAPARdata.version <- Prostar.version <- DAPAR.version <- NULL
    tryCatch({
      require(XML)
      Prostar.html <- readHTMLTable("http://bioconductor.org/packages/release/bioc/html/Prostar.html")
      DAPAR.html <- readHTMLTable("http://bioconductor.org/packages/release/bioc/html/DAPAR.html")
      DAPARdata.html <- readHTMLTable("http://bioconductor.org/packages/release/data/experiment/html/DAPARdata.html")
      ll.versions$Prostar <-as.character(Prostar.html[[3]][2][1,])
      ll.versions$DAPAR <-as.character(DAPAR.html[[3]][2][1,])
      ll.versions$DAPARdata <- as.character(DAPARdata.html[[3]][2][1,])
    }, warning = function(w) {
      return()
    }, error = function(e) {
      return()
    }, finally = {
      #cleanup-code 
    })
    
    
    return (ll.versions)
  }
  
  
  GetLocalVersions <- function(){
    local.version <- list()
    #loc.pkgs <-c("Prostar.loc", "DAPAR.loc", "DAPARdata.loc")
    local.version <- list(Prostar = installed.packages(lib.loc=Prostar.loc)["Prostar","Version"],
                          DAPAR = installed.packages(lib.loc=DAPAR.loc)["DAPAR","Version"],
                          DAPARdata = installed.packages(lib.loc=DAPARdata.loc)["DAPARdata","Version"])
    
    return(local.version)
  }
  
  
  
  getPackagesVersions2 <- reactive({
    
    outOfDate <- "(Out of date)"
    dev <- "(Devel)"
    
    bioconductor.version <-GetBioconductorVersions()
    local.version <- GetLocalVersions()
    names <- c(as.character(tags$a(href="http://www.bioconductor.org/packages/release/bioc/html/Prostar.html", "Prostar")), 
               as.character(tags$a(href="http://www.bioconductor.org/packages/release/bioc/html/DAPAR.html", "DAPAR")), 
               as.character(tags$a(href="http://www.bioconductor.org/packages/release/data/experiment/html/DAPARdata.html", "DAPARdata")))
    
    
    df <- data.frame("Name" = names,
                     "Installed packages"= unlist(local.version), 
                     "Bioc release" =  unlist(bioconductor.version),
                     stringsAsFactors = FALSE)
    
    
    if (!is.null(local.version$Prostar) && !is.null(local.version$DAPAR)) {
      tryCatch({
        
        compare.prostar <- compareVersion(local.version$Prostar,bioconductor.version$Prostar)
        if (compare.prostar == 0){}
        if (compare.prostar == 1){
          df[1,"Name"] <-   paste(names[1],  "<strong>",dev, "</strong>", sep=" ")
        }
        if (compare.prostar==-1){
          df[1,"Name"] <-   paste(names[1], "<strong>", outOfDate, "</strong>", sep=" ")
        }
        
        compare.dapar <- compareVersion(local.version$DAPAR,bioconductor.version$DAPAR)
        if (compare.dapar == 0){}
        if (compare.dapar == 1){df[2,"Name"] <-   paste(names[2],  "<strong>",dev, "</strong>", sep=" ")}
        if (compare.dapar ==-1){
          df[2,"Name"] <-   paste(names[2],  "<strong>",outOfDate, "</strong>", sep=" ")
        }
        
        if (compareVersion(local.version$DAPARdata,bioconductor.version$DAPARdata) == 0){}
        if (compareVersion(local.version$DAPARdata , bioconductor.version$DAPARdata) == 1){
          df[3,"Name"] <-   paste(names[3],  "<strong>",dev, "</strong>", sep=" ")
        }
        if (compareVersion(local.version$DAPARdata , bioconductor.version$DAPARdata)==-1){
          df[3,"Name"] <-   paste(names[3],  "<strong>",outOfDate, "</strong>", sep=" ")
        }
        df[, "Bioc.release"] <- unlist(biocPkgs)
      }, warning = function(w) {
        return()
      }, error = function(e) {
        return()
      }, finally = {
        #cleanup-code 
      })
      
    }
    
    
    df
    
  })
  
}
    
## To be copied in the UI
# mod_check_updates_ui("check_updates_ui_1")
    
## To be copied in the server
# callModule(mod_check_updates_server, "check_updates_ui_1")
 
