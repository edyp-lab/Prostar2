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
mod_check_updates_server <- function(id){
  
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$baseVersions <- renderUI({
      
      tagList(
        tags$p("Prostar is running on ",R.version.string, style="font-size: 16px"),
        tags$p(paste0("and uses the Bioconductor Release ",as.character(BiocManager::version())), style="font-size: 16px"),
        tags$br()
      )
      
    })
    
    mod_format_DT_server('tab_versions',
                         table2show=reactive({getPackagesVersions()}),
                         style = reactive({NULL}))
    
    output$infoForNewVersions <- renderUI({
      df <- getPackagesVersions()
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
    
    
    
    GetBioconductorVersions <- reactive({
      ll.versions <- list(Prostar = "NA",
                          DAPAR = "NA",
                          DAPARdata = "NA")
      
      DAPARdata.version <- Prostar.version <- DAPAR.version <- NULL
      tryCatch({
        bioc <-available.packages(contrib.url("http://www.bioconductor.org/packages/release/bioc/"))
        ll.versions$Prostar <-bioc['Prostar', "Version"]
        ll.versions$DAPAR <-bioc['DAPAR', "Version"]
        
        biocExperiment <- available.packages(contrib.url("http://www.bioconductor.org/packages/release/data/experiment"))
        ll.versions$DAPARdata <- biocExperiment['DAPARdata', "Version"]
      }, warning = function(w) {
        warning(e)
        return()
      }, error = function(e) {
        print(e)
        return()
      }, finally = {
        #cleanup-code
      })
      
      ll.versions
      
    })
    
    GetLocalVersions <- reactive({
      local.version <- list()
      #loc.pkgs <-c("Prostar.loc", "DAPAR.loc", "DAPARdata.loc")
      ll.packages <- installed.packages()[,"Version"]
      local.version <- list(Prostar = if (!is.null(match('Prostar2', names(ll.packages)))) 
        ll.packages[match('Prostar2', names(ll.packages))] 
        else '-',
        DAPAR = if (!is.null(match('DAPAR2', names(ll.packages)))) 
          ll.packages[match('DAPAR2', names(ll.packages))] 
        else '-',
        DAPARdata = if (!is.null(match('DAPARdata2', names(ll.packages)))) 
          ll.packages[match('DAPARdata2', names(ll.packages))] 
        else '-'
      )
      
      local.version
    })
    
    
    
    getPackagesVersions <- reactive({
      
      outOfDate <- "(Out of date)"
      dev <- "(Devel)"
      
      bioconductor.version <- GetBioconductorVersions()
      local.version <- GetLocalVersions()
      names <- c(as.character(tags$a(href="http://www.bioconductor.org/packages/release/bioc/html/Prostar.html", "Prostar2")),
                 as.character(tags$a(href="http://www.bioconductor.org/packages/release/bioc/html/DAPAR.html", "DAPAR2")),
                 as.character(tags$a(href="http://www.bioconductor.org/packages/release/data/experiment/html/DAPARdata.html", "DAPARdata2")))
      
      
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
    
    
  })
  
  
  
}

## To be copied in the UI
# mod_check_updates_ui("check_updates_ui_1")

## To be copied in the server
# callModule(mod_check_updates_server, "check_updates_ui_1")

