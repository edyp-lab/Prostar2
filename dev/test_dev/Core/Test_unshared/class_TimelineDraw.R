TimelineDraw <- R6Class(
  "TimelineDraw",
  private=list(),
  public = list(id = NULL,
                
                initialize = function(id, mandatory, style) {
                  #browser()
                  self$id <- id
                  self$style <- style
                  self$mandatory <- mandatory
                  self$length <- length(self$mandatory)
                },
                
                verbose = T,
                length = NULL,
                style = NULL,
                mandatory = NULL,
                
                global = list(VALIDATED = 1,
                              UNDONE = 0,
                              SKIPPED = -1,
                              RESETED = 2),
                
                GetCSSCode = function(){
                  file <- paste0('./Timelines/timeline',self$style, '.sass')
                  #code <- code_sass_timeline[[paste0('self$style',self$style)]],"\n")
                  code <- strsplit(readLines(file),"\n")
                  firstLine <- code[[1]][1]
                  prefix <- substr(firstLine, 1, unlist(gregexpr(pattern =':',firstLine)))
                  suffix <- substr(firstLine, unlist(gregexpr(pattern =';',firstLine)), nchar(firstLine))
                  
                  code[[1]][1] <- paste0(prefix, self$length, suffix, collapse='')
                  code <- paste(unlist(code), collapse = '')
                  code
                },
                
                BuildTimeline2 = function(status, pos){
                  tl_status <- rep('', self$length)
                  tl_status[which(self$mandatory)] <- 'mandatory'
                  tl_status[which(unlist(status) == self$global$VALIDATED)] <- 'complete'
                  tl_status[which(unlist(status) == self$global$SKIPPED)] <- 'skipped'
                  
                  active  <- rep('', self$length)
                  active[pos] <- 'active'
                  
                  txt <- "<ul class='timeline' id='timeline'>"
                  for (i in 1:self$length){
                    txt <- paste0(txt, 
                                  "<li class='li ",
                                  tl_status[i],
                                  " ",
                                  active[i],
                                  "'><div class='timestamp'></div><div class='status'><h4>", 
                                  names(self$mandatory)[i],
                                  "</h4></div></li>")
                  }
                  txt <- paste0(txt,"</ul>")
                  
                  txt
                },
                
                ui = function() {
                  ns <- NS(self$id)
                  wellPanel(style="background-color: orange;",
                            uiOutput(ns('load_CSS')),
                            uiOutput(ns('show_TL'))
                  )
                },
                
                server = function(status, position) {
                  ns <- NS(self$id)
                  moduleServer(self$id, function(input, output, session) {
                    
                    output$load_CSS <- renderUI({
                      shinyjs::inlineCSS(sass::sass(self$GetCSSCode()))
                    })
                    
                    output$show_TL <- renderUI({ 
                      HTML(self[[paste0('BuildTimeline', self$style)]](status(), position()))
                    })
                    
                  }
                  )
                }
  )
)