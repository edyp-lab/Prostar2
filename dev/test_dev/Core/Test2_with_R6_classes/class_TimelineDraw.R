TimelineDraw <- R6Class(
  "TimelineDraw",
  private=list(verbose = T,
               length = NULL,
               style = NULL,
               steps = NULL,
               
               global = list(VALIDATED = 1,
                             UNDONE = 0,
                             SKIPPED = -1,
                             RESETED = 2),
               
               GetCSSCode = function(){
                 file <- paste0('./Timelines/timeline',private$style, '.sass')
                 #code <- code_sass_timeline[[paste0('private$style',private$style)]],"\n")
                 code <- strsplit(readLines(file),"\n")
                 firstLine <- code[[1]][1]
                 prefix <- substr(firstLine, 1, unlist(gregexpr(pattern =':',firstLine)))
                 suffix <- substr(firstLine, unlist(gregexpr(pattern =';',firstLine)), nchar(firstLine))
                 
                 code[[1]][1] <- paste0(prefix, private$length, suffix, collapse='')
                 code <- paste(unlist(code), collapse = '')
                 code
               },
               
               BuildTimeline2 = function(status, pos){
                 tl_status <- rep('', private$length)
                 tl_status[which(unlist(private$steps()))] <- 'mandatory'
                 tl_status[which(unlist(status) == private$global$VALIDATED)] <- 'complete'
                 tl_status[which(unlist(status) == private$global$SKIPPED)] <- 'skipped'
                 
                 active  <- rep('', private$length)
                 active[pos] <- 'active'
                 
                 txt <- "<ul class='timeline' id='timeline'>"
                 for (i in 1:private$length){
                   txt <- paste0(txt, 
                                 "<li class='li ",
                                 tl_status[i],
                                 " ",
                                 active[i],
                                 "'><div class='timestamp'></div><div class='status'><h4>", 
                                 names(private$steps())[i],
                                 "</h4></div></li>")
                 }
                 txt <- paste0(txt,"</ul>")
                 
                 txt
               }
               ),
  public = list(id = NULL,
                
                initialize = function(id, steps, style) {
                  self$id <- id
                  private$style <- style
                  private$steps <- steps
                  private$length <- length(private$steps())
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
                      shinyjs::inlineCSS(sass::sass(private$GetCSSCode()))
                    })
                    
                    output$show_TL <- renderUI({ 
                      HTML(private[[paste0('BuildTimeline', private$style)]](status(), position()))
                    })
                    
                  }
                  )
                }
  )
)