TimelineDraw <- R6Class(
  "TimelineDraw",
  private=list(verbose = T,
               length = NULL,
               global = list(VALIDATED = 1,
                             UNDONE = 0,
                             SKIPPED = -1,
                             RESETED = 2),
               reset_OK = NULL),
  public = list(id = NULL,
                
                style = NULL,
                steps = NULL,
                rv = reactiveValues(
                  current.pos = 1
                ),
                initialize = function(id, steps, style) {
                  self$id = id
                  self$style <- style
                  self$steps <- steps
                }, 
                
                ui = function() {
                  ns <- NS(self$id)
                  wellPanel(style="background-color: orange;",
                            h3('TimelineDraw'),
                            uiOutput(ns('show')),
                            uiOutput(ns('load_CSS')),
                            uiOutput(ns('show_TL'))
                  )
                },
                
                server = function(status, position) {
                  ns <- NS(self$id)
                  moduleServer(self$id, function(input, output, session) {
                    
                    observeEvent(self$steps(),{private$length <- length(self$steps())})
                    
                    output$show <- renderUI({
                      tagList(
                        p(paste0('status = ', paste0(status(), collapse=' '))),
                        p(paste0('current.pos = ', position()))
                      )
                    })
                    
                    output$load_CSS <- renderUI({
                      #req(self$steps)
                      shinyjs::inlineCSS(sass::sass(self$GetCSSCode()))
                    })
                    
                    output$show_TL <- renderUI({ 
                      #req(self$steps)
                      HTML(self[[paste0('BuildTimeline', self$style)]](status(), position()))
                    })
                    
                  }
                  )
                },
                
                BuildTimeline2 = function(status, pos){
                  print(paste0('In buildTimeline, new value for status() : ', paste0(status,collapse=' ')))
                  #browser()
                  tl_status <- rep('', private$length)
                  tl_status[which(unlist(self$steps()))] <- 'mandatory'
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
                                  names(self$steps())[i],
                                  "</h4></div></li>")
                  }
                  txt <- paste0(txt,"</ul>")
                  
                  txt
                },
                
                GetCSSCode = function(){
                  file <- paste0('./Timelines/timeline',self$style, '.sass')
                  #code <- code_sass_timeline[[paste0('private$style',private$style)]],"\n")
                  code <- strsplit(readLines(file),"\n")
                  firstLine <- code[[1]][1]
                  prefix <- substr(firstLine, 1, unlist(gregexpr(pattern =':',firstLine)))
                  suffix <- substr(firstLine, unlist(gregexpr(pattern =';',firstLine)), nchar(firstLine))
                  
                  code[[1]][1] <- paste0(prefix, private$length, suffix, collapse='')
                  code <- paste(unlist(code), collapse = '')
                  code
                }
  )
)