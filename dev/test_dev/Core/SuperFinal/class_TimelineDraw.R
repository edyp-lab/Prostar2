TimelineDraw <- R6Class(
  "TimelineDraw",
  private=list(
    ns = NULL,
    length = NULL,
    style = NULL,
    mandatory = NULL,
    
    GetCSSCode = function(){
      if (verbose) cat(paste0(class(self)[1], '::GetCSSCode()\n'))
      file <- paste0('./Timelines/timeline', private$style, '.sass')
      #code <- code_sass_timeline[[paste0('self$style',self$style)]],"\n")
      code <- strsplit(readLines(file),"\n")
      firstLine <- code[[1]][1]
      prefix <- substr(firstLine, 1, unlist(gregexpr(pattern =':',firstLine)))
      suffix <- substr(firstLine, unlist(gregexpr(pattern =';',firstLine)), nchar(firstLine))
      code[[1]][1] <- paste0(prefix, private$length, suffix, collapse='')
      code <- paste(unlist(code), collapse = '')
      code
    },
    
    GetFirstMandatoryNotValidated = function(status, mandatory){
      first <- NULL
      first <- unlist((lapply(1:length(status), 
                              function(x){mandatory[x]&&!status[x]})))
      if (sum(first) > 0)
        min(which(first == TRUE))
      else
        NULL
    },
    
    BuildTimeline2 = function(status, pos, enabled){
      if (verbose) cat(paste0(class(self)[1], '::BuildTimeline2()) from ', self$id, '\n'))
      
      tl_status <- rep('undone', private$length)
      tl_status[which(private$mandatory)] <- 'mandatory'
      tl_status[which(unlist(status) == global$VALIDATED)] <- 'completed'
      tl_status[which(unlist(status) == global$SKIPPED)] <- 'skipped'
      
      for (i in 1:length(enabled))
        if (!enabled[i])
          tl_status[i] <- paste0(tl_status[i], 'Disabled')
      
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
                      names(private$mandatory)[i],
                      "</h4></div></li>")
      }
      
      txt <- paste0(txt,"</ul>")
      txt
    }
  ),
  public = list(
    id = NULL,

    initialize = function(id, mandatory, style=2) {
      self$id <- id
      private$style <- style
      private$ns <- NS(self$id)
      private$mandatory <- mandatory
      private$length <- length(private$mandatory)
      },

    
    ui = function() {
      tagList(
        uiOutput(private$ns('load_CSS')),
        uiOutput(private$ns('show_TL'))
        )
      },
    
    server = function(status, position, enabled) {
      
      if (verbose) cat(paste0(class(self)[1], '::server()\n'))
      
      moduleServer(self$id, function(input, output, session) {
        
        if (verbose) cat(paste0(class(self)[1], '::moduleServer()\n'))

        output$load_CSS <- renderUI({
          if (verbose) cat(paste0(class(self)[1], '::output$load_CSS\n'))
          shinyjs::inlineCSS(sass::sass(private$GetCSSCode()))
          })
        
        output$show_TL <- renderUI({
          if (verbose) cat(paste0(class(self)[1], '::output$show_TLS\n'))
          HTML(private[[paste0('BuildTimeline', private$style)]](status(), position(), enabled()))
        })
        }
      )
      }
  )
)