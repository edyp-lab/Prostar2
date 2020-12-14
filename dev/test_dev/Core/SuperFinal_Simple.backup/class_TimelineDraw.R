TimelineDraw <- R6Class(
  "TimelineDraw",
  private=list(),
  public = list(
    id = NULL,
    ns = NULL,
    verbose = T,
    length = NULL,
    style = NULL,
    mandatory = NULL,
    
    
    initialize = function(id, mandatory, style=2) {
      #browser()
      self$id <- id
      self$style <- style
      self$ns <- NS(self$id)
      self$mandatory <- mandatory
      self$length <- length(self$mandatory)
      },
    
    GetCSSCode = function(){
      cat(paste0(class(self)[1], '::GetCSSCode()\n'))
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
      cat(paste0(class(self)[1], '::BuildTimeline2()) from ', self$id, '\n'))
      
      #browser()
      
      tl_status <- rep('undone', self$length)
      tl_status[which(self$mandatory)] <- 'mandatory'
      tl_status[which(unlist(status) == global$VALIDATED)] <- 'completed'
      tl_status[which(unlist(status) == global$SKIPPED)] <- 'skipped'
      #browser()
      
      for (i in 1:length(enabled))
        if (!enabled[i])
          tl_status[i] <- paste0(tl_status[i], 'Disabled')

       cat(paste0(paste0(tl_status,collapse=', '), '\n'))
       
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
      #wellPanel(
      #style="background-color: orange;",
      tagList(
        uiOutput(self$ns('load_CSS')),
        uiOutput(self$ns('show_TL'))
        )
      #)
      },
    
    server = function(status, position, enabled) {
      
      cat(paste0(class(self)[1], '::server()\n'))
      
      moduleServer(self$id, function(input, output, session) {
        
        cat(paste0(class(self)[1], '::moduleServer()\n'))

        output$load_CSS <- renderUI({
          cat(paste0(class(self)[1], '::output$load_CSS\n'))
          shinyjs::inlineCSS(sass::sass(self$GetCSSCode()))
          })
        
        output$show_TL <- renderUI({
          cat(paste0(class(self)[1], '::output$show_TLS\n'))
          HTML(self[[paste0('BuildTimeline', self$style)]](status(), position(), enabled()))
        })
        }
      )
      }
  )
)