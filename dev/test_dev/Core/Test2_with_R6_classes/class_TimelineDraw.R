

#Timeline_Style_R6.R
TimelineDraw = R6Class(
  "TimelineDraw",
  private = list(
    verbose = T,
    length = NULL,
    VALIDATED = 1,
    UNDONE = 0,
    SKIPPED = -1,
    RESETED = 2
  ),
  public = list(
    # attributes
    id = NULL,
    style = NULL,
    steps = NULL,

    
    # initializer
    initialize = function(id, steps, style){
      self$id = id
      self$style <- style
      self$steps <- steps
      private$length <- length(steps)
      },
    
    # UI
    ui = function(){
       ns = NS(self$id)
      
      tagList(
        uiOutput(ns('load_CSS')),
        uiOutput(ns('show_TL'))
      )
    },
    
    # server
    server = function(status, position) {
      ns = NS(self$id)
     moduleServer(self$id, function(input, output, session) {
        

      ##
      ## Functions defining timeline and styles
      ##
      output$load_CSS <- renderUI({
        if(private$verbose)
          print(paste0('TL style : load_css_style'))
        
        shinyjs::inlineCSS(sass::sass(self$GetCSSCode()))
      })
      
      
      output$show_TL <- renderUI({ 
        if(private$verbose)
          print(paste0(self$id, 'show_TL<-renderUI()'))

          HTML(self[[paste0('BuildTimeline', self$style)]](status(), self$steps, position()))

        })

      }
    )
    },
    
    BuildTimeline2 = function(status, pos){
      req(private$length)
      
      tl_status <- rep('', private$length)
      #browser()
      if( !is.null(self$steps))
        tl_status[which(unlist(self$steps))] <- 'mandatory'
      
      #status <- rep('',length(config$stepsNames))
      tl_status[which(unlist(status) == private$VALIDATED)] <- 'complete'
      
      #Compute the skipped steps
      tl_status[which(unlist(status) == private$SKIPPED)] <- 'skipped'
      
      active  <- rep('', private$length)
      active[pos] <- 'active'
      
      txt <- "<ul class='timeline' id='timeline'>"
      for (i in 1:private$length){
        txt <- paste0(txt, "<li class='li ",tl_status[i]," ",active[i],"'><div class='timestamp'></div><div class='status'><h4>", names(self$steps)[i],"</h4></div></li>")
      }
      txt <- paste0(txt,"</ul>")
      
      txt
    },
    
    GetCSSCode = function(){
      browser()
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
