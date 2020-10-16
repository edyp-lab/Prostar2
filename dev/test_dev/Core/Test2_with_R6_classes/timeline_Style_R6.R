VALIDATED <- 1
UNDONE <- 0
SKIPPED <- -1
RESETED <- 2

#Timeline_Style_R6.R
TimelineStyle = R6Class(
  "TimelineStyle",
  private = list(
    verbose = T,
    length = NULL,
    status = NULL
  ),
  public = list(
    # attributes
    id = NULL,
    
    # initializer
    initialize = function(id){
      self$id = id
      },
    
    # UI
    ui = function(){
      if(private$verbose)
        print(paste0('TL(style) : ui()'))
      
      # the ns function here will prepend a prefix to all the ids in the app.
      ns = NS(self$id)
      
      tagList(
        uiOutput(ns('load_CSS')),
        uiOutput(ns('show_TL'))
      )
    },
    
    # server
    server = function(input, output, session, style, steps, status, pos){
      ns = NS(self$id)
      
      observeEvent(steps, { private$length <- length(steps)})
      
      ##
      ## Functions defining timeline and styles
      ##
      output$load_CSS <- renderUI({
        if(private$verbose)
          print(paste0('TL style : load_css_style'))
        
        req(private$length)
        req(style != 3)
        
        file <- paste0('./Timelines/timeline',style, '.sass')
        #code <- code_sass_timeline[[paste0('private$style',private$style)]],"\n")
        code <- strsplit(readLines(file),"\n")
        firstLine <- code[[1]][1]
        prefix <- substr(firstLine, 1, unlist(gregexpr(pattern =':',firstLine)))
        suffix <- substr(firstLine, unlist(gregexpr(pattern =';',firstLine)), nchar(firstLine))
        
        code[[1]][1] <- paste0(prefix, private$length, suffix, collapse='')
        
        shinyjs::inlineCSS(sass::sass(paste(unlist(code), collapse = '')))
        
      })
      
      
      output$show_TL <- renderUI({ ns(paste0('timeline', style))})
      
      output$timeline2 <- renderUI({
        req(private$length)
        if(private$verbose)
          print(paste0('TL(style) : timeline2. status = ', paste0(status, collapse=' ')))
        
        private$status <- rep('', private$length)
        #browser()
        if( !is.null(steps))
          private$status[which(unlist(steps))] <- 'mandatory'
        
        #status <- rep('',length(config$stepsNames))
        private$status[which(unlist(status) == VALIDATED)] <- 'complete'
        
        #Compute the skipped steps
        private$status[which(unlist(status) == SKIPPED)] <- 'skipped'
        
        active  <- rep('', private$length)
        active[pos] <- 'active'
        
        txt <- "<ul class='timeline' id='timeline'>"
        for (i in 1:private$length){
          txt <- paste0(txt, "<li class='li ",private$status[i]," ",active[i],"'><div class='timestamp'></div><div class='status'><h4>", names(steps)[i],"</h4></div></li>")
        }
        txt <- paste0(txt,"</ul>")
        
        HTML(txt)
      })
      
      
    },
    
    # call
    call = function(input, ouput, session, style, steps, status, pos){
      if(private$verbose)
        print(paste0('TL style : call()'))
      
      callModule(self$server, self$id, style, steps, status, pos)
    }
  )
)
