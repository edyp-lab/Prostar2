#' @title Add row to history
#'
#' @description This function adds a row to the history.
#'
#' @param history A `data.frame` corresponding to the current history.
#' @param step A `character(1)` corresponding to the process name.
#' @param substep A `character(1)` corresponding to the substep name.
#' @param param.name A `character(1)` corresponding to the parameter name.
#' @param value The value of the corresponding parameter.
#' 
#' @return A `data.frame` with one added row
#'
#' @examples
#' history <- InitializeHistory()
#' Add2History(history, "Example step", "First sub-step", "my param", "THE value")
#'
#' @export
#'
Add2History <- function(history, step, substep, param.name, value){
  if (inherits(value, "list")) {
    value <- paste(names(value), unlist(value), collapse = ", ", sep = "=")
  }
  
  if (is.null(value)) {
    value <- NA
  }
  
  history[nrow(history) + 1, ] <- c(step, substep, param.name, value)
  
  return(history)
}



#' @title Get the history of an assay
#' 
#' @param dataIn An instance of `MultiAssayExperiment` class
#' @param x The name of a slot in the object
#'
#' @return A `data.frame()`
#'
#' @examples
#' NULL
#' 
#' @export
#' 
GetHistory <- function(dataIn, x){
    history <- NULL
    
    if (x == 'Description'){
      if ('Convert' %in% names(dataIn))
        history <- DaparToolshed::paramshistory(dataIn[['Convert']])
    } else if (x == 'Save'){
      history <- NULL
    } else if (x %in% names(dataIn)){
      history <- DaparToolshed::paramshistory(dataIn[[x]])
    }

    return(history)
  }



#' @title Initialize the history
#'
#' @description This function initializes the history.
#'
#' @return An empty `data.frame` with 4 columns ('Step', 'Substep', 'Parameter' and 'Value')
#'
#' @examples
#' InitializeHistory()
#' 
#' @export
#' 
InitializeHistory <- function() {
  history <- NULL
  history <- setNames(
    data.frame(matrix(ncol = 4, nrow = 0)),
    c("Step", "Substep", "Parameter", "Value")
  )
  
  return(history)
}



#' @title Loads packages
#' 
#' @description Checks if a package is available to load it
#' 
#' @param ll.deps A `character()` vector which contains packages names
#' 
#' @return NA
#' 
#' @examples 
#' NULL
#' 
#' @export
#' 
#' @importFrom QFeatures addAssay removeAssay
#' @import DaparToolshed
#' @importFrom MagellanNTK Get_Code_Declare_widgets Get_Code_for_ObserveEvent_widgets source_shinyApp_files nav_process_ui nav_process_server source_wf_files Get_Code_for_rv_reactiveValues Get_Code_Declare_rv_custom Get_Code_for_dataOut format_DT_ui format_DT_server Timestamp toggleWidget mod_popover_for_help_server mod_popover_for_help_ui
#' 
#' @author Samuel Wieczorek
#' 
pkgs_require <- function(ll.deps){
  
  if (!requireNamespace('BiocManager', quietly = TRUE)) {
    txt <- paste0("Please run install.packages('BiocManager')")
    stop(txt)
  }
  
  lapply(ll.deps, function(x) {
    if (!requireNamespace(x, quietly = TRUE)) {
      txt <- paste0("Please install ", x, ": BiocManager::install('", x, "')")
      stop(txt)
    }
  })
}


#' @title Add resource paths
#' 
#' @return NA
#' 
#' @examples
#' add_resourcePath()
#' 
#' @export
#' 
#' @importFrom shiny addResourcePath
#' @author Samuel Wieczorek
#' 
add_resourcePath <- function(){
  addResourcePath("www", system.file("app/www", package = "Prostar2"))
  addResourcePath("images", system.file("app/images", package = "Prostar2"))
}





#' @title
#' xxxx
#'
#' @description
#' xxxx
#'
#' @param typeDataset xx
#' 
#' @return NA
#' 
#' @examples
#' NULL
#'
#' @export
BuildColorStyles <- function(typeDataset) {
  mc <- DaparToolshed::metacellDef(typeDataset)
  styles <- setNames(mc$color, nm = mc$node)

  styles
}




#' @title
#' xxxx
#'
#' @description
#' xxxx
#'
#' @param obj.se xx
#' @param digits xxx
#' 
#' @return NA
#' 
#' @examples
#' NULL
#' 
#' @export
#'
Build_enriched_qdata <- function(obj.se, digits = NULL) {
  if (is.null(digits)) {
    digits <- 2
  }
  
  test.table <- as.data.frame(round(SummarizedExperiment::assay(obj.se)))
  
  if (!is.null(names(DaparToolshed::qMetacell(obj.se)))) { 
   
    colnames.data <- colnames(SummarizedExperiment::assay(obj.se))
    colnames.metadata <- colnames(DaparToolshed::qMetacell(obj.se))
    colnames.metadata <- gsub('metacell_', '', colnames.metadata)
    .ind2keep <- which(colnames.metadata %in% colnames.data)
    
    test.table <- cbind(
      round(SummarizedExperiment::assay(obj.se), digits = digits),
      DaparToolshed::qMetacell(obj.se)[ ,.ind2keep]
    )
  } else {
    test.table <- cbind(
      test.table,
      as.data.frame(
        matrix(rep(NA, ncol(test.table) * nrow(test.table)),
          nrow = nrow(test.table)
        )
      )
    )
  }
  return(test.table)
}


#' @title Extract value with the correct class
#'
#' @description
#' Extract value with the correct class
#'
#' @param value Value to extract
#' @param expected_type Expected value class
#' 
#' @return The value with the correct class, 
#' or NA if the coercion is not possible
#' 
#' @examples
#' Extract_Value(1, "character")
#' Extract_Value("test", "numeric")
#' 
#' @export
#'
Extract_Value <- function(value, 
                          expected_type = c("numeric", "character", "logical", "factor", "integer")) {
  expected_type <- match.arg(expected_type)
  
  tryCatch({
    switch(
      expected_type,
      numeric = as.numeric(value),
      character = as.character(value),
      logical = as.logical(value),
      factor = as.factor(value),
      integer = as.integer(value)
    )
  },
  warning = function(w) NA,
  error = function(e) NA
  )
}


#' @title Get filters scope
#'
#' @description Get the list of possible scopes for filters
#'
#' @return A `list`
#'
#' @examples
#' GetFiltersScope()
#'
#' @export
#'
GetFiltersScope <- function(){
  c("Whole Line" = "WholeLine",
    "Whole matrix" = "WholeMatrix",
    "For every condition" = "AllCond",
    "At least one condition" = "AtLeastOneCond"
  )
}


#' @title Numeric test
#'
#' @description Test whether an object is numeric or not
#'
#' @param input The object to test
#'
#' @return `NULL` if numeric, a `character` if not
#'
#' @examples
#' not_a_numeric(1)
#' not_a_numeric("A")
#'
#' @export
#'
not_a_numeric <- function(input) {
  if (is.na(as.numeric(input))) {
    "Please input a number"
  } else {
    NULL
  }
}


#' @title Object contained in another one
#'
#' @description Check whether an object contained in another one
#'
#' @param strA Object to find
#' @param strB Object to check
#' 
#' @return A `logical(1)`
#'
#' @examples
#' isContainedIn("A", c("A", "B"))
#' isContainedIn("A", c("ABCDE", "FGHIJ"))
#'
#' @export
#'
isContainedIn <- function(strA, strB) {
  return(all(strA %in% strB))
}



#' @title Check missing values
#'
#' @description Check whether there is missing values in a SE assay
#'
#' @param data A `QFeatures` 
#' @param i The index or name of the assay to check
#' 
#' @return A `logical(1)`
#'
#' @examples
#' data(subR25prot, package = "DaparToolshed")
#' checkNA(subR25prot)
#'
#' @export
#'
checkNA <- function(data, 
                    i = NULL) {
  if (is.null(i)){
    i <- length(data)
  }
  
  qdata <- SummarizedExperiment::assay(data[[i]])
  sum(is.na(qdata)) > 0
}


#' @title Count pattern
#'
#' @description Count the number of occurence of a selected metacell tag 
#'
#' @param data A `SummarizedExperiment`
#' @param pattern A `character(1)`, pattern to count
#' @param level Dataset level
#'
#' @return A `numeric(1)` 
#'
#' @examples
#' data(subR25prot, package = "DaparToolshed")
#' countPattern(subR25prot[[length(subR25prot)]],
#'                pattern = "Missing POV")
#'
#' @export
#'
countPattern <- function(dataSE, 
                         pattern, 
                         level = NULL) {
  if (is.null(level)){
    level <- DaparToolshed::typeDataset(dataSE)
  }
  
  m <- DaparToolshed::matchMetacell(
    DaparToolshed::qMetacell(dataSE),
    pattern = pattern,
    level = level)
  
  length(which(m))
}


#' @title Show log console
#'
#' @description Show R logs in a console in shiny 
#'
#' @param expr xxx
#' @param id_notif xxx
#' @param cond_info xxx
#' @param cond_warning xxx
#' @param cond_error xxx
#' @param color_info xxx
#' @param color_warning xxx
#' @param color_error xxx
#' @param new_first xxx
#' @param console_message xxx
#' @param prefix xxx
#'
#' @return xxx
#'
#' @examples
#' NULL
#'
#' @export
#'
show_log_console <- function(
    expr,
    id_notif = NULL,
    cond_info = TRUE,
    cond_warning = TRUE,
    cond_error = TRUE,
    color_info = "grey",
    color_warning = "blue",
    color_error = "red",
    new_first = TRUE,
    console_message = TRUE,
    prefix = "Imputation"
) {
  notifications <- reactiveVal(list()) # To store messages 
  prefix <- paste0(prefix, if (prefix == "") " " else "-")
  msg_actions <- list( ### Create functions to use depending on message type
    message = function(m) { ### For info
      if (cond_info){
        ### Show custom message in console
        if (console_message) message_console(m$message, level = "INFO", info_text = paste0(prefix, "INFO"), color_info = color_info)
        ### Get message
        if (!all(m$message %in% c("\r", "\n", ""))){
          new_notification <- paste0("<b>[", prefix, "INFO]</b> <i>", format(Sys.time(), "%d-%m-%Y %X"), "</i> — ", m$message)
          current_notifications <- notifications()
          if (new_first) current_notifications <- c(paste0('<span style="color: ', color_info, '">', new_notification, '</span>'), current_notifications)
          else current_notifications <- c(current_notifications, paste0('<span style="color: ', color_info, '">', new_notification, '</span>'))
          notifications(current_notifications) # Update list of messages
          ### Show message in app
          if (!is.null(id_notif)) shinyjs::html(id_notif, paste("<ul>", paste("<li>", current_notifications, "</li>", collapse = ""), "</ul>"))
        }
      }
    },
    warning = function(m) { ### For warning
      if (cond_warning){
        ### Show custom message in console
        if (console_message) message_console(m$message, level = "WARNING", warning_text = paste0(prefix, "WARNING"), color_warning = color_warning)
        ### Get message
        if (!all(m$message %in% c("\r", "\n", ""))){
          new_notification <- paste0("<b>[", prefix, "WARNING]</b> <i>", format(Sys.time(), "%d-%m-%Y %X"), "</i> — ", m$message)
          current_notifications <- notifications()
          if (new_first) current_notifications <- c(paste0('<span style="color: ', color_warning, '">', new_notification, '</span>'), current_notifications)
          else current_notifications <- c(current_notifications, paste0('<span style="color: ', color_warning, '">', new_notification, '</span>'))
          notifications(current_notifications) # Update list of messages
          ### Show message in app
          if (!is.null(id_notif)) shinyjs::html(id_notif, paste("<ul>", paste("<li>", current_notifications, "</li>", collapse = ""), "</ul>"))
        }
      }
    },
    error = function(m) { ### For error
      if (cond_error){
        ### Show custom message in console
        if (console_message) message_console(m$message, level = "ERROR", error_text = paste0(prefix, "ERROR"), color_error = color_error)
        ### Get message
        if (!all(m$message %in% c("\r", "\n", ""))){
          new_notification <- paste0("<b>[", prefix, "ERROR]</b> <i>", format(Sys.time(), "%d-%m-%Y %X"), "</i> — ", m$message)
          current_notifications <- notifications()
          if (new_first) current_notifications <- c(paste0('<span style="color: ', color_warning, '">', new_notification, '</span>'), current_notifications)
          else current_notifications <- c(current_notifications, paste0('<span style="color: ', color_warning, '">', new_notification, '</span>'))
          notifications(current_notifications) # Update list of messages
          ### Show message in app
          if (!is.null(id_notif)) shinyjs::html(id_notif, paste("<ul>", paste("<li>", current_notifications, "</li>", collapse = ""), "</ul>"))
        }
      }
    }
  )
  
  if (console_message) { ### Show custom message in console
    tryCatch(
      suppressMessages(suppressWarnings( # Remove R messages in console
        withCallingHandlers( # Add custom messages in console
          expr,
          message = function(m) msg_actions$message(m),
          warning = function(m) msg_actions$warning(m)
        ))),
      error = function(m) {
        msg_actions$error(m)
        return(NULL)}
      
    )
  } else { ### Show message only in app
    tryCatch(
      withCallingHandlers(
        expr,
        message = function(m) msg_actions$message(m),
        warning = function(m) msg_actions$warning(m)
      )
      ,
      error = function(m) {
        msg_actions$error(m)
        return(NULL)
      }
    )
  }
  return(notifications())
}


#' @title Message console
#'
#' @description Style R logs in a console in shiny 
#'
#' @param msg xxx
#' @param level xxx
#' @param info_text xxx
#' @param warning_text xxx
#' @param error_text xxx
#' @param color_info xxx
#' @param color_warning xxx
#' @param color_error xxx
#' @param color_other xxx
#'
#' @return xxx
#'
#' @examples
#' NULL
#'
#' @export
#'
message_console <- function(msg,
                            level = "INFO",
                            info_text = "INFO",
                            warning_text = "WARNING",
                            error_text = "ERROR",
                            color_info = "grey",
                            color_warning = "blue",
                            color_error = "red",
                            color_other = "black"){
  msg <- paste0(msg, collapse = "")
  # Text in the prefix depending on message type
  level_text <- switch(toupper(level), 
                       "INFO" = info_text,
                       "WARNING" = warning_text,
                       "ERROR" = error_text,
                       level
  )
  # Create message
  msg <- if(!(msg %in% c("\r", "\n", ""))){ paste0("[", level_text, "] ", format(Sys.time(), "%d-%m-%Y %X"), " — ", msg)}
  # Show custom message in console depending on message type
  switch(toupper(level),
         "INFO" = message(print_color(msg, color_info)),
         "WARNING" = print_color(paste0(msg, "\n"), color_warning),
         "ERROR" = print_color(paste0(msg, "\n"), color_error),
         cat(print_color(msg, color_other), sep = "\n")
  )
}
