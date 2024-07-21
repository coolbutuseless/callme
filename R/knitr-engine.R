

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' An engine for knitr to handle C code compatible with \code{.Call()}
#' 
#' @param options the data passed in from the code chunk in Rmarkdown or
#'        Quarto etc.
#'
#' @examplesIf interactive()
#' # Set the engine in an initial chunk in the document
#' # Then use \code{callme} as the chunk engine
#' knitr::knit_engines$set(callme = callme::callme_engine)
#'
#' @return text block to be rendered in the document
#' @importFrom methods formalArgs
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
callme_engine <- function(options) {
  
  if (requireNamespace('knitr', quietly = TRUE)) {
    "%||%" <- \(x,y) if (is.null(x)) y else x
    
    
    args <- options[names(options) %in% formalArgs(compile)]
    
    args$code <- paste(args$code, collapse = "\n")
    
    if (options$headers %||% TRUE) {
      if (!grepl("#include\\s+<Rinternals.h>", args$code)) {
        args$code <- paste0("#include <Rinternals.h>\n\n", args$code)
      }
      if (!grepl("#include\\s+<R.h>", args$code)) {
        args$code <- paste0("#include <R.h>\n", args$code)
      }
    }
    
    args$env <- knitr::knit_global()
    
    if (options$compile %||% TRUE) {
      do.call(compile, args)
    }
    
    # Format as C code in output
    options$engine <- 'c'
    options$class.source <- c('callme')
    res <- knitr::engine_output(options, args$code, "")
    
    options <- options[sort(names(options))]
    # print(options)
    
    res
  } else {
    NULL
  }
}