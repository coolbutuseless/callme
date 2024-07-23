

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' An engine for knitr to handle C code compatible with \code{.Call()}
#' 
#' @param options the data passed in from the code chunk in Rmarkdown or
#'        Quarto etc.
#'
#' \describe{
#' \item{compile}{Actually compile the code. default TRUE}
#' \item{headers}{automatically include minimal R headers in code. Default: TRUE}
#' \item{rcode}{Include R code in a code block beneath the C code, Default: TRUE}
#' }
#'
#' @examplesIf interactive()
#' # Set the engine in an initial chunk in the document
#' # Then use \code{callme} as the chunk engine
#' knitr::knit_engines$set(callme = callme:::callme_engine)
#'
#' @return text block to be rendered in the document
#' @importFrom methods formalArgs
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
callme_engine <- function(options) {
  
  if (requireNamespace('knitr', quietly = TRUE)) {
    "%||%" <- function(x,y) if (is.null(x)) y else x
    
    
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
    options$class.source <- c('callme') # CSS class name
    res <- knitr::engine_output(options, args$code, "")
    
    if (options$rcode %||% TRUE) {
      rcode <- paste(
        sprintf('code = r"(\n%s\n)"', args$code),
        "",
        "callme::compile(code)",
        sep = "\n"
      )
      # Format as C code in output
      options$engine <- 'r'
      options$class.source <- c() 
      resr <- knitr::engine_output(options, rcode, "")
      
      resr <- paste(
        "<details>",
        "<summary>Click to show R code</summary>",
        resr,
        "</details>",
        sep = "\n"
      )
      
      res <- paste(res, resr, sep = "\n")
    }
    
    res
  } else {
    NULL
  }
}
