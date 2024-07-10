

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
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
callme_engine <- function(options) {
  
  if (requireNamespace('knitr', quietly = TRUE)) {
    "%||%" <- \(x,y) if (is.null(x)) y else x
    verbose <- options$verbose %||% FALSE
    
    env = knitr::knit_global()
    code <- paste(options$code, collapse = "\n")
    tmp <- callme(
      code      = code, 
      cpp_flags = options$cpp_flags, 
      ld_flags  = options$ld_flags,
      verbose   = verbose
    )
    
    dllname <- options$dllname %||% 'dll'
    assign(dllname, tmp, envir = env)
    
    options$engine <- 'c'
    res <- knitr::engine_output(options, code, "")
    
    # I want to include the YAML options, 
    # but NOT have the YAML formatted as C code.
    # Let's pretend it's R code instead
    yaml <- paste(options$yaml.code, collapse = "\n")
    res <- paste0("```{.c .cell-code}\n", yaml, "\n```", res, sep = "\n")
    
    res
  } else {
    NULL
  }
}