

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Compile C code and load into R for use with \code{.Call()}
#' 
#' See also \code{?SHLIB}
#' 
#' @param code C code following the \code{.Call()} conventions.  Must
#'        also includes any \code{#include} statements.
#' @param cpp_flags character string of flags for the C pre-processor.
#'        Default: NULL
#'        e.g. \code{"-I/opt/homebrew/include"} to add the include path 
#'        for homebrew to the compilation step. 
#' @param ld_flags character string of flags when linking. Default: NULL.
#'        e.g. \code{"-L/opt/homebrew/lib -lzstd"} to include the homebrew 
#'        libraries in the linker search path and to link to the \code{zstd}
#'        library installed there. 
#' @param verbose Should the output of the compiler be echoed to the R console?
#'        Default: FALSE
#'        
#' @export
#' 
#' @return A named list of R functions which wrap the calls to the equivalent
#'         C functions.  When this returned object is garbage collected, the
#'         generated library will be unloaded.
#'         
#' @examples
#' \dontrun{
#'   code <- "
#' #include <R.h>
#' #include <Rdefines.h>
#' SEXP calc_(SEXP val1_, SEXP val2_) {
#'   return ScalarReal(asReal(val1_) + asReal(val2_));
#' }"
#' 
#' # Need to keep a reference to the returned value in order to retain access
#' # to the compiled functions.  I.e. the dll will be unloaded (via \code{dyn.unload()})
#' # when \code{'dll'} gets garbage collected.
#' dll <- callme(code)
#' 
#' # Manually call the function
#' .Call("calc_", 1, 2.5)
#' 
#' # Use the auto-generated wrapper function
#' dll$calc_(1, 2.5)
#' }
#' 
#'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
callme <- function(code, cpp_flags = NULL, ld_flags = NULL, verbose = FALSE) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity check code
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(is.character(code))
  stopifnot(length(code) == 1)
  stopifnot(nchar(code) > 0)
  stopifnot(!is.na(code))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a new directory to work in.
  # This is so we don't clobber any other 'Makevars' file
  # which might exist.  e.g. two R processes trying to run 'callme()' at 
  # the same time.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  datestamp <- strftime(Sys.time(), "%Y%m%d-%H%M")
  tmp_dir   <- tempfile(pattern = paste0("callme_", datestamp, "_"))
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  
  tmp_file  <- file.path(tmp_dir, paste0("callme_", datestamp))
  c_file    <- paste0(tmp_file, ".c")
  dll_file  <- paste0(tmp_file, .Platform$dynlib.ext)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Due to path name weirdness between R, C, gcc, llvm and windows, 
  # it's better to just shift to the source code directory and compile
  # the C file specified without a path
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  start_dir <- getwd()
  on.exit(setwd(start_dir))
  setwd(tmp_dir)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Dump code to a file.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  write(code, c_file)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Write a 'Makevars' with the CPPFLAGS because as-far-as-I-know
  # you cannot set CPPFLAGs when doing "R CMD SHLIB" 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is.null(cpp_flags)) {
    stopifnot(is.character(cpp_flags))
    stopifnot(length(cpp_flags) == 1)
    stopifnot(nchar(cpp_flags) > 0)
    stopifnot(!is.na(cpp_flags))
    cpp_flags <- paste0("PKG_CPPFLAGS=", cpp_flags)
    writeLines(cpp_flags, "Makevars")
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Setup stdout/stderr
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (isTRUE(verbose)) {
    stdout = ""  # echo to R console
    stderr = ""  # echo to R console
  } else {
    stdout = "stdout"  # divert to file
    stderr = "stderr"  # divert to file
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Compile a shared library.
  # LDFLAGs can be included at the end of a "R CMD SHLIB" call to add
  # extra library search paths and link to libraries.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ret <- system2(
    command = paste0(R.home(component = "bin"), "/R"), 
    args    = paste("CMD SHLIB", basename(c_file), ld_flags),
    stdout  = stdout,
    stderr  = stdout
  )
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If there's an error, force show the output even if verbose = FALSE
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (ret != 0) {
    warning("Error during compilation")
    if (!verbose) {
      if (file.exists('stdout'))
        cat(readLines("stdout"), sep = "\n")
      if (file.exists('stderr')) 
        cat(readLines("stderr"), sep = "\n")
    }
    return(NULL)
  }
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Load the DLL
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dll <- dyn.load(dll_file)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a 'finalizer' which will be run when a specific environment falls
  # out of scope.
  # param 'env' the particular environment which will be watched
  #
  # Will unload the library and delete the directory where it was compiled
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  finalizer <- function(env) {
    short_name <- basename(tmp_file)
    if (short_name %in% names(getLoadedDLLs())) {
      dyn.unload(dll_file)
      suppressWarnings(
        unlink(tmp_dir, recursive = TRUE)
      )
    }
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create an environment and attach it to the DLLInfo 
  # Register a finalizer on this environment.
  # When the returned 'dll' object falls out of scope:
  #    * the 'env' will be garbage collected
  #    * the finalizer() function will be called
  #         * which unloads the actual dll 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  res <- list()
  dll[['env']] <- new.env()
  reg.finalizer(dll[['env']], finalizer, onexit = TRUE)
  res$raw_dll <- dll
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Automatically generate some wrapper functions
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  funcs <- create_wrapper_functions(code)
  for (fname in names(funcs)) {
    res[[fname]] <- funcs[[fname]]
  }
  
  
  res
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create \code{.Call()} function wrappers for C functions
#' 
#' @param code C code as single string.  This does not recurse into source
#'        or header files referenced in the C code.
#'        
#' @return named list of R functions which can call into the library
#' 
#' @import stringr
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_wrapper_functions <- function(code) {
  
  decls <- extract_function_declarations(code)
  funcs <- lapply(decls, create_wrapper_function)
  
  unlist(funcs, recursive = FALSE)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Extract function declarations which are consistent with .Call() semantics
#' 
#' @param code C code as single string.  This does not recurse into source
#'        or header files referenced in the C code.
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
extract_function_declarations <- function(code) {
  # Find all function declarsions which match: "SEXP func_name(...)"
  decls <- stringr::str_extract_all(code, stringr::regex("^\\s*SEXP\\s+[a-zA-Z0-9_]+\\s*\\(.*?\\)", multiline = TRUE, dotall=TRUE))[[1]]
  
  # tidy function declarations for next step
  decls <- stringr::str_trim(decls)
  decls <- stringr::str_replace_all(decls, "\\s+", " ")
  
  decls
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a wrapper functon for the given C declaration of a .Call compatible 
#' function
#' 
#' @param decl C declaration. E.g. \code{"SEXP two_(SEXP vara, SEXP varb)"}
#' 
#' @return anonymous R function to \code{.Call} the dll
#' 
#' @import stringr
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_wrapper_function <- function(decl) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Extract the function name
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  func <- stringr::str_match(decl, "SEXP\\s+([a-zA-Z0-9_]+)")
  stopifnot(nrow(func) == 1)
  func_name <- func[1, 2]
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Extract the argument names
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  args <- stringr::str_match(decl, "\\((.*?)\\)")
  stopifnot(nrow(args) == 1)
  args <- args[1, 2]
  args <- stringr::str_split(args, ",")[[1]]
  args <- stringr::str_replace_all(args, "SEXP", "")
  args <- stringr::str_trim(args)
  if (length(args) == 1 && args == "") {
    # Setting args to NULL here makes the function creation easier.
    args <- NULL
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the function as a string
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  func_str <- sprintf(
    "function(%s) .Call(%s)", 
    paste(args, collapse = ", "),
    paste(c(dQuote(func_name, q=FALSE), args), collapse = ", ")
  )
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Parse function into R code and put in a named list
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  res <- list(eval(parse(text = func_str)))
  names(res) <- func_name
  
  res
}
