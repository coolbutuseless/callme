

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
#'        
#' @return A reference to the dynamic library which was loaded into R with 
#'         \code{dyn.load()}.  This is returned invisibly, and usually not
#'         needed for anything.
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
#' dll <- callme(code)
#' 
#' # Call the function
#' .Call("calc_", 1, 2.5)
#' 
#' # Advanced: Get info about the C function / library.
#' dll$calc_
#' }
#' 
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
callme <- function(code, cpp_flags = NULL, ld_flags = NULL) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity check code
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(is.character(code))
  stopifnot(length(code) == 1)
  stopifnot(nchar(code) > 0)
  stopifnot(!is.na(code))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Setup temporary file paths 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tmp_file <- tempfile(pattern = "callme-")
  c_file   <- paste0(tmp_file, ".c")
  dll_file <- paste0(tmp_file, .Platform$dynlib.ext)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Due to path name weirdness between R, C, gcc, llvm and windows, 
  # it's better to just shift to the source code directory and compile
  # the C file specified without a path
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  on.exit(setwd(getwd()))
  setwd(dirname(c_file))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Dump code to a file.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  write(code, c_file)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Compile code.
  # Have to an excplit 'COMPILE' step so we get to set CPPFLAGs, 
  # as you cannot set CPPFLAGs when doing "R CMD SHLIB" (afaik)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is.null(cpp_flags)) {
    stopifnot(is.character(cpp_flags))
    stopifnot(length(cpp_flags) == 1)
    stopifnot(nchar(cpp_flags) > 0)
    stopifnot(!is.na(cpp_flags))
    
    cpp_flags <- paste0("CPPFLAGS=", cpp_flags)
  } 
  
  system2(
    command = paste0(R.home(component = "bin"), "/R"), 
    args    = paste(c("CMD COMPILE", cpp_flags, basename(c_file)), collapse = " ")
  )
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Link code
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  system2(
    command = paste0(R.home(component = "bin"), "/R"), 
    args    = paste("CMD SHLIB", basename(c_file), ld_flags)
  )
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Load the DLL
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dll <- dyn.load(dll_file)
  
  invisible(dll)
}

