

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
#' @export
#' 
#' @return A reference to the dynamic library which was loaded into R with 
#'         \code{dyn.load()} i.e. a 'DLLInfo' object.  This has been setup 
#'         such that when the returned object goes out of scope, the dynamic 
#'         library is unloaded i.e. the C function will no longer be accessible.
#'         Thus the user needs to keep a reference to this returned object for 
#'         as long as they need access to the C function.
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
#' # Call the function
#' .Call("calc_", 1, 2.5)
#' 
#' # Advanced: Get info about the C function / library.
#' dll$calc_
#' }
#' 
#'
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
  # Create a new directory to work in.
  # This is so we don't clobber any other 'Makevars' file
  # which might exist.  e.g. two R processes trying to run 'callme()' at 
  # the same time.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  datestamp <- strftime(Sys.time(), "%Y%m%d-%H%M")
  tmp_dir   <- tempfile(pattern = paste0("callme_", datestamp, "_"))
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  
  tmp_file  <- tempfile(tmpdir = tmp_dir)
  c_file    <- paste0(tmp_file, ".c")
  dll_file  <- paste0(tmp_file, .Platform$dynlib.ext)
  
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
  # Compile a shared library.
  # LDFLAGs can be included at the end of a "R CMD SHLIB" call to add
  # extra library search paths and link to libraries.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  system2(
    command = paste0(R.home(component = "bin"), "/R"), 
    args    = paste("CMD SHLIB", basename(c_file), ld_flags)
  )
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Load the DLL
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dll <- dyn.load(dll_file)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a 'finalizer' which will be run when a specific environment falls
  # out of scope.
  # param env the particular environment which will be watched
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  finalizer <- function(env) {
    short_name <- basename(tmp_file)
    if (short_name %in% names(getLoadedDLLs())) {
      dyn.unload(dll_file)
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
  dll[['env']] <- new.env()
  reg.finalizer(dll[['env']], finalizer, onexit = TRUE)
  
  
  dll
}

