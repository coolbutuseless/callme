

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Compile C code and load into R for use with \code{.Call()}
#' 
#' See also \code{?SHLIB}
#' 
#' @param code C code following the \code{.Call()} conventions.  Must
#'        also include any \code{#include} statements.
#' @param cpp_flags character string of flags for the C pre-processor.
#'        Default: NULL
#'        e.g. \code{cpp_flags = "-I/opt/homebrew/include"} to add the include path 
#'        for homebrew to the compilation step. 
#' @param ld_flags character string of flags when linking. Default: NULL.
#'        e.g. \code{ld_flags = "-L/opt/homebrew/lib -lzstd"} to include the homebrew 
#'        libraries in the linker search path and to link to the \code{zstd}
#'        library installed there. 
#' @param env environment into which to assign the R wrapper functions.
#'        Default: \code{parent.frame()}.  If \code{NULL} then no 
#'        assignment takes place.
#' @param overwrite which existing variables can be overwritten when this function
#'        creates wrapper variables? If permission not given to overwrite a 
#'        variable which already exists in the environment, then an error is 
#'        raised.
#' \describe{
#' \item{"callme"}{(Default) Only overwrite other functions created by \code{callme()}}
#' \item{"all"}{All variables will be overwritten}
#' \item{"functions"}{Only functions are overwritten}
#' \item{"none"}{No variables may be overwritten}
#' }
#' @param verbosity Level of output: Default: 0. current max: 2
#'        
#' @export
#' 
#' @return Invisibly returns a named list of R functions. Each R function 
#'         calls to the equivalent C functions.  
#'         
#' @examples
#' code <- "
#' #include <R.h>
#' #include <Rdefines.h>
#' SEXP calc(SEXP val1, SEXP val2) {
#'   return ScalarReal(asReal(val1) + asReal(val2));
#' }"
#' 
#' # Need to keep a reference to the returned value in order to retain access
#' # to the compiled functions.  I.e. the dll will be unloaded (via \code{dyn.unload()})
#' # when \code{'dll'} gets garbage collected.
#' callme(code)
#' 
#' # Use the auto-generated wrapper function
#' calc(1, 2.5)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
callme <- function(code, cpp_flags = NULL, ld_flags = NULL, env = parent.frame(), 
                   overwrite = "callme", verbosity = 0) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity check code
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(is.character(code))
  stopifnot(length(code) == 1)
  stopifnot(nchar(code) > 0)
  stopifnot(!is.na(code))
  stopifnot(overwrite %in% c('all', 'callme', 'functions', 'none'))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a new directory to work in.
  # This is so we don't clobber any other 'Makevars' file
  # which might exist.  e.g. two R processes trying to run 'callme()' at 
  # the same time.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  datestamp <- strftime(Sys.time(), "%Y%m%d-%H%M")
  suffix    <- paste(sample(c(letters, LETTERS), 8), collapse = "") # random junk
  datestamp <- paste(datestamp, suffix, sep = "_")
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
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Check if we have the two required includes
  #  #include <R.h>
  #  #include <Rdefines.h>
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!grepl("#include\\s+<Rdefines.h>", code)) {
    code <- paste("#include <Rdefines.h>", code, sep = "\n")
  }
  if (!grepl("#include\\s+<R.h>", code)) {
    code <- paste("#include <R.h>", code, sep = "\n")
  }
  
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
  if (verbosity > 0) {
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
  # Show errors
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (ret != 0) {
    warning("Error during compilation")
    if (verbosity == 0) {
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
  if (verbosity >= 2) {
    cat("dll file: ", dll_file, "\n")
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Automatically generate some wrapper functions in a named list
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  func_list <- create_wrapper_functions(code, dll_file)

  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Assign functions into environment
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is.null(env)) {
    for (func_name in names(func_list)) {
      if (exists(func_name, envir = env, inherits = FALSE)) {
        var <- get0(func_name, envir = env, inherits = FALSE)
        
        if (
          (overwrite == 'all') ||
          (overwrite == 'callme' && inherits(var, 'callme')) ||
          (overwrite == 'functions' && is.function(var)) 
        ) {
          # Allowed overwrite
        } else {
          stop("callme(): Not allowed to overwrite existing variable with new wrapper function '", func_name, "'", call. = FALSE)
        }
         
        
      }
      assign(func_name, func_list[[func_name]], pos = env)
    }
  }
  
  
  invisible(func_list)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create \code{.Call()} function wrappers for C functions
#' 
#' @param code C code as single string.  This does not recurse into source
#'        or header files referenced in the C code.
#' @param dll_file full path to dill filename
#'        
#' @return named list of R functions which can call into the library
#' 
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_wrapper_functions <- function(code, dll_file) {
  
  decls <- extract_function_declarations(code)
  funcs <- lapply(decls, create_wrapper_function, dll_file = dll_file)
  funcs <- Filter(Negate(is.null), funcs)
  
  if (length(funcs) == 0) {
    warning("Code does not appear to contain any functions compatible with .Call()")
  }
  
  unlist(funcs, recursive = FALSE)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Extract function declarations which are consistent with .Call() semantics
#' 
#' @param code C code as single string.  This does not recurse into source
#'        or header files referenced in the C code.
#' @return character vector of function declarations in \code{code}
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
extract_function_declarations <- function(code) {
  m <- gregexpr("(?ms)^\\s*SEXP\\s+[a-zA-Z0-9_]+\\s*\\(.*?\\)", code, perl = TRUE, ignore.case = TRUE)
  decls <- regmatches(code, m)[[1]]
  
  # tidy function declarations for next step
  decls <- trimws(decls)
  decls <- gsub("\\s+", " ", decls)
  
  decls
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Extract a character vector of arguments from the declaration
#' 
#' @param decl string containing function declaration
#' @return Character vector of argument names for the given function declaration
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
extract_args_from_declaration <- function(decl) {
  m    <- gregexpr("\\((.*?)\\)", decl, perl = TRUE)
  args <- regmatches(decl, m)[[1]]
  args <- substr(args, 2, nchar(args) - 1) # Remove ()
  args <- strsplit(args, ",")[[1]]
  args <- trimws(args)
  args
  
  if (length(args) == 0 || (length(args) == 1 && (args == "" || args == "void"))) {
    return(NULL) # no args
  }
  
  if (!all(grepl("SEXP ", args))) {
    # one (or more) of the arguments is not of type "SEXP"
    # which means this cannot be a .Call() function 
    return(NA_character_)
  }
  
  args <- gsub("SEXP", "", args)
  args <- trimws(args)
  if (length(args) == 1 && args == "") {
    # Setting args to NULL here makes the function creation easier.
    args <- NULL
  }
  
  args
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a wrapper function for the given C declaration of a \code{.Call()}-compatible 
#' function
#' 
#' @param decl C declaration. E.g. \code{"SEXP two(SEXP vara, SEXP varb)"}
#' 
#' @return anonymous R function to \code{.Call} the dll
#' 
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_wrapper_function <- function(decl, dll_file) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Extract the function name
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  m <- regexpr("SEXP\\s+([a-zA-Z0-9_]+)", decl, perl = TRUE)
  func_name <- regmatches(decl, m)
  func_name <- sub("^\\s*SEXP\\s+", "", func_name, perl = TRUE)
  func_name
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Extract the argument names
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  args <- extract_args_from_declaration(decl)
  if (anyNA(args)) {
    # This is not a .Call() function
    return(NULL)
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the function as a string
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  func_str <- sprintf(
    "function(%s) .Call(%s, PACKAGE = '%s')", 
    paste(args, collapse = ", "),
    paste(c(dQuote(func_name, q=FALSE), args), collapse = ", "),
    tools::file_path_sans_ext(base::basename(dll_file))
  )
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Parse function into R code and put in a named list
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  func <- eval(parse(text = func_str))
  class(func) <- "callme"
  res <- list(func)
  names(res) <- func_name
  
  res
}

