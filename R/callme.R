

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
callme <- function(code, cpp_flags = NULL, ld_flags = NULL, env = parent.frame(), verbosity = 0) {
  
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
  if (!stringr::str_detect(code, "#include\\s+<Rdefines.h>")) {
    code <- paste("#include <Rdefines.h>", code, sep = "\n")
  }
  if (!stringr::str_detect(code, "#include\\s+<R.h>")) {
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
      if (exists(func_name, envir = env)) {
        warning("Clobbering function: '", func_name, "'", call. = FALSE)
      }
      if (verbosity >= 1) {
        message("Assigning new function: '", func_name, "()'")
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
#' @import stringr
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
  # Find all function declarations which match: "SEXP func_name(...)"
  decls <- stringr::str_extract_all(code, stringr::regex("^\\s*SEXP\\s+[a-zA-Z0-9_]+\\s*\\(.*?\\)", multiline = TRUE, dotall=TRUE))[[1]]
  
  # tidy function declarations for next step
  decls <- stringr::str_trim(decls)
  decls <- stringr::str_replace_all(decls, "\\s+", " ")
  
  decls
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Extract a character vector of arguments from the declaration
#' 
#' @param decl string containing function declaration
#' @return Character vector of argument names for the given function declaration
#' @import stringr
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
extract_args_from_declaration <- function(decl) {
  args <- stringr::str_match(decl, "\\((.*?)\\)")
  stopifnot(nrow(args) == 1)
  args <- args[1, 2]
  args <- stringr::str_split(args, ",")[[1]]
  
  if (length(args) == 1 && args == "") {
    return(NULL) # no args
  }
  
  if (!all(str_detect(args, "SEXP "))) {
    # one (or more) of the arguments is not of type "SEXP"
    # which means this cannot be a .Call() function 
    return(NA_character_)
  }
  
  args <- stringr::str_replace_all(args, "SEXP", "")
  args <- stringr::str_trim(args)
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
#' @import stringr
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_wrapper_function <- function(decl, dll_file) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Extract the function name
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  func <- stringr::str_match(decl, "SEXP\\s+([a-zA-Z0-9_]+)")
  stopifnot(nrow(func) == 1)
  func_name <- func[1, 2]
  
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
  res <- list(eval(parse(text = func_str)))
  names(res) <- func_name
  
  res
}

