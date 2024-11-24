

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run code with the given R_MAKEVARS_USER filename
# Similar idea to 'withr::with_makevars()'
#
# The reason this is done with "R_MAKEVARS_USER + CFLAGS" and not 
# "Makevars + PKG_CFLAGS" is that if PKG_CFLAGS 
# come **before** R's default flags i.e.
#    clang  <PKG_CFLAGS>  <R default cflags>  file.c
#
# but 'CFLAGS' **replaces** R's default CFLAGS, i.e.
#    clang <CFLAGS> file.c
#
# @param R_MAKEVARS_USER filename for makevars-user
# @param code code to execute
# @return returns result of executing code
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
with_makevars_file_env <- function(R_MAKEVARS_USER, code) {
  default <- Sys.getenv('R_MAKEVARS_USER')
  if (default == "") {
    on.exit(Sys.unsetenv('R_MAKEVARS_USER'))
  } else {
    on.exit(Sys.setenv(R_MAKEVARS_USER = default))
  }
  Sys.setenv(R_MAKEVARS_USER = R_MAKEVARS_USER)
  force(code)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Single valid character string
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
assert_single_string <- function(x) {
  stopifnot(is.character(x))
  stopifnot(length(x) == 1)
  stopifnot(nchar(x) > 0)
  stopifnot(!is.na(x))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Compile C code and create wrapper functions to call from R
#' 
#' This function uses the \code{R CMD SHLIB} process to compile
#' C code into a linked library.  This library is then loaded, and 
#' appropriate functions created in R to call into this library.  See also: \code{?SHLIB}
#' 
#' @param code C code following the \code{.Call()} conventions, or a filename
#'        containing this code. This code must also include 
#'        any \code{#include} statements - include \code{<R.h>} and
#'        \code{<Rinternals.h>} at the very least.
#' @param CFLAGS character string of flags for the C compiler. e.g. "-O3"
#'        Default: NULL.  If specified this value will \emph{replace} the
#'        default \code{CFLAGS} R would normally use.  To see these default
#'        flags use \code{maketools::cc_info()$flags}.
#' @param PKG_CPPFLAGS character string of flags for the C pre-processor.
#'        Flags such as "-I", "-D" and "-U" go here.
#'        Default: NULL
#'        e.g. \code{PKG_CPPFLAGS = "-I/opt/homebrew/include"} to add the include path 
#'        for homebrew to the compilation step. 
#' @param PKG_LIBS character string of flags for linking. "-L" and "-l" flags
#'        go here. Default: NULL.
#'        e.g. \code{PKG_LIBS = "-L/opt/homebrew/lib -lzstd"} to include the homebrew 
#'        libraries in the linker search path and to link to the \code{zstd}
#'        library installed there. 
#' @param env environment into which to assign the R wrapper functions.
#'        Default: \code{parent.frame()}.  If \code{NULL} then no 
#'        assignment takes place and the (invisible) return value should
#'        be assigned to a variable to access the compiled code.
#' @param overwrite Which existing variables can be overwritten when wrapper 
#'        functions are created in the given environment? An error will be
#'        raised if the name of the wrapper function already exists in 
#'        the environment and permission has not been given to overwrite.
#' @param invisible Should the R wrapper function return the result invisibly?
#'        Default: FALSE.  Set this to \code{TRUE} if the code is only 
#'        run for its side-effect e.g. just printing data and not returning anything.
#' \describe{
#' \item{"callme"}{(Default) Only functions created by this package can be overwritten}
#' \item{"all"}{All objects can be overwritten}
#' \item{"functions"}{Only functions can be overwritten}
#' \item{"none"}{No existing objects can be overwritten}
#' }
#' @param verbosity Level of output: Default: 0. Max level: 4
#' 
#' @return Invisibly returns a named list of R functions. Each R function 
#'         calls to the equivalent C function.  If \code{env} is specified, 
#'         then these wrapper functions are assigned in the given 
#'         environment.
#'         
#' @examples
#' code <- "
#' #include <R.h>
#' #include <Rinternals.h>
#' 
#' // Add 2 numbers
#' SEXP add(SEXP val1, SEXP val2) {
#'   return ScalarReal(asReal(val1) + asReal(val2));
#' }
#' 
#' // Multiply 2 numbers
#' SEXP mul(SEXP val1, SEXP val2) {
#'   return ScalarReal(asReal(val1) * asReal(val2));
#' }
#' 
#' // sqrt elements in a vector
#' SEXP new_sqrt(SEXP vec) {
#'   SEXP res = PROTECT(allocVector(REALSXP, length(vec)));
#'   double *res_ptr = REAL(res);
#'   double *vec_ptr = REAL(vec);
#'   for (int i = 0; i < length(vec); i++) {
#'     res_ptr[i] = sqrt(vec_ptr[i]);
#'   }
#'   
#'   UNPROTECT(1);
#'   return res;
#' }
#' "
#' 
#' # compile the code and load into R
#' compile(code)
#' 
#' # Call the functions
#' add(99.5, 0.5)
#' mul(99.5, 0.5)
#' new_sqrt(c(1, 10, 100, 1000))
#'        
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
compile <- function(code, CFLAGS = NULL, PKG_CPPFLAGS = NULL, PKG_LIBS = NULL, env = parent.frame(), 
                    overwrite = "callme", verbosity = 0, invisible = FALSE) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity check code
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  assert_single_string(code)
  stopifnot(overwrite %in% c('all', 'callme', 'functions', 'none'))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Load code from file if it is an existing filename
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (file.exists(code)) {
    code <- paste(readLines(code), collapse = "\n")
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Dump flags if verbosity is very high
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (verbosity >= 4) {
    if (!is.null(CFLAGS)) {
      message("CFLAGS      : ", CFLAGS)
    }
    if (!is.null(PKG_CPPFLAGS)) {
      message("PKG_CPPFLAGS: ", PKG_CPPFLAGS)
    }
    if (!is.null(PKG_LIBS)) {
      message("PKG_LIBS    : ", PKG_LIBS)
    }
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a new directory to work in.   YYYYmmdd-HHMM-[8randomchars]
  # This is so we don't clobber any other 'Makevars' file
  # which might exist.  e.g. two R processes trying to run 'compile()' at 
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
  # Check if we have the two required includes.
  #  #include <R.h>
  #  #include <Rinternals.h>
  #
  # This is the only allowed manipulation of the C source code. 
  # Even then, I'm not broadcasting that I'm doing it :)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!grepl("#include\\s+<Rinternals.h>", code)) {
    code <- paste("#include <Rinternals.h>", code, sep = "\n")
  }
  if (!grepl("#include\\s+<R.h>", code)) {
    code <- paste("#include <R.h>", code, sep = "\n")
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Dump code to a file.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  write(code, c_file)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Set 'CFLAGS' in a temporary R_MAKEVARS_USER
  # If CFLAGS is set here, then this **replaces** the default CFLAGS R 
  # would use when compiling
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  makevars_user <- file.path(tmp_dir, "makevars-user.txt")
  
  if (!is.null(CFLAGS)) {
    assert_single_string(CFLAGS)
    CFLAGS <- paste0("CFLAGS=", CFLAGS)
    writeLines(CFLAGS, makevars_user)
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Write a 'Makevars' with the PKG_CPPFLAGS, PKG_LIBS
  # AFAIK: you cannot set PKG_CPPFLAGS on the command line when doing "R CMD SHLIB" 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  makevars <- c("")
  
  if (!is.null(PKG_CPPFLAGS)) {
    assert_single_string(PKG_CPPFLAGS)
    PKG_CPPFLAGS <- paste0("PKG_CPPFLAGS=", PKG_CPPFLAGS)
    makevars <- c(makevars, PKG_CPPFLAGS)
  }
  
  if (!is.null(PKG_LIBS)) {
    assert_single_string(PKG_LIBS)
    PKG_LIBS <- paste0("PKG_LIBS=", PKG_LIBS)
    makevars <- c(makevars, PKG_LIBS)
  }
  
  writeLines(makevars, "Makevars")
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Setup stdout/stderr to print to console if verbosity is non-zero
  # otherwise just captured to the files named 'stdout', 'stderr'
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
  # Run with a temporary R_MAKEVARS_USER so that we an override
  # R's default CFLAGS 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  command <- paste0(R.home(component = "bin"), "/R")
  args    <- paste("CMD SHLIB", basename(c_file))
  if (verbosity >= 2) {
    message(
      "# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n",
      "# To create DLL file:\n",
      "#   1. Change to working directory\n",
      "#   2. Run 'R CMD SHLIB ...\n",
      "cd ", tmp_dir, "\n", 
      command, " ", args, "\n", 
      "# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
    )
  }
  
  
  ret <- with_makevars_file_env(
    R_MAKEVARS_USER = makevars_user, 
    system2(
      command = command, 
      args    = args,
      stdout  = stdout,
      stderr  = stdout
    ))
  
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
  dyn.load(dll_file)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Automatically generate some wrapper functions in a named list
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  func_list <- create_wrapper_functions(code, dll_file, invisible)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Assign wrapper functions into the nominated environment
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
          if (verbosity >= 3) {
            message("Name exists, but permission has been given to overwrite: '", func_name, "'")
          }
        } else {
          stop("compile(): Not allowed to overwrite existing variable with new wrapper function '", func_name, "'", call. = FALSE)
        }
      }
      if (verbosity >= 2) {
        message("Creating wrapper function '", func_name, "()'")
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
create_wrapper_functions <- function(code, dll_file, invisible) {
  
  decls <- extract_function_declarations(code)
  funcs <- lapply(decls, create_wrapper_function, dll_file = dll_file, invisible = invisible)
  funcs <- Filter(Negate(is.null), funcs)
  
  if (length(funcs) == 0) {
    warning("Code does not appear to contain any functions compatible with .Call()")
  }
  
  unlist(funcs, recursive = FALSE)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Extract function declarations which are consistent with .Call() syntax
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
#' @param dll_file full path the dll file
#' @param invisible logical. Should the result be return invisibly
#' 
#' @return anonymous R function to \code{.Call} the dll
#' 
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_wrapper_function <- function(decl, dll_file, invisible) {
  
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
  if (isTRUE(invisible)) {
    fmt_string <- "function(%s) invisible(.Call(%s, PACKAGE = '%s'))"
  } else {
    fmt_string <- "function(%s) .Call(%s, PACKAGE = '%s')"
  }
  
  func_str <- sprintf(
    fmt_string, 
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

