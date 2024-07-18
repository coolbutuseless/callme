test_that("function declaration parser works", {
  
  code <- r"(
#include <R.h>
#include <Rinternals.h>
  
SEXP version_() {
  return ScalarInteger(1);
}

int crap() {
   Rprintf("Hello\n");
   return 999;
}

SEXP two(SEXP vara, SEXP varb) {
  return ScalarInteger(2);
}


SEXP three(
            SEXP vara
) {
  return ScalarInteger(3);
}


SEXP not_valid(int vara, SEXP varb) {
  return ScalarInteger(2);
}

)"

  # Declaration extraction works
  decls <- callme:::extract_function_declarations(code)
  expect_identical(
    decls,
    c("SEXP version_()", "SEXP two(SEXP vara, SEXP varb)", "SEXP three( SEXP vara )", "SEXP not_valid(int vara, SEXP varb)")
  )
  
  
  funcs <- callme:::create_wrapper_functions(code, dll_file = "jnk", invisible = FALSE)
  
  # Created 3 functions
  expect_length(funcs, 3)
  expect_named(funcs)
  expect_identical(names(funcs), c('version_', 'two', 'three'))

  # Function arguments are good
  expect_null(formalArgs(funcs[[1]]))
  expect_identical(formalArgs(funcs[[2]]), c('vara', 'varb'))
  expect_identical(formalArgs(funcs[[3]]), c('vara'))
    
})


test_that("argument parsing works", {
  
  decl <- "SEXP one(SEXP two, SEXP three)"
  expect_identical(
    callme:::extract_args_from_declaration(decl),
    c('two', 'three')
  )
  
  
  
  decl <- "SEXP one(int two, SEXP three)"
  expect_true(
    is.na(
      callme:::extract_args_from_declaration(decl)
    )
  )  
  
  decl <- "SEXP one()"
  expect_null(
    callme:::extract_args_from_declaration(decl)
  )
  
  decl <- "SEXP one(  )"
  expect_null(
    callme:::extract_args_from_declaration(decl)
  )
  
  decl <- "SEXP one(void)"
  expect_null(
    callme:::extract_args_from_declaration(decl)
  )
  
  decl <- "SEXP one(   void    )"
  expect_null(
    callme:::extract_args_from_declaration(decl)
  )
  
  
})
