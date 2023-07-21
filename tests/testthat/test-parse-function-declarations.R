test_that("function declaration parser works", {
  code <- r"(
#include <R.h>
#include <Rdefines.h>
  
SEXP version_() {
  return ScalarInteger(1);
}

int crap_() {
   Rprintf("Hello\n");
   return 999;
}

SEXP two_(SEXP vara, SEXP varb) {
  return ScalarInteger(2);
}



SEXP three_(
            SEXP vara
) {
  return ScalarInteger(3);
}


)"

  # Declaration extraction works
  decls <- callme:::extract_function_declarations(code)
  expect_identical(
    decls,
    c("SEXP version_()", "SEXP two_(SEXP vara, SEXP varb)", "SEXP three_( SEXP vara )")
  )
  
  
  funcs <- callme:::create_wrapper_functions(code)
  
  # Created 3 functions
  expect_length(funcs, 3)
  expect_named(funcs)
  expect_identical(names(funcs), c('version_', 'two_', 'three_'))

  # Function arguments are good
  expect_null(formalArgs(funcs[[1]]))
  expect_identical(formalArgs(funcs[[2]]), c('vara', 'varb'))
  expect_identical(formalArgs(funcs[[3]]), c('vara'))
    
})

