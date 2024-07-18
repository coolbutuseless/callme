

test_that("compilation works", {

  code <- "
#include <R.h>
#include <Rinternals.h>

SEXP add(SEXP val1, SEXP val2) {
  return ScalarReal(asReal(val1) + asReal(val2));
}
"
  
  # Keep reference to returned object.
  # C library will be unloaded when 'dll' variable is garbage collected.
  compile(code)
  
  # Manual call
  expect_equal(add(1, 2.5), 3.5)
  expect_equal(add(99.5, 0.5), 100)
  
})
