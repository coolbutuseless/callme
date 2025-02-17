

test_that("Support 'const SEXP'", {

  code <- r"(
  SEXP add1(SEXP x_, SEXP y_) {
    double *x = REAL(x_);
    double *y = REAL(y_);
  
    for (int i = 0; i < Rf_length(x_); i++) {
      x[i] = x[i] + y[i];
    }
  
    return x_;
  })"
  
  callme::compile(code)
  
  N <- 10000
  x <- runif(N)
  xorig <- 1.0 * x
  y <- runif(N)
  expect_equal(add1(x, y), xorig + y)
  
  
  
  code <- r"(
  SEXP add2(const SEXP x_, const SEXP y_) {
    double *x = REAL(x_);
    double *y = REAL(y_);
  
    for (int i = 0; i < Rf_length(x_); i++) {
      x[i] = x[i] + y[i];
    }
  
    return x_;
  })"
  
  callme::compile(code)
  
  N <- 10000
  x <- runif(N)
  xorig <- 1.0 * x
  y <- runif(N)
  expect_equal(add2(x, y), xorig + y)
  
  
  
})

