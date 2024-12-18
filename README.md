

<!-- README.md is generated from README.Rmd. Please edit that file -->

# callme <img src="man/figures/logo.png" align="right" width="30%" />

<!-- badges: start -->

![](https://img.shields.io/badge/cool-useless-green.svg) [![CRAN
status](https://www.r-pkg.org/badges/version/callme.png)](https://CRAN.R-project.org/package=callme)
[![R-CMD-check](https://github.com/coolbutuseless/callme/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/coolbutuseless/callme/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`{callme}` compiles inline C code and generates wrappers so that the C
code can be easily called from R.

Features:

- Compile inline C code (or code from a file) and makes it immediately
  (and easily!) available to R.
- Accepts complete C code - including function declaration and header
  `#include` directives.
- Explicit handling for `CFLAGS`, `PKG_CPPFLAGS` and `PKG_LIBS` for
  setting compiler flags, C pre-processor flags, and library linking
  flags, respectively.
- Generates R functions to call the compiled C functions.
- Multiple function definitions allowed in a single code block.

### What’s in the box

- `compile(code, CFLAGS, PKG_CPPFLAGS, PKG_LIBS, env, verbosity)`
  compile the C `code` and assign R functions into the nominated `env`
  in R. C code could be as a string or in a file.

## Installation

This package can be installed from CRAN

``` r
install.packages('callme')
```

You can install the latest development version from
[GitHub](https://github.com/coolbutuseless/callme) with:

``` r
# install.package('remotes')
remotes::install_github('coolbutuseless/callme')
```

Pre-built source/binary versions can also be installed from
[R-universe](https://r-universe.dev)

``` r
install.packages('callme', repos = c('https://coolbutuseless.r-universe.dev', 'https://cloud.r-project.org'))
```

## Example

The following example compiles a code snippet into a C library and
creates a wrapper function in R (of the same name) which can be used to
call the compiled code.

``` r
library(callme)

code <- "
#include <R.h>
#include <Rinternals.h>

// Add 2 numbers
SEXP add(SEXP val1, SEXP val2) {
  return ScalarReal(asReal(val1) + asReal(val2));
}

// Multiply 2 numbers
SEXP mul(SEXP val1, SEXP val2) {
  return ScalarReal(asReal(val1) * asReal(val2));
}

// sqrt elements in a vector
SEXP new_sqrt(SEXP vec) {
  SEXP res = PROTECT(allocVector(REALSXP, length(vec)));
  double *res_ptr = REAL(res);
  double *vec_ptr = REAL(vec);
  for (int i = 0; i < length(vec); i++) {
    res_ptr[i] = sqrt(vec_ptr[i]);
  }
  
  UNPROTECT(1);
  return res;
}
"

# compile the code
compile(code)

# Call the functions
add(99.5, 0.5)
```

    #> [1] 100

``` r
mul(99.5, 0.5)
```

    #> [1] 49.75

``` r
new_sqrt(c(1, 4, 25, 999))
```

    #> [1]  1.00000  2.00000  5.00000 31.60696

## Linking against an installed library

In this example we want to get the version of the `zstd` library (which
has already been installed on the computer), and return it as a
character string.

We need to tell R when compiling the code:

- to look in the `/opt/homebrew/include` directory for `zstd.h`.
- to look for the actual `zstd` library in `/opt/homebrew/lib`.
- to link to the `zstd` library (`-lzstd`)

Note: This code works for `zstd` installed via `homebrew` on macOS.
Paths will be different for other operating systems.

``` r
code <- r"(
#include <R.h>
#include <Rinternals.h>
#include "zstd.h"
  
SEXP zstd_version() {
  return mkString(ZSTD_versionString());
}
)"

# Compile the code 
compile(code, 
       PKG_CPPFLAGS = "-I/opt/homebrew/include", 
       PKG_LIBS     = "-L/opt/homebrew/lib -lzstd")

# Call the function
zstd_version()
```

    #> [1] "1.5.6"

# References

- Hadley’s [R internals](https://github.com/hadley/r-internals).
- [Advanced R Book](http://adv-r.had.co.nz/C-interface.html) has a
  specfic chapter or R’s interface to C.
- Ella Kay’s
  [UserR2024](https://userconf2024.sched.com/event/1c8zS/c-for-r-users-ella-kaye-university-of-warwick)
  conference presentation: [“C for R
  users”](https://static.sched.com/hosted_files/userconf2024/84/c-for-r-users.pdf)
- Book: [Deep R
  Programming](https://deepr.gagolewski.com/chapter/310-compiled.html)
- Davis Vaughan’s [Now you C
  me](https://blog.davisvaughan.com/posts/2019-03-02-now-you-c-me/)
- [c3po](https://github.com/ramiromagno/c3po)
- [R Native API](https://github.com/HenrikBengtsson/RNativeAPI)

### R project official documentation

- Writing R extensions Section 5 [System and foreign language
  interfaces](https://cran.r-project.org/doc/manuals/R-exts.html#System-and-foreign-language-interfaces)
- [R
  internals](https://cran.stat.auckland.ac.nz/doc/manuals/r-devel/R-ints.html)
