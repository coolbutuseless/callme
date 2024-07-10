

<!-- README.md is generated from README.Rmd. Please edit that file -->

# callme <img src="man/figures/logo.png" align="right" height="400/"/>

<!-- badges: start -->

![](https://img.shields.io/badge/cool-useless-green.svg)
<!-- badges: end -->

`{callme}` compiles C code and generates wrappers so that the C code can
be called easily from R.

Code is defined in a character string, and must be valid C code matching
R’s `.Call()` syntax.

Features:

- Supports `.Call()` syntax only.
- User submits complete C code - including functino declaration and
  header `#include` directives.
- Explicit handling for `PKG_CPPFLAGS` and `PKG_LDFLAGS` for setting C
  pre-processor flags, and library linking flags so code can link to
  other libraries installed on the system.
- Generates R functions to call the compiled C functions.
- Multiple functions allowed in a single code block.

### What’s in the box

- `callme(code, PKG_CPPFLAGS = NULL, PKG_LDFLAGS = NULL, env = .GlobalEnv, verbosity = 0)`
  compile the `code` and assign R functions into the nominated `env` in
  R.

### `.Call()` compatible C functions

`.Call()` requires any C functions to be accessed from R must only take
`SEXP` arguments, and return a `SEXP` value.
e.g. `SEXP add(SEXP x, SEXP y) { ... }`

## Installation

You can install from [GitHub](https://github.com/coolbutuseless/callme)
with:

``` r
# install.package('remotes')
remotes::install_github('coolbutuseless/callme')
```

## Example

The following example compiles a code snippet into a C library and then
shows how to call the function from R.

``` r
library(callme)

code <- "
#include <R.h>
#include <Rdefines.h>

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
callme(code)

# Call the functions
add(99.5, 0.5)
```

    #> [1] 100

``` r
mul(99.5, 0.5)
```

    #> [1] 49.75

``` r
new_sqrt(c(1, 10, 100, 1000))
```

    #> [1]  1.000000  3.162278 10.000000 31.622777

## Linking against an installed library

In this example we want to get the version of the `zstd` library (which
is installed on the computer), and return it as a character string.

We need to tell R when compiling the code:

- to look in the `/opt/homebrew/include` directory for `zstd.h`.
- to look for the actual `zstd` library in `/opt/homebrew/lib`.
- to link to the `zstd` library (`-lzstd`)

Note: This if for `zstd` installed via `homebrew` on macOS. Paths will
be different for other operating systems.

``` r
code <- r"(
#include <R.h>
#include <Rdefines.h>
#include "zstd.h"
  
SEXP zstd_version() {
  return mkString(ZSTD_versionString());
}
)"

# Compile the code 
callme(code, 
       PKG_CPPFLAGS = "-I/opt/homebrew/include", 
       PKG_LDFLAGS  = "-L/opt/homebrew/lib -lzstd")

# Call the function
zstd_version()
```

    #> [1] "1.5.6"
