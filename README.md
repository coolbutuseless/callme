

<!-- README.md is generated from README.Rmd. Please edit that file -->

# callme <img src="man/figures/logo.png" align="right" height="400/"/>

<!-- badges: start -->

![](https://img.shields.io/badge/cool-useless-green.svg)
<!-- badges: end -->

`{callme}` compiles C code to loadable dynamic libraries callable via
`.Call()`. This is useful for rapid prototyping of C code.

Code is defined in a character string, and must be valid C code callable
via `.Call()`.

Features:

- Simple. Purpose is to support `.Call()` only.
- Straightforward. Requires user to submit complete C code - including
  header `#include` directives.
- Flexible. Includes explicit handling for `PKG_CPPFLAGS` and `PKG_LIBS`
  for setting C pre-processor flags, and library linking flags so code
  can link to other libraries installed on the system.
- Generates R functions to call the compiled C functions.
- Multiple functions allowed in a single code block.

### What’s in the box

- `callme(code, cpp_flags = NULL, ld_flags = NULL, env = .GlobalEnv, verbosity = 0)`
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

SEXP add(SEXP val1, SEXP val2) {
  return ScalarReal(asReal(val1) + asReal(val2));
}
"

# compile the code
callme(code)

# Call the function
add(99.5, 0.5)
```

    #> [1] 100

## Linking against an installed library

In this example we want to get the version of the `zstd` library which
is (already) installed on the computer, and return it as a character
string.

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
callme(code, cpp_flags = "-I/opt/homebrew/include", ld_flags = "-L/opt/homebrew/lib -lzstd")

# Call the function
zstd_version()
```

    #> [1] "1.5.6"

## Use in a code chunk in Rmarkdown/Quarto

#### Set the `knitr` engine to handle `callme` code blocks

``` r
knitr::knit_engines$set(callme = callme::callme_engine)
```

#### Possible chunk options

Possible configure variables at start of chunk:

- `dllname` the variable name to hold the results of `callme()`.
  Default: `dll`
- `cpp_flags` the equivalent of the argument `callme(cpp_flags = ...)`
- `ld_flags` the equivalent of the argument `callme(ld_flags = ...)`

#### Example `callme` code chunk in Quarto

```` markdown
```{callme}
#| dllname: mydll
#include <R.h>
#include <Rdefines.h>

SEXP add(SEXP val1, SEXP val2) {
  return ScalarReal(asReal(val1) + asReal(val2));
}


SEXP mul(SEXP val1, SEXP val2) {
  return ScalarReal(asReal(val1) * asReal(val2));
}
```
````

#### Echo of code when knitting document

``` c
#| dllname: mydll
```

``` c
#include <R.h>
#include <Rdefines.h>

SEXP add(SEXP val1, SEXP val2) {
  return ScalarReal(asReal(val1) + asReal(val2));
}


SEXP mul(SEXP val1, SEXP val2) {
  return ScalarReal(asReal(val1) * asReal(val2));
}
```

#### Running the compiled C code

``` r
add(3, 2)
#> [1] 5
mul(3, 2)
#> [1] 6
```
