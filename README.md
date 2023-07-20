
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

- Simple. Supports `.Call()` only
- Straightforward. Requires complete C code - including header includes,
  and value unpacking.
- Flexible. Includes explicit handling for `PKG_CPPFLAGS` and `PKG_LIBS`
  for setting C pre-processor flags, and library linking flags so code
  can link to other libraries installed on the system.

### What’s in the box

- `callme(code, cpp_flags, ld_flags)` compile and load the function into
  R.

### What’s new

#### v0.1.1 2023-07-20

- Automatically unload the dll
- Switch to using `Makevars` for setting `CPPFLAGS` to be compatible
  with Windows (which doesn’t support `R CMD COMPILE`)

#### v0.1.0 2023-07-19

- Initial release

### Limitations

- Very little sanity checking. You must already know how to write C
  code!
- As required by `.Call()`
  - The C functions must only take `SEXP` arguments, and return a `SEXP`
    value.
  - All conversion of `SEXP` to C objects must be written explicitly.
    There’s no magic here!
- It’s up to the user to track the name of the function in C which is to
  be used as the first argument to `.Call()`.
- This package tries not to assume anything about your code, or
  magically take care of object conversion between R and C.
- Supports only C code in alignment with `.Call()` calling conventions.
- Need to have standard `#include` statements in code (No magic!)
- No support for other calling conventions like `.C()` or `.Fortran()`

## Installation

You can install from [GitHub](https://github.com/coolbutuseless/callme)
with:

``` r
# install.package('remotes')
remotes::install_github('coolbutuseless/callme')
```

## Example

The following example compiles a code snippet into a C library and then
executes the function from R using `.Call()`.

The function name in C is `add_`, which means that this is the first
argument to `.Call()` i.e. `.Call("add_", ...)`.

The function name could be almost any valid C function name, but I
prefer to use an underscore suffix to indicate that this a C function
meant to be called from R.

``` r
library(callme)

code <- "
#include <R.h>
#include <Rdefines.h>

SEXP add_(SEXP val1_, SEXP val2_) {
  return ScalarReal(asReal(val1_) + asReal(val2_));
}
"

# Keep reference to returned object.
# C library will be unloaded when 'dll' is garbage collected.
dll <- callme(code)

.Call("add_", 1, 2.5)
```

    [1] 3.5

## Linking against an installed library

The following code snippet asks for the version of the `zstd` library
which is (already) installed on the computer, and return it as a
character string.

We need to tell R when compiling the code:

- to look in the `/opt/homebrew/include` directory for `zstd.h`.
- to look for the actual `zstd` library in `/opt/homebrew/lib`.
- to link to the `zstd` library (`-lzstd`)

``` r
  code <- r"(
#include <R.h>
#include <Rdefines.h>
#include "zstd.h"
  
SEXP version_() {
  return mkString(ZSTD_versionString());
}
)"

# Keep reference to returned object.
# C library will be unloaded when 'dll' is garbage collected.
dll <- callme(code, cpp_flags = "-I/opt/homebrew/include", ld_flags = "-L/opt/homebrew/lib -lzstd")

.Call("version_")
```

    [1] "1.5.5"

## Acknowledgements

- R Core for developing and maintaining the language.
- CRAN maintainers, for patiently shepherding packages onto CRAN and
  maintaining the repository
