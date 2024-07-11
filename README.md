

<!-- README.md is generated from README.Rmd. Please edit that file -->

# callme <img src="man/figures/logo.png" align="right" height="400/"/>

<!-- badges: start -->

![](https://img.shields.io/badge/cool-useless-green.svg)
[![R-CMD-check](https://github.com/coolbutuseless/callme/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/coolbutuseless/callme/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`{callme}` compiles C code and generates wrappers so that the C code can
be called easily from R.

Code is defined in a character string, and must be valid C code matching
R’s `.Call()` syntax.

Features:

- Supports `.Call()` syntax only.
- User submits complete C code - including function declaration and
  header `#include` directives.
- Explicit handling for `PKG_CPPFLAGS` and `PKG_LDFLAGS` for setting C
  pre-processor flags, and library linking flags so code can link to
  other libraries installed on the system.
- Generates R functions to call the compiled C functions.
- Multiple functions allowed in a single code block.

### What’s in the box

- `compile(code, PKG_CPPFLAGS = NULL, PKG_LDFLAGS = NULL, env = .GlobalEnv, verbosity = 0)`
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
compile(code, 
       PKG_CPPFLAGS = "-I/opt/homebrew/include", 
       PKG_LDFLAGS  = "-L/opt/homebrew/lib -lzstd")

# Call the function
zstd_version()
```

    #> [1] "1.5.6"

# In-depth case study: A faster `sqrt()`

In this case study, I am investigating a faster `sqrt()` in R.

R’s `sqrt()` is already pretty fast as it calls the system math sqrt
function internally. But can we go faster??

In the following sections I have written

- `sqrt_simple()` which is a naive implementation in C
- `sqrt_unrolled()` is an implementation in which I have manually
  unrolled the for loop.
- `sqrt_simd_avx()` which uses AVX SIMD instructions. This is
  particularly interesting as the code is running on macOS which doesn’t
  have AVX instructions!

### Bespoke `sqrt()` in C

``` r
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Generic 'scalar' sqrt()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
code_sqrt_simple <- r"(
#include <R.h>
#include <Rdefines.h>

// sqrt elements in a vector
SEXP sqrt_simple(SEXP vec) {
  SEXP res = PROTECT(allocVector(REALSXP, length(vec)));
  double *res_ptr = REAL(res);
  double *vec_ptr = REAL(vec);
  for (int i = 0; i < length(vec); i++) {
    res_ptr[i] = sqrt(vec_ptr[i]);
  }
  
  UNPROTECT(1);
  return res;
}
)"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# compile the code and benchmark against standard sqrt()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
callme::compile(code_sqrt_simple)

vec <- runif(80000)
bench::mark(
  sqrt(vec),
  sqrt_simple(vec),
  check = TRUE
)
```

    #> # A tibble: 2 × 6
    #>   expression            min   median `itr/sec` mem_alloc `gc/sec`
    #>   <bch:expr>       <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
    #> 1 sqrt(vec)            72µs    106µs     9103.     625KB    107. 
    #> 2 sqrt_simple(vec)    135µs    159µs     6178.     625KB     72.5

This simple implentation is slightly faster than the built-in version in
R. It’s probably faster because it doesn’t do *anything* to check the
sanity of the inputs.

### Bespoke `sqrt()` in C with unrolled loops

[Unrolling loops can make code
faster](https://lemire.me/blog/2019/04/12/why-are-unrolled-loops-faster/).
The compiler can do this automatically in many situations. Here, I’ve
chosen to manually unroll the main loop.

``` r
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Manual loop unrolling
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
code_sqrt_unrolled <- r"(
// sqrt elements in a vector
SEXP sqrt_unrolled(SEXP vec) {
  SEXP res = PROTECT(allocVector(REALSXP, length(vec)));
  double *res_ptr = REAL(res);
  double *vec_ptr = REAL(vec);
#define UNROLL 4
  int i = 0;
  if (length(vec) > (UNROLL - 1)) {
    for (; i < length(vec) - (UNROLL - 1); i+=UNROLL) {
      res_ptr[i + 0] = sqrt(vec_ptr[i + 0]);
      res_ptr[i + 1] = sqrt(vec_ptr[i + 1]);
      res_ptr[i + 2] = sqrt(vec_ptr[i + 2]); 
      res_ptr[i + 3] = sqrt(vec_ptr[i + 3]);
    }
  }
  for (; i < length(vec); i++) {
    res_ptr[i] = sqrt(vec_ptr[i]);
  }
  
  UNPROTECT(1);
  return res;
}
)"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# compile the code and benchmark against standard sqrt()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
callme::compile(code_sqrt_unrolled)

vec <- runif(80000)
bench::mark(
  sqrt(vec),
  sqrt_simple(vec),
  sqrt_unrolled(vec),
  check = TRUE
)
```

    #> # A tibble: 3 × 6
    #>   expression              min   median `itr/sec` mem_alloc `gc/sec`
    #>   <bch:expr>         <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
    #> 1 sqrt(vec)              72µs  106.1µs     9084.     625KB    113. 
    #> 2 sqrt_simple(vec)    135.3µs  157.8µs     6291.     625KB     76.2
    #> 3 sqrt_unrolled(vec)   45.4µs   72.2µs    13543.     625KB    166.

The above benchmark shows that unrolling the loop gives a signifcant
speed advantage over the simple implementation

### Bespoke `sqrt()` in C using AVX SIMD on mac ARM

This gets a little bit into the weeds:

- Most CPUs have SIMD instructions
- x86 has MMX, SSE, AVX etc
- mac ARM processors have Neon
- In the code below is a `sqrt()` for macOS using AVX instructions which
  don’t actually exist on the mac ARM CPU.

The magic is [SIMDe](https://github.com/simd-everywhere/simde) - this
“header-only library provides fast, portable implementations of SIMD
intrinsics on hardware which doesn’t natively support them”.

The following SIMD code should work on both x86 **and** mac ARM.

``` r
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Using AVX SIMD instructions and the "SIMDE" compatibility library
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
code_sqrt_simd_avx <- r"(
#define SIMDE_ENABLE_NATIVE_ALIASES 1

#if defined(__AVX__) || defined(__aarch64__)
  #include "simde/x86/avx.h"
#else
  #warning AVX2/NEON support is not available. Code will not compile.
#endif

SEXP sqrt_simd_avx(SEXP vec) {
  if (length(vec) % 4 != 0) { error("must be multiple of 4"); }

  SEXP res = PROTECT(allocVector(REALSXP, length(vec)));
  double *vec_ptr = REAL(vec);
  double *res_ptr = REAL(res);
 
  int i = 0;
  for (; i < length(vec) - 3; i += 4) {
    __m256d a = _mm256_load_pd(vec_ptr + i);
    __m256d b = _mm256_sqrt_pd(a);
    _mm256_storeu_pd(res_ptr + i, b);
  }
  for (; i < length(vec); i++) {
    res_ptr[i] = sqrt(vec_ptr[i]);
  }

  UNPROTECT(1);
  return res;
}
)"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# compile the code 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
callme::compile(code_sqrt_simd_avx, 
                PKG_CPPFLAGS = '-march=native -O3 -I"/Users/mike/projectsdata/simde/"')

vec <- runif(80000)
bench::mark(
  sqrt(vec),
  sqrt_simple(vec),
  sqrt_unrolled(vec),
  sqrt_simd_avx(vec),
  check = TRUE
)
```

    #> # A tibble: 4 × 6
    #>   expression              min   median `itr/sec` mem_alloc `gc/sec`
    #>   <bch:expr>         <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
    #> 1 sqrt(vec)              72µs  104.2µs     9267.     625KB    120. 
    #> 2 sqrt_simple(vec)    135.4µs  158.1µs     6354.     625KB     84.2
    #> 3 sqrt_unrolled(vec)   45.5µs   73.7µs    13687.     625KB    174. 
    #> 4 sqrt_simd_avx(vec)   34.7µs   66.8µs    15512.     625KB    192.

Using SIMD instructions further speeds up the code! This speed-up occurs
even though no ARM SIMD (i.e. Neon) was written. Instead, I wrote AVX
SIMD intrinsics (for x86) and let he SIMDe library take care of the
translation.
