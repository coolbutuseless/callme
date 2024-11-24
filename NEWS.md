
# callme 0.1.11

* Fix title in DESCRIPTION for CRAN.  Put 'C' in single quotes.
* Remove `cflags_default()`
    * Recommend use of `{maketools}` instead. i.e. `maketools::cc_info()`

# callme 0.1.10 2014-07-23

* Vignettes
* `compile()` changes
    * Read code from file if `code` argument is a valid existing path to a file.
    * Add option `invisible` to return wrapper result invisibly


# callme 0.1.9  2024-07-13

* Support `CFLAGS` argument to replace the default C compiler flags
* `PKG_LDFLAGS` replaced by `PKG_LIBS` and now used in `Makevars` file
  rather than on the command line.

# callme 0.1.8  2024-07-10

* Now writes wrapper functions into the nominated environment

# callme 0.1.7  2023-07-23

* Add `callme_engine()` to enable the use of `callme()` code in
  knitr/Rmarkdown/Quarto chunks.
* Switch to `README.qmd` from `README.Rmd`

# callme 0.1.6  2023-07-22

* Auto-injecting bare mimimum required headers if not present. i.e. `R.h` and `Rinternals.h`
* Raise warning when there doesn't appear to be any `.Call()`-compatible functions in 
  the code

# callme 0.1.5  2023-07-21

* Be stricter about rejecting function signatures which aren't compatible with `.Call()`
  when auto-generating wrappers.

# callme 0.1.4  2023-07-21

* Return a more useful object which contains wrapper functions for 
  all the C functions which adhere to `.Call()` syntax
* Added `verbose` flag to control whether output from the compiler is echoed
  to the terminal. Default: `FALSE`
  
# callme 0.1.3  2023-07-21

* Remove directory where dll was compiled when unloading the dll.

# callme 0.1.2  2023-07-20

* bugfix for directory handling

# callme 0.1.1  2023-07-20

* Explicitly set a finalizer on the returned reference to the DLL such that 
  the library will be unloaded when this returned variable gets garbage collected.
    * User must now keep a reference to the returned object to ensure the 
      C functions remain available.
    * Automatically unloading the library means that
        1. it will not pollute the namespace
        2. Newer versions of the compiled function are not masked by 
           prior compiled versions.
* Switch to use `Makevars` file for setting `CPPFLAGS` as Windows doesn't 
  understant `R CMD COMPILE`



# callme 0.1.0  2023-07-19

* Initial release
