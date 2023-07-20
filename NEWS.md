
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
