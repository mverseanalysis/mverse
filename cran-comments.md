==> devtools::check()

ℹ Updating mverse documentation
ℹ Loading mverse
Loading required package: multiverse
Loading required package: knitr
Writing NAMESPACE
Writing NAMESPACE
Writing hurricane.Rd
Writing soccer.Rd
── Building ───────────────────────────────────────────────────── mverse ──
Setting env vars:
• CFLAGS    : -Wall -pedantic -fdiagnostics-color=always
• CXXFLAGS  : -Wall -pedantic -fdiagnostics-color=always
• CXX11FLAGS: -Wall -pedantic -fdiagnostics-color=always
• CXX14FLAGS: -Wall -pedantic -fdiagnostics-color=always
• CXX17FLAGS: -Wall -pedantic -fdiagnostics-color=always
• CXX20FLAGS: -Wall -pedantic -fdiagnostics-color=always
───────────────────────────────────────────────────────────────────────────
✔  checking for file ‘/Users/moon/Documents/Projects/mverse/DESCRIPTION’ ...
─  preparing ‘mverse’:
✔  checking DESCRIPTION meta-information ...
─  installing the package to build vignettes
✔  creating vignettes (12.3s)
─  checking for LF line-endings in source and make files and shell scripts
─  checking for empty or unneeded directories
   Removed empty directory ‘mverse/tests/testthat/_snaps’
─  building ‘mverse_0.1.0.tar.gz’
   
── Checking ───────────────────────────────────────────────────── mverse ──
Setting env vars:
• _R_CHECK_CRAN_INCOMING_USE_ASPELL_: TRUE
• _R_CHECK_CRAN_INCOMING_REMOTE_    : FALSE
• _R_CHECK_CRAN_INCOMING_           : FALSE
• _R_CHECK_FORCE_SUGGESTS_          : FALSE
• NOT_CRAN                          : true
── R CMD check ────────────────────────────────────────────────────────────
─  using log directory ‘/Users/moon/Documents/Projects/mverse.Rcheck’
─  using R version 4.1.2 (2021-11-01)
─  using platform: aarch64-apple-darwin20 (64-bit)
─  using session charset: UTF-8
─  using options ‘--no-manual --as-cran’
✔  checking for file ‘mverse/DESCRIPTION’ ...
─  checking extension type ... Package
─  this is package ‘mverse’ version ‘0.1.0’
─  package encoding: UTF-8
✔  checking package namespace information ...
✔  checking package dependencies (2.8s)
✔  checking if this is a source package ...
✔  checking if there is a namespace
✔  checking for executable files ...
✔  checking for hidden files and directories ...
✔  checking for portable file names ...
✔  checking for sufficient/correct file permissions
✔  checking whether package ‘mverse’ can be installed (3.8s)
✔  checking installed package size ...
✔  checking package directory ...
✔  checking for future file timestamps ...
✔  checking ‘build’ directory
✔  checking DESCRIPTION meta-information ...
✔  checking top-level files ...
✔  checking for left-over files
✔  checking index information ...
✔  checking package subdirectories ...
✔  checking R files for non-ASCII characters ...
✔  checking R files for syntax errors ...
✔  checking whether the package can be loaded (497ms)
✔  checking whether the package can be loaded with stated dependencies (463ms)
✔  checking whether the package can be unloaded cleanly (461ms)
✔  checking whether the namespace can be loaded with stated dependencies (445ms)
✔  checking whether the namespace can be unloaded cleanly (481ms)
✔  checking loading without being on the library search path (515ms)
✔  checking dependencies in R code (507ms)
✔  checking S3 generic/method consistency (1.2s)
✔  checking replacement functions (466ms)
✔  checking foreign function calls (498ms)
✔  checking R code for possible problems (2.9s)
✔  checking Rd files ...
✔  checking Rd metadata ...
✔  checking Rd line widths ...
✔  checking Rd cross-references ...
✔  checking for missing documentation entries (614ms)
✔  checking for code/documentation mismatches (1.8s)
✔  checking Rd \usage sections (1.3s)
✔  checking Rd contents ...
✔  checking for unstated dependencies in examples ...
✔  checking contents of ‘data’ directory (520ms)
✔  checking data for non-ASCII characters (654ms)
✔  checking LazyData
✔  checking data for ASCII and uncompressed saves ...
✔  checking installed files from ‘inst/doc’ ...
✔  checking files in ‘vignettes’ ...
✔  checking examples (7.8s)
✔  checking for unstated dependencies in vignettes ...
✔  checking package vignettes in ‘inst/doc’ ...
✔  checking re-building of vignette outputs (10.7s)
✔  checking for non-standard things in the check directory
✔  checking for detritus in the temp directory
   
   
── R CMD check results ────────────────────────────────── mverse 0.1.0 ────
Duration: 40.2s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded

This is the first CRAN release.
