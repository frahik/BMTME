BMTME v1.0.19
==============

Changes:

 * [FIX] Rcpp bug to run test


BMTME v1.0.17
==============

Changes:

 * [FIX] Rcpp bug to run test
 * [FIX] test of the package
 * [IMP] Random partition Environment and Traits now are exported as character, not as factor.
 
BMTME v1.0.12
==============

Changes:

 * [FIX] Users can overwrite the package compiler.

BMTME v1.0.11
==============

Changes:

 * [FIX] Users can overwrite the package compiler.

BMTME v1.0.10
==============

Changes:

 * [FIX] clang: warnings
 * [FIX] Deprecated `funs` from dplyr

BMTME v1.0.6
==============

Changes:

 * [Fix bug] Fixed eigen values in `BME` and `BMTME` functions.

BMTME v1.0.5
==============

Changes:

 * [Update] Fixed package to keep it compatible with new version of dplyr.
 * [Update] New parameter `tolerance` in cholesky function.


BMTME v1.0.4  [First CRAN release]
==============

Changes:

 * [Update] Examples provided to BMORS, BME, BMTME and BMORS_Env functions.
 * [Update] Description of the package.

BMTME v1.0.3
==============

Changes:

 * [Update] New tests was added.
 * [refactoring code] Update validation of cores to use only the 50% of the available cores.
 * [refactoring code] Change to snow and doSNOW packages to parallel work.
 * [refactoring code] Bug fixed.

BMTME v1.0.2
==============

Changes:

 * [Update] New tests was added.
 * [refactoring code] Now the progress bar with parallel work it is with pbapply package
 * [refactoring code] Change snow and doSNOW packages to parallel package
 * [refactoring code] New function to crossvalidation, easier to maintain.
 * [Update] Add a good description.
 

BMTME v1.0.1
==============

Changes:
 
 * [Fixed] Makevars.win to compile C++ code.
 * [refactoring code] cholesky function.


BMTME v1.0.0
==============

Changes:
 * Fixed typo
 * Include vignettes
 * Removed unused code
 
BMTME v0.0.26
==============

Changes:
 * Improvements in tests
 * Minor fixes
 

BMTME v0.0.25
==============

Changes:
 
 * Include coverage test
 * Typo fixed
 * Fixed bug in `BMTME()` function with a vector as testingSet parameter.
 * Fixed bug in `RandomPartition()` function.
 * Implemented predictions with not crossvalidation in `BMORS()` function.
 * Include `print()` function to `BMORS` object with not crossvalidation.
 

BMTME v0.0.24
==============

Changes:

 * Removed stratified random partitions.
 * Fixed barplot for BMORS_Env.
 * Good practice with <-
 * complete F or T, logical arguments.
 * Bug fixed: changed 1:length to seq_len to avoid bugs.
 * Include ORCID.

BMTME v0.0.23
==============

Changes:

 * Include more tests.
 * Fixed print message of `BMORS()` and `BMORS_Env()` functions.
 * Fixed parallel work of `BMORS()` function.
 
BMTME v0.0.22
==============

Changes:

 * Important updates to the documentation.
 * Fixed MAC-OS 10.11 El Capitan, installation (issue #1).
 
BMTME v0.0.21
==============

Changes:

 * `BMTERS()` function now is `BMORS()` function.
 * `BMTERS_Env()` function now is `BMORS_Env()` function.

BMTME v0.0.20
==============

Changes:

 * Update Windows c++ support.
 * Fixed issues #1 (Installation in Windows) and #2 (Typos). Thanks to @volpatoo and @j450h1 by reporting it.

BMTME v0.0.19
==============

Changes:
 
 * Update plot and boxplot functions
 
BMTME v0.0.18
==============

Changes:
 
 * New `predictor_Sec_complete` parameter in `BMTMERS_Env()` function.
 
BMTME v0.0.17
==============

Changes:

 * New `predictor_Sec_complete` parameter in `BMTMERS()` function.

BMTME v0.0.16
==============

Changes:

 * The MSEP was changed to MAAPE for the error estimations of the predictions.
 * Minor fixes in the documentation.
 * Now the boxplots can be ordered by the MAAPE.
 * fixed the predicted output.
 
BMTME v0.0.15
==============

Changes:
 
 * Implement parallel mode in the BMTMERS function
 * Implement validation to `parallelCores` parameter in the functions that could use it. 
 * export `n_cores` used to fit the models that could use `parallelCores` parameter.
 * fixed class of `BME` function.
 
BMTME v0.0.14
==============

Changes:

 * Update Unix support
 
BMTME v0.0.13
==============

Changes:

 * Initial development is in progress, but there has not yet been a stable, usable release suitable for the public; this is a pre-release, be careful.

BMTME v0.0.1
==============

Changes:

* Initial commit
