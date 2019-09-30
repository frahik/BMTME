## Test environments

* MacOS 10.15 - R 3.6.1
* win-builder (devel and release)
* chech_rhub (devel)
* TravisCI (devel and release)

## R CMD check results

There were no ERRORs, or WARNINGs.

There were 1 NOTE:

Uses the superseded package: 'doSNOW'

R CMD check succeeded

## Last round comments
This does

clang++ -std=gnu++11 -shared -L/usr/local/clang/lib64 -L/usr/local/lib64
-o BMTME.so RcppExports.o Sample_IW.o -lstdc++
-L/data/gannet/ripley/R/R-clang/lib -lRlapack
-L/data/gannet/ripley/R/R-clang/lib -lRblas -lgfortran -lm -lquadmath

The hard-coded inclusion of g++'s C++ library is wrong and harmful.

I don't know what you think

CC=ccache clang -Qunused-arguments
CXX=ccache clang++ -Qunused-arguments

does, but users cannot override these in src/Makevars so this should be
cleaned up.

Please correct ASAP and before Oct 12 to retain the package on CRAN.

## Special comments

This version implements the corrections mentioned in the last round of comments

### First submission
I am using the doSNOW package, to be able to show a progress bar in the advance of the calculations, since it is a very demanding algorithm in time (in large data sets usually takes at least 8 hours). I think it is convenient to show the progress bar when the work is done in parallel to take advantage of all the cores of the computer, as well as to be able to show the user the progress of cross validations.

To be more specific, this package is only used when cross validation is applied to the model, and it has been specified in one of the functions available in the package that more than one kernel of the computer is to be used.
