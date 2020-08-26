## Test environments

* MacOS 
  + R 4.0.2
* TravisCI
  + Linux R devel
  + Linux R release
* AppVeyor
  + Windows R 4.0.2
* Github Actions
  + MacOS R devel
  + MacOS R release
  + Windows R 4.0
  + Ubuntu R 4.0
  + Ubuntu R 3.6
  + Ubunty R 3.5

## R CMD check results

There were no ERRORs, or WARNINGs.

There were 1 NOTE:

Uses the superseded package: 'doSNOW'

R CMD check succeeded

## Last round comments

[SOLVED] package BMTME_1.0.17.tar.gz does not pass the incoming checks

[SOLVED] Prof Brian says:
The Solaris result indicates what is wrong with the tests (skipped on
most platforms).  Thanks to a recurrence of a bug in Rcpp, you need to add
Imports: Rcpp
to DESCRIPTION and
importFrom(Rcpp, evalCpp)
to NAMESPACE.

## Special comments

### First submission
I am using the doSNOW package, to be able to show a progress bar in the advance of the calculations, since it is a very demanding algorithm in time (in large data sets usually takes at least 8 hours). I think it is convenient to show the progress bar when the work is done in parallel to take advantage of all the cores of the computer, as well as to be able to show the user the progress of cross validations.

To be more specific, this package is only used when cross validation is applied to the model, and it has been specified in one of the functions available in the package that more than one kernel of the computer is to be used.
