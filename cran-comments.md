## Test environments

* MacOS [R 4.0]
* win-builder (devel and release)
* chech_rhub (devel)
* TravisCI (devel and release)

## R CMD check results

There were no ERRORs, or WARNINGs.

There were 1 NOTE:

Uses the superseded package: 'doSNOW'

R CMD check succeeded

## Last round comments


### V 1.0.15

This version fix a test errors with the change of factor=False by default of R.

## Special comments

This version implements the corrections mentioned in the last round of comments

### First submission
I am using the doSNOW package, to be able to show a progress bar in the advance of the calculations, since it is a very demanding algorithm in time (in large data sets usually takes at least 8 hours). I think it is convenient to show the progress bar when the work is done in parallel to take advantage of all the cores of the computer, as well as to be able to show the user the progress of cross validations.

To be more specific, this package is only used when cross validation is applied to the model, and it has been specified in one of the functions available in the package that more than one kernel of the computer is to be used.
