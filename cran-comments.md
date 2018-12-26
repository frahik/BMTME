## Test environments

* Linux - Manjaro R-base [3.5.1]
* Windows 10 R [3.5.1]
* win-builder (devel and release)
* chech_rhub (devel)
* TravisCI (devel and release)

## R CMD check results

There were no ERRORs, or WARNINGs.

There were 1 NOTE:

Maintainer: 'Francisco Javier Luna-Vazquez <frahik@gmail.com>'
New submission

Uses the superseded package: 'doSNOW'

R CMD check succeeded

## Last round comments

Thanksk we see several Rd files do not contain examples. Plerase add 
examples. [DONE]

Is there some reference about the method you can add in the Description 
field in the form Authors (year) <doi:.....>?  [DONE]
Also, please elaborate which kind of models can be estimated. [DONE]

## Special comments
I am using the doSNOW package, to be able to show a progress bar in the advance of the calculations, since it is a very demanding algorithm in time (in large data sets usually takes at least 8 hours). I think it is convenient to show the progress bar when the work is done in parallel to take advantage of all the cores of the computer, as well as to be able to show the user the progress of cross validations.

To be more specific, this package is only used when cross validation is applied to the model, and it has been specified in one of the functions available in the package that more than one kernel of the computer is to be used.
