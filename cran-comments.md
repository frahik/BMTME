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

R CMD check succeeded

## Last round comments

Come on ... please provide a Description.  [Done (Sorry for this mistake)]

We also see:

   Uses the superseded package: 'doSNOW'

Can't you use parallel and doParallel, if you really need it. [Done]
