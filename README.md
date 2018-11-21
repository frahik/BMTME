
<p align="center">

<a href="https://github.com/frahik/BMTME">
<img src="Logo.png" alt="BMTME Logo"/> </a>

<h3 align="center">

Bayesian Multi-Trait Multi-Environment | Development version 0.0.24

</h4>

<p align="center">

<a href="https://www.tidyverse.org/lifecycle/#experimental">
<img src="https://img.shields.io/badge/lifecycle-experimental-orange.svg" alt="Experimental">
</a> <a href="https://www.gnu.org/licenses/lgpl-3.0">
<img src="https://img.shields.io/badge/License-LGPL%20v3-blue.svg" alt="LGPL, Version 3.0">
</a> <a href="http://www.repostatus.org/#wip">
<img src="http://www.repostatus.org/badges/latest/wip.svg" alt="Status of the Repo:  Initial development is in progress, but there has not yet been a stable, usable release suitable for the public">
</a> <a href="">
<img src="http://cranlogs.r-pkg.org/badges/BMTME" alt="Dowloads from the CRAN">
</a> <a href="https://cran.r-project.org/package=BMTME">
<img src="http://www.r-pkg.org/badges/version-ago/BMTME" alt="CRAN">
</a>

</p>

<h4 align="center">

\[Last README update: 2018-11-21\]

</h4>

</p>

-----

# Table of contents

  - [NEWS](#news)
      - [Instructions](#instructions)
          - [Installation](#install)
          - [Dataset](#data)
      - [How to cite this package](#cite)
      - [Contributions](#contributions)
      - [Authors](#authors)

<h2 id="news">

News of this version (0.0.24)

</h2>

Revision 24

  - Removed stratified random partitions.
  - Fixed barplot for BMORS\_Env.
  - Good practice with \<-
  - complete F or T, logical arguments.
  - Bug fixed: changed 1:length to seq\_len to avoid bugs.
  - Incude ORCID.

Revision 23

  - Include more tests.
  - Fixed print message of `BMORS()` and `BMORS_Env()` functions.
  - Fixed parallel work of `BMORS()` function.

Revision 22

  - Important updates to the documentation.
  - Fixed MAC-OS 10.11 El Capitan, installation (issue \#1).

Revision 21

  - `BMTERS()` function now is `BMORS()` function.
  - `BMTERS_Env()` function now is `BMORS_Env()` function.

Revision 20

  - Update Windows c++ support.
  - Fixed issues \#1 (Installation in Windows) and \#2 (Typos). Thanks
    to @volpatoo and @j450h1 by reporting it.

Revision 19

  - Update plot and boxplot functions

Revision 18

  - New `predictor_Sec_complete` parameter in `BMTMERS_Env()` function.

Revision 17

  - New `predictor_Sec_complete` parameter in `BMTMERS()` function.

Revision 16

  - The MSEP was changed to MAAPE for the error estimations of the
    predictions.
  - Minor fixes in the documentation.
  - Now the boxplots can be ordered by the MAAPE.
  - fixed the predicted output.

Revision 15

  - Implement parallel mode in the BMTMERS function
  - Implement validation to `parallelCores` parameter in the functions
    that could use it.
  - export `n_cores` used to fit the models that could use
    `parallelCores` parameter.
  - fixed class of `BME` function.

Revision 14

  - Update Unix support

Revision 13

  - Initial development is in progress, but there has not yet been a
    stable, usable release suitable for the public; this is a
    pre-release, be careful.

<h2 id="instructions">

Instructions for proper implementation

</h2>

<h3 id="install">

Installation

</h3>

To complete installation of dev version of BMTME from GitHub, you have
to install [Rtools
Software](https://cran.r-project.org/bin/windows/Rtools/) and a few
packages first.

``` r
install.packages('devtools')
devtools::install_github('frahik/BMTME')
```

<h3 id="data">

Datasets

</h3>

The package include 6 sample
datasets

| Name         | Lines | Environment | Traits | Total of observations | ME models | MTME models |
| ------------ | ----- | ----------- | ------ | --------------------- | --------- | ----------- |
| MaizeToy     | 30    | 3           | 3      | 270                   | \*        | \*          |
| WheatIranian | 30    | 2           | 2      | 120                   | \*        | \*          |
| WheatJapa30  | 30    | 1           | 6      | 180                   | \*        |             |
| WheatJapa50  | 50    | 1           | 3      | 150                   | \*        |             |
| WheatMadaToy | 50    | 1           | 6      | 300                   | \*        |             |
| WheatToy     | 30    | 3           | 2      | 180                   | \*        | \*          |

To load one dataset, use the function `data(datasetName)`

<h2 id="cite">

How to cite this package

</h2>

First option, by the article paper

(Coming soon)

Second option, by the manual package

(Coming soon)

<h2 id="contributions">

Contributions

</h2>

If you have any suggestions or feedback, I would love to hear about it.
Feel free to report new issues in [this
link](https://github.com/frahik/BMTME/issues/new), also if you want to
request a feature/report a bug, or make a pull request if you can
contribute.

<h2 id="authors">

Authors

</h2>

  - Francisco Javier Luna-Vázquez (Author, Maintainer)
  - Osval Antonio Montesinos-López (Author)
  - Abelardo Montesinos-López (Author)
  - José Crossa (Author)
  - Fernando Tolero (Author)
