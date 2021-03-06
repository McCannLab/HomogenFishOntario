# HomogenFishOntario
:book: [Cazelles et al. (2019) DOI:10.1111/gcb.14829](https://onlinelibrary.wiley.com/doi/abs/10.1111/gcb.14829) - research compendium

[![Launch Rstudio Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/McCannLab/HomogenFishOntario/master?urlpath=rstudio)
[![Build Status](https://travis-ci.org/McCannLab/HomogenFishOntario.svg?branch=master)](https://travis-ci.org/McCannLab/HomogenFishOntario)
[![Build status](https://ci.appveyor.com/api/projects/status/iw2lkapvla1flr8v/branch/master?svg=true)](https://ci.appveyor.com/project/KevCaz/homogenfishontario/branch/master)
[![DOI](https://zenodo.org/badge/205234842.svg)](https://zenodo.org/badge/latestdoi/205234842)






Analysis pipeline used in "Homogenization of freshwater lakes: recent compositional shifts in fish communities are explained by gamefish movement and not climate change" by [Cazelles et al. (2019) DOI:10.1111/gcb.14829](https://onlinelibrary.wiley.com/doi/abs/10.1111/gcb.14829).

:warning: We are not allowed to share the exact lake locations, therefore, the context map and 3 supplementary figures cannot be reproduced with this research compendium. The data included in this package are released under [CC BY NC](https://creativecommons.org/licenses/by-nc/2.0/).


# How to reproduce the analysis?

Two options:

1. use the binder;
2. install and use this package.

Moreover the vignettes ["Reproduce the analysis carried out in Cazelles et. al (2019)"](http://mccannlab.ca/HomogenFishOntario/articles/homogenOntario.html) guides you through the different functions included in the package.



## Using the binder

You can use [Binder](https://mybinder.readthedocs.io/en/latest/index.html#) by
clicking on the following badge: [![Launch Rstudio
Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/McCannLab/HomogenFishOntario/master?urlpath=rstudio)
which sets up an R environment (takes 15-20min :hourglass:) and then a launches
an [RStudio](https://www.rstudio.com/) instance. Once done, one can simply run:

```R
# this first step is quick cause all packages required are already installed!
library(devtools)
load_all()
pipeline()
```

NB: the binder was created with the great R package [holepunch](https://karthik.github.io/holepunch/)!



## Install and use this R package

First, install the package:

```r
install.packages("remotes")
remotes::install_github("McCannLab/HomogenFishOntario")
```

This should install all dependencies, but in case you are encountering
difficulties with the R packages available on GitHub, use the following:

```r
remotes::install_github(
  c("KevCaz/ecoocc", "inSileco/graphicsutils", "inSileco/inSilecoMisc")
)
```

Once installed, you can simply reproduce the analysis like so:

```r
HomogenFishOntario::pipeline()
```


# An R :package:?

The form of an R package in a GitHub repository is quite convenient:

- to share of the analysis pipeline;
- to check code quality (even without a proper set of test, several aspects are covered when the package is checked);
- to report potential issues.