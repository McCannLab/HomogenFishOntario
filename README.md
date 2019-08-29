# HomogenFishOntario
:book: Cazelles et al. (2019) DOI:2Badded - research compendium

[![Launch Rstudio Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/McCannLab/HomogenFishOntario/master?urlpath=rstudio)


Analysis pipeline for the analysis carried out in "Homogenization of freshwater lakes: recent compositional shifts in fish communities are explained by gamefish movement and not climate change" by Cazelles et al. (2019) DOI: 2Badded. Note that we were nota allowed to release the exact lake locations and therefore the context map and 2 supplementary figures cannot be reproduced.


# How to reproduce the analysis?

There are two ways:

- use the binder
- install and use the package

the vignettes ["Reproduce the analysis carried out in Cazelles et. al (2019)"]()
guide you through the different functions.



## Using the binder

[holepunch](https://karthik.github.io/holepunch/)

Click on
[![Launch Rstudio Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/McCannLab/HomogenFishOntario/master?urlpath=rstudio)

then run:

`library(HomogenFishOntario)`


## Install and use the package

To install the package using the following lines of code:

```r
install.packages("remotes")
remotes::install_github("McCannLab/HomogenFishOntario")
```

This should also installed all dependencies. In case are encountering
difficulties with packages on GitHub, below are the line to install remote
packages :

```r
remotes::install_github(
  c("KevCaz/ecoocc", "inSileco/graphicsutils", "inSileco/inSilecoMisc")
)
```

Once installed you can simply run:

```r
HomogenFishOntario::pipeline()
```


# An R :package:?

The form of an R package in a GitHub repository is quite convenient:

- to share of the analysis pipeline;
- to check code quality (even without a proper set of test, several aspect are covered when the package is checked);
- to report potential issues.