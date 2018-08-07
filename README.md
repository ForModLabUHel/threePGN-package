[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[//]: # [![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[//]: # [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/BayesianTools)](https://cran.r-project.org/package=BayesianTools)

# threePGN

An R package to run the [3PGN](http://3pg.forestry.ubc.ca) model written in fortran.

### Installing the development version from GitHub

If you want to install our development version from GitHub, use 

```{r}
devtools::install_github(repo = "checcomi/threePGN-package/r3pg", dependencies = T, build_vignettes = T)
```

https://travis-ci.org/checcomi/threePGN-package?utm_source=email&utm_medium=notification


New developments will be done in extra branches and will be tested before merging in the developtment branch, so the developmet version should usually be usable (consider it in a beta stage), while feature branches should be considered alpha. 

Windows users: the package contains c++ code, so if you compile yourself, you need [RTools](https://cran.r-project.org/bin/windows/Rtools/) installed. 

### Working with the package

To get an overview about its functionality once the package is installed, run

```{r}
library(threePGN)
?threePGN
vignette("threePGN", package="threePGN")
```

As for every R package, you can get the suggested citation via

```{r}
citation("threePGN")
```
