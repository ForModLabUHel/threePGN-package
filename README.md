# threePGN-package
3PGN model in fortran called from R


### Installing the development version from GitHub

If you want to install our development version from GitHub, use 

```{r}
devtools::install_github(repo = "checcomi/threePGN-package/r3pg", dependencies = T, build_vignettes = T)
```

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
