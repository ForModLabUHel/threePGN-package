## ----global_options, include=FALSE---------------------------------------
knitr::opts_chunk$set(fig.width=5, fig.height=5, warning=FALSE, cache = F)

## ---- echo = F, message = F----------------------------------------------
set.seed(123)
library(threePGN)

## ------------------------------------------------------------------------
firstRun <- r3pgn(siteData = data_site[1:3,], climate = data_climate, parameters = data_param[,2], outputs = c(1:5,
  10:12, 26:27))

