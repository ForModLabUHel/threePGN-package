
# Prepare the internal datasets -------------------------------------------

#' This is a temporal script to prepare the internal datasets. I suggest
#' devide the input data into three different data sets, so they can be easily documented
#'
#' The file will be removed once agreed on the data structure

library(dplyr)

oldwd <- getwd()

setwd('r3pg')
load('inst/temp/testinputs.rdata')

#' Suggestion is to use the same naming pattern along the script, e.g.
#' variables name always start with lower case and each new word start with upper case
#'

# Site Data ---------------------------------------------------------------

data_site <- siteData %>%
  select(
    siteId = site_number,
    latitude = lat,
    nTrees = Num_tree,
    soilClass = `SoilClass.1.4.S.SL.CL.C.`,
    iAsw = iASW,
    minAsw = minASW,
    maxAsw = maxASW,
    poolFraction = poolFractn,
    startAge = startAge,
    endAge = endAge,
    startMonth = startMonth,
    wfI = WF_i,
    wrI = WR_i,
    wsI = WS_i,
    nThinnings = nThinning,
    climId = climID, #' can we use also `character` here, or only `real` can be used?
    fr = FR)

devtools::use_data(data_site, overwrite = T)



# Climate data ------------------------------------------------------------
data_climate <- weather
devtools::use_data(data_climate, overwrite = T)



# Parameters --------------------------------------------------------------
data_param <- pars %>%
  select(parName = ParName, mode, min, max)

devtools::use_data(data_param, overwrite = T)


# thinning ----------------------------------------------------------------
data_thinning <- thinning %>%
  select(
    ageThinning,
    nTrees = Nstock,
    f = `X.F`,
    r = `X.R`,
    s = `X.S`,
    siteId = SiteN)

devtools::use_data(data_thinning, overwrite = T)
# Reset the directory -----------------------------------------------------
setwd(oldwd)

