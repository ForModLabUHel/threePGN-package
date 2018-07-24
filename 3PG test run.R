
# 0. Libraries ------------------------------------------------------------
library(threePGN)
library(dplyr)
library(ggplot2)


# 1. Inicial run of the model ---------------------------------------------
# siteData <- siteData[1:2,]
firstRun <- threePGN(nvariables = 8, siteData = siteData, weather=weather, pValues = pars[,2])


# 3. Visualize the output -------------------------------------------------
varnames <- c('standAge','NEP','dbh','H','WF','WR','WS','StandVol')

# tranform output to the long format
matrix(firstRun$output, ncol = firstRun$noOfSites, byrow = F) %>%
  data.frame() %>%
  mutate(variable = rep(varnames, each = firstRun$nMonths),
         id = rep(1:firstRun$nMonths, times = length(varnames))) %>%
  tidyr::gather(site, value, -variable, -id) %>%
  tidyr::spread(variable, value) %>%
  select(-id) %>%
  tidyr::gather(variable, value, -site, -standAge) ->
  firstRun_long

# visualize the run
firstRun_long %>%
  ggplot()+
  geom_line(aes(standAge, value, colour = site))+
  facet_wrap(~variable, scales = 'free_y') +
  theme_bw()
