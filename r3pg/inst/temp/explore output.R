
set.seed(123)
library(threePGN)
library(dplyr)
library(ggplot2)



firstRun <- r3pgn(siteData = data_site[1:3,], climate = data_climate, parameters = data_param[,2], outputs = c(1:5,
  10:12, 26:27))


varnames <- firstRun$vars

# tranform output to the long format
matrix(firstRun$output, ncol = firstRun$noOfSites, byrow = F) %>%
  data.frame() %>%
  mutate(variable = rep(varnames, each = firstRun$nMonths),
    id = rep(1:firstRun$nMonths, times = length(varnames))) %>%
  tidyr::gather(site, value, -variable, -id) %>%
  tidyr::spread(variable, value) %>%
  select(-id) %>%
  tidyr::gather(variable, value, -site, -StandAge) ->
  firstRun_long

# visualize the run
firstRun_long %>%
  ggplot()+
  geom_line(aes(StandAge, value, colour = site))+
  facet_wrap(~variable, scales = 'free_y') +
  theme_bw()


