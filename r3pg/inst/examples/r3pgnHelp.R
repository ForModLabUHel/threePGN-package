firstRun <- r3pgn(siteData = data_site, climate = data_climate, parameters = data_param[,2])

print(firstRun)
plot(firstRun)
