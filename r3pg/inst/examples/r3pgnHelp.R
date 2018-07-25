firstRun <- r3pgn(siteData = data_site, climate = data_climate, parameters = data_param[,2],
                  outputs = 1:10)

print(firstRun)
plot(firstRun)
