firstRun <- r3pgn(siteData = data_site,
                  climate = data_climate,
                  parameters = data_param[,2],
                  outputs = c(1:5, 10:12, 26:27))

print(firstRun)
plot(firstRun)
