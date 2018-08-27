firstRun <- r3pgn(siteData = data_site,
                  climate = data_climate,
                  parameters = data_param[,2],
                  outputs = c(1:5, 10:12, 26:27))

firstRunMix <- r3pgnMix(siteData = data_siteMix,
                  climate = data_climate[,,1],
                  parameters = data_paramMix,
                  outputs = c(1:5, 10:12, 26:27))


plot(firstRunMix$output[,3,1])
points(firstRunMix$output[,3,2],pch=20,col=2)
points(firstRunMix$output[,3,3],pch=20,col=3)


print(firstRun)
plot(firstRun)
