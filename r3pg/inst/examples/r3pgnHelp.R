firstRun <- r3pgn(siteData = siteData,weather=weather,parameters = pars[,2])
print(firstRun)
plot(firstRun)

plot(firstRun$output[,1,1],firstRun$output[,2,1],ylab="",main="NEP",xlab="Age")
