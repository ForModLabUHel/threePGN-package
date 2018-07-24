firstRun <- r3pgn(nvariables = 8, siteData = siteData,weather=weather,pValues = pars[,2])
plot(firstRun$output[,1,1],firstRun$output[,2,1],ylab="",main="NEP",xlab="Age")
