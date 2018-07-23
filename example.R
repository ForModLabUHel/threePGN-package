devtools::install_github("checcomi/threePGN-package")

library(threePGN)

# siteData <- siteData[1:2,]
firstRun <- threePGN(nvariables = 8, siteData = siteData,weather=weather,pValues = pars[,2])

plot(firstRun$output[,1,1],firstRun$output[,2,1],ylab="",main="NEP",xlab="Age")
plot(firstRun$output[,1,1],firstRun$output[,3,1],ylab="",main="DBH",xlab="Age")
plot(firstRun$output[,1,1],firstRun$output[,4,1],ylab="",main="Height",xlab="Age")
plot(firstRun$output[,1,1],firstRun$output[,5,1],ylab="",main="Wfoliage",xlab="Age")
plot(firstRun$output[,1,1],firstRun$output[,6,1],ylab="",main="Wroot",xlab="Age")
plot(firstRun$output[,1,1],firstRun$output[,7,1],ylab="",main="Wstem",xlab="Age")
plot(firstRun$output[,1,1],firstRun$output[,8,1],ylab="",main="Volume",xlab="Age")
