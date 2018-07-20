#' Define the function to run 3PGN in R
threePGN <- function(nvariables,siteData,
                       thinning = NA,weather,pValues){

  nMonths <- dim(weather)[1]
  noOfSites <- nrow(siteData)
  totThinning <- sum(siteData$nThinning)
  if(all(is.na(thinning)) | totThinning == 0.) thinning = matrix(0,2,6)
  nvariables <- 8
  y <- array(-999,dim=c(nMonths,nvariables,noOfSites))
  out <- .Fortran('model',
                  output = as.array(y),
                  nMonths = as.integer(nMonths),
                  noOfSites = as.integer(noOfSites),
                  nvariables = as.integer(nvariables),
                  siteData = as.matrix(siteData),
                  totThinning = as.integer(totThinning),
                  thinning = as.matrix(thinning),
                  weather = as.array(weather),
                  pValues = as.numeric(pValues))

  return(out)
}
