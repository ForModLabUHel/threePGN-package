#' Define the function to run 3PGN in R
#'
#' @param nvariables needs to be documented later
#' @details This is the model
#' @example /inst/examples/r3pgnHelp.R
#' @export
#' @useDynLib threePGN
r3pgn <- function(siteData,
                  weather,
                  thinning = NA,
                  parameters,
                  outputs = NA
                  ){

  nMonths <- dim(weather)[1]
  noOfSites <- nrow(siteData)
  nClimID <- dim(weather)[3]
  totThinning <- sum(siteData$nThinning)
  if(all(is.na(thinning)) | totThinning == 0.) thinning = matrix(0,2,6)
  nvariables = outputs
  nvariables <- 8
  y <- array(-999,dim=c(nMonths,nvariables,noOfSites))
  if(!all(siteData[,16]<=noOfSites)) stop('Provided weather data don´t match with site data')
  out <- .Fortran('model',
                  output = as.array(y),
                  nMonths = as.integer(nMonths),
                  noOfSites = as.integer(noOfSites),
                  nClimID = as.integer(nClimID),
                  nvariables = as.integer(nvariables),
                  siteData = as.matrix(siteData),
                  totThinning = as.integer(totThinning),
                  thinning = as.matrix(thinning),
                  weather = as.array(weather),
                  pValues = as.numeric(parameters))

  class(out) = "r3pgOut"
  return(out)
}

#' @export
print.r3pgOut <- function(x, ...){
  print("test")
}

#' @export
plot.r3pgOut <- function(x, ...){
  print("test")
}





