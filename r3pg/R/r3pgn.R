#' Define the function to run 3PGN in R
#'
#' @param siteData an initial site information data. \code{Data frame}, with each row representing one site. Please refer to \code{\link{data_site}} for more information
#' @param climate a \code{three-dimentional array} containing the weather information. Please refer to \code{\link{data_climate}} for more information
#' @param thinning an \code{data frame} containing information regarding thinning. Please refer to \code{\link{data_thinning}} for more information
#' @param parameters a vector of parameters values. To see the order of parameters please refer to \code{\link{data_param}}
#'
#' @details This is the model
#'
#' @example /inst/examples/r3pgnHelp.R
#'
#' @export
#' @useDynLib threePGN
#'
r3pgn <- function(siteData,
                  climate,
                  thinning = NULL,
                  parameters,
                  outputs = NULL
                  ){

  nMonths <- dim(climate)[1]
  noOfSites <- nrow(siteData)
  nClimID <- dim(climate)[3]
  totThinning <- sum(siteData$nThinning)

  if( all( is.null( thinning ) ) | totThinning == 0.) thinning = matrix(0,2,6)

  nvariables = outputs
  nvariables <- 8

  y <- array(-999,dim=c(nMonths,nvariables,noOfSites))

  if( length(unique(siteData[['climId']])) != nClimID ) stop('Provided climate data don´t match with site data')

  out <- .Fortran('model',
                  output = as.array(y),
                  nMonths = as.integer(nMonths),
                  noOfSites = as.integer(noOfSites),
                  nClimID = as.integer(nClimID),
                  nvariables = as.integer(nvariables),
                  siteData = as.matrix(siteData),
                  totThinning = as.integer(totThinning),
                  thinning = as.matrix(thinning),
                  weather = as.array(climate),
                  pValues = as.numeric(parameters))

  class(out) = "r3pgOut"
  return(out)
}
