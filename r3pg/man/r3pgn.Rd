% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/r3pgn.R
\name{r3pgn}
\alias{r3pgn}
\title{Define the function to run 3PGN in R}
\usage{
r3pgn(siteData, climate, thinning = NULL, parameters, outputs = c(1:5,
  10:12, 26:27))
}
\arguments{
\item{siteData}{an initial site information data. \code{Data frame}, with each row representing one site. Please refer to \code{\link{data_site}} for more information}

\item{climate}{a \code{three-dimentional array} containing the weather information. Please refer to \code{\link{data_climate}} for more information}

\item{thinning}{an \code{data frame} containing information regarding thinning. Please refer to \code{\link{data_thinning}} for more information}

\item{parameters}{a vector of parameters values. To see the order of parameters please refer to \code{\link{data_param}}}

\item{outputs}{a numerical vector of output variables IDs. To see the order of variables and meaning refer to \code{\link{data_output}}}
}
\description{
Define the function to run 3PGN in R
}
\details{
This is the model
}
\examples{
firstRun <- r3pgn(siteData = data_site,
                  climate = data_climate,
                  parameters = data_param[,2],
                  outputs = c(1:5, 10:12, 26:27))

print(firstRun)
plot(firstRun)
}
