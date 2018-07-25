#' The site data
#'
#' A dataset containing the site information about site.
#'
#' \itemize{
#'   \item siteId: unique id of the single site. Can be character
#'   \item latitude: site latitude (-ve for the S hemisphere)
#'   \item nTrees: number of trees
#'   \item soilClass. soil class in accordance with Table 2 of \href{http://3pg.sites.olt.ubc.ca/files/2014/04/3PGpjs_UserManual.pdf}{model documentation}
#'   \item iAsw: ??
#'   \item minAsw: the minimum allowed available soil water (mm). If actual available soil water falls below a non-zero minimum, it is assumed the shortfall is made up by an external source
#'   \item maxAsw: maximum available water stored in the soil (mm)
#'   \item poolFraction:
#'   \item startAge: inicial age (\eqn{years}) of the site (Shall it be year?)
#'   \item endAge: the age (\eqn{years}) when tree is removed
#'   \item startMonth: inicial month of the site
#'   \item wfI: the individual stand foliage biomass pools (\eqn{t/ha}), i.e. at \code{startAge}, \code{startMonth}
#'   \item wrI: the individual stand root biomass pools (\eqn{t/ha}), i.e. at \code{startAge}, \code{startMonth}
#'   \item wsI: the individual stand stem (including branches and bark) biomass pools (\eqn{t/ha}), i.e. at \code{startAge}, \code{startMonth}
#'   \item nThinnings: identifies table of thinning events
#'   \item climId: an id of the climate data set \code{\link{data_climate}}
#'   \item fr:
#' }
#'
#' @docType data
#' @keywords datasets
#' @name data_site
#' @usage data(data_site)
#' @format A \code{data frame} with 5 rows and 17 variables
NULL

#' The thinning data
#'
#' A dataset containing the thinning information about site.
#'
#' \itemize{
#'   \item ageThinning: age (year and month as decimals) at which thinning is performed
#'   \item nTrees: number of trees remained
#'   \item f:
#'   \item r:
#'   \item s:
#'   \item siteId: the minimum allowed available soil water (mm). If actual available soil water falls below a non-zero minimum, it is assumed the shortfall is made up by an external source
#' }
#'
#' @docType data
#' @keywords datasets
#' @name data_thinning
#' @usage data(data_thinning)
#' @format A \code{data frame} with 80 rows and 5 variables
NULL

#' The weather data
#'
#' A dataset containing the weather information.
#'
#' \itemize{
#'   \item Tx: monthly mean daily maximum temperature
#'   \item Tn: monthly mean daily minimum temperature
#'   \item rain: monthly rainfall
#'   \item solarRad: monthly mean daily solar radiation
#'   \item frostDays: frost days per month
#' }
#'
#' @docType data
#' @keywords datasets
#' @name data_climate
#' @usage data(data_climate)
#' @format A \code{three-dimentional array} with two \code{matrices} of dimentions 156 rows and 5 variables. The order of the collumns corresponds to the below listed items
NULL


#' The parameter data
#'
#' A dataset containing the parameters data, including medium, maximum and minimum.
#'
#' \itemize{
#'   \item parName: parameter name
#'    \itemize{
#'      \item StemPower:
#'      \item aH: cnstant in the stem height relationship
#'      \item bW:
#'      \item klmax:
#'      \item alpha:
#'      \item Tmin: minimum temperature for growth
#'      \item krmax:
#'      \item gammaF1: maximum litterfall rate
#'      \item fN0: value of \code{fNutr} when \eqn{FR = 0}
#'      \item rg:
#'      \item rho1:
#'      \item gammaR: average monthly root turnover rate
#'      \item Topt: optimum temperature for growth
#'      \item MaxCond: maximum canopy conductance
#'      \item StemConst:
#'      \item pFS20: foliage:stem partitioning ratio D=20 cm
#'      \item pRn: minimum fraction of NPP to roots
#'      \item k: extinction coefficient for absorption of PAR by canopy
#'      \item fracBB1: branch and bark fraction for mature stands
#'      \item LAIgcx: LAI for maximum canopy conductance
#'      \item fullCanAge: age at canopy closure
#'      \item pRx: maximum fraction of NPP to roots
#'      \item CoeffCond: defines stomatal response to VPD
#'      \item pFS2: foliage:stem partitioning ratio D=2 cm
#'      \item hc:
#'      \item kF: number of days production lost for each frost day
#'      \item SLA1: specific leaf area for mature leaves
#'      \item tBB: age at which \eqn{fracBB = (fracBB0+fracBB1)/2}
#'      \item m0: value of \code{m} when \eqn{FR = 0}
#'      \item tSLA: age at which \eqn{SLA = (SLA0+SLA1)/2}
#'      \item Tmax: maximum temperature for growth
#'      \item MaxIntcptn: maximum proportion of rainfall evaporated from canopy
#'      \item fracBB0: branch and bark fraction at age 0
#'      \item SLA0: specific leaf area at age 0
#'      \item BLcond: canopy boundary layer conductance
#'      \item nAge: power of relative age in function for \code{fAge}
#'      \item tgammaF: age at which litterfall rate has median value
#'      \item MaxAge: maximum stand age used in age modifier
#'      \item rAge: relative age to give \eqn{fAge = 0.5}
#'      \item gammaF0: litterfall rate at \eqn{t = 0}
#'      \item komax:
#'      \item dmC:
#'      \item Yl_C_i:
#'      \item Yr_C_i:
#'      \item O_C_i:
#'      \item LAImaxIntcptn: LAI for maximum rainfall interception
#'      \item FR_esp:
#'      \item FR_ferc:
#'      \item FR_ferf:
#'      \item FR_feri:
#'      \item FR_ferif:
#'    }
#'   \item mode: medium value
#'   \item min: minimum value
#'   \item max: maximum value
#' }
#'
#' @docType data
#' @keywords datasets
#' @name data_param
#' @usage data(data_param)
#' @format A \code{data frame} with 51 rows and 4 variables
NULL


#' The parameter data ALTERNATIVE
#'
#' The structure of parameters as in \href{http://3pg.sites.olt.ubc.ca/files/2014/04/3PGpjs_UserManual.pdf}{original documentation}
#'
#' \itemize{
#'   \item Allometric relationships & partitioning
#'    \itemize{
#'      \item pFS2: foliage:stem partitioning ratio D=2 cm
#'      \item pFS20: foliage:stem partitioning ratio D=20 cm
#'      \item aWS: constant in the stem mass v. diam. relationship
#'      \item nWS: power in the stem mass v. diam. relationship
#'      \item pRx: maximum fraction of NPP to roots
#'      \item pRn: minimum fraction of NPP to roots
#'      }
#'   \item Litterfall & root turnover
#'    \itemize{
#'     \item gammaF1: maximum litterfall rate
#'     \item gammaF0: litterfall rate at t = 0
#'     \item tgammaF: age at which litterfall rate has median value
#'     \item gammaR: average monthly root turnover rate
#'     \item leafgrow: if deciduous, leaves are produced at end of this month
#'     \item leaffall: if deciduous, leaves all fall at start of this month
#'    }
#' }
#'
#' @docType data
#' @keywords datasets
#' @name data_param_alternative
#' @format A \code{data frame} with 51 rows and 4 variables
NULL
