
# Site data ---------------------------------------------------------------


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


# Thinning data -----------------------------------------------------------


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


# Parrameter data ---------------------------------------------------------

#' The parameter data
#'
#' The order of the parameters corresponds to the \code{Table 4} in \href{http://3pg.sites.olt.ubc.ca/files/2014/04/3PGpjs_UserManual.pdf}{original documentation}, but in the \code{data frame} to numbers in front of the parameter
#'
#' \itemize{
#'   \item parName: parameter name
#' \itemize{
#'   \item Allometric relationships & partitioning
#'    \itemize{
#'      \item 24: \code{pFS2} - Foliage:stem partitioning ratio \eqn{D = 2} cm
#'      \item 16: \code{pFS20} - Foliage:stem partitioning ratio \eqn{D = 20} cm
#'      \item 15: \code{aWS} - Constant in the stem mass v. diam. relationship (alt. stemConst)
#'      \item 1: \code{nWS} - Power in the stem mass v. diam. relationship (alt. stemPower)
#'      \item 22: \code{pRx} - Maximum fraction of NPP to roots
#'      \item 17: \code{pRn} - Minimum fraction of NPP to roots
#'      }
#'   \item Litterfall & root turnover
#'    \itemize{
#'     \item 8: \code{gammaF1} - Maximum litterfall rate
#'     \item 40: \code{gammaF0} - Litterfall rate at \eqn{t = 0}
#'     \item 37: \code{tgammaF} - Age at which litterfall rate has median value
#'     \item 12: \code{gammaR} - Average monthly root turnover rate
#'     \item -: \code{leafgrow} - If deciduous, leaves are produced at end of this month
#'     \item -: \code{leaffall} - If deciduous, leaves all fall at start of this month
#'    }
#'   \item Temperature modifier \code{fT}
#'    \itemize{
#'     \item 6: \code{Tmin} - Minimum temperature for growth
#'     \item 13: \code{Topt} - Optimum temperature for growth
#'     \item 31: \code{Tmax} - Maximum temperature for growth
#'    }
#'   \item Frost modifier \code{fFrost}
#'    \itemize{
#'     \item 26: \code{kF} - Number of days production lost for each frost day
#'    }
#'   \item Fertility modifiers
#'    \itemize{
#'     \item 29: \code{m0} - Value of \code{m} when \eqn{FR = 0}
#'     \item 9: \code{fN0} - Value of \code{fNutr} when \eqn{FR = 0}
#'     \item -: \code{fNn} - Power of \eqn{(1-FR)} in \code{fN}
#'    }
#'   \item Age modifier
#'    \itemize{
#'     \item 38: \code{MaxAge} - Maximum stand age used in age modifier
#'     \item 36: \code{nAge} - Power of relative age in function for \code{fAge}
#'     \item 39: \code{rAge} - Relative age to give \eqn{fAge = 0.5}
#'    }
#'   \item Stem mortality and self-thinning
#'    \itemize{
#'     \item -: \code{gammaN0} -
#'    }
#'   \item Conductance
#'    \itemize{
#'     \item -: \code{MinCond} -
#'     \item 14: \code{MaxCond} - Maximum canopy conductance
#'     \item 20: \code{LAIgcx} - LAI for maximum canopy conductance
#'     \item 23: \code{CoeffCond} - Defines stomatal response to VPD
#'     \item 35: \code{BLcond} - Canopy boundary layer conductance
#'    }
#'   \item Specific leaf area
#'    \itemize{
#'     \item 34: \code{SLA0} - Specific leaf area at age 0
#'     \item 27: \code{SLA1} - Specific leaf area for mature leaves
#'     \item 30: \code{tSLA} - Age at which \eqn{SLA = (SLA0+SLA1)/2}
#'    }
#'   \item Rainfall interception
#'    \itemize{
#'     \item 32: \code{MaxIntcptn} - Maximum proportion of rainfall evaporated from canopy
#'     \item 46: \code{LAImaxIntcptn} - LAI for maximum rainfall interception
#'    }
#'   \item Light interception, production and respiration
#'    \itemize{
#'     \item 18: \code{k} - Extinction coefficient for absorption of PAR by canopy
#'     \item 21: \code{fullCanAge} - Age at canopy closure
#'     \item 5: \code{alpha} - Maximum canopy quantum efficiency
#'     \item 10: \code{rg} - Ratio NPP/GPP (alt. Y)
#'    }
#'   \item Branch and bark fraction \code{(fracBB)}
#'    \itemize{
#'     \item 33: \code{fracBB0} - Branch and bark fraction at age 0
#'     \item 19: \code{fracBB1} - Branch and bark fraction for mature stands
#'     \item 28: \code{tBB} - Age at which \eqn{fracBB = (fracBB0+fracBB1)/2}
#'    }
#'   \item Baasic density
#'    \itemize{
#'     \item 11: \code{rho1} - Ratio of basic density of young to old trees
#'    }
#'   \item Stem height allometric relationship
#'    \itemize{
#'     \item 2: \code{aH} - Constant in the stem height relationship
#'     \item 3: \code{bW} -
#'    }
#'   \item Soil module part
#'    \itemize{
#'     \item 4: \code{klmax} - Decomposition rate constant for the `young and labile` pool per month
#'     \item 7: \code{krmax} - Decomposition rate constant for the `young and refractory` pool per month
#'     \item 41: \code{komax} - Decomposition rate constant for the `old` pool per month
#'     \item 25: \code{hc} - Humification coefficient
#'     \item 42: \code{dmC} -
#'     \item 43: \code{Yl_C_i} - Ratio of labile litter input?
#'     \item 44: \code{Yr_C_i} - Ratio of refractory litter input?
#'     \item 45: \code{O_C_i} - Ratio of humification?
#'    }
#'   \item NOT DOCUMENTED
#'    \itemize{
#'     \item 47: \code{FR_esp} -
#'     \item 48: \code{FR_ferc} -
#'     \item 49: \code{FR_ferf} -
#'     \item 50: \code{FR_feri} -
#'     \item 51: \code{FR_ferif} -
#'    }
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



# Output variables --------------------------------------------------------

#' Description of output variables
#'
#' Names and description of 3PGP output variables as described in \code{Table 4.} in \href{http://3pg.sites.olt.ubc.ca/files/2014/04/3PGpjs_UserManual.pdf}{original documentation}
#'
#' \itemize{
#'   \item Stand attributes
#'    \itemize{
#'      \item 1: \code{StandAge} - Stand age (\eqn{yr})
#'      \item 2: \code{StemNo} - Stand stocking (\eqn{trees ha^-1})
#'      \item 3: \code{BasArea} - Stand basal area (\eqn{m^2 ha^-1})
#'      \item 4: \code{StandVol} - Stand volume excluding branch & bark (\eqn{m^3 ha^-1})
#'      \item 5: \code{avDBH} - Stand volume excluding branch & bark (\eqn{cm})
#'      \item 6: \code{MAI} - Mean annual volume increment (\eqn{m^3 ha^-1 yr^-1})
#'      }
#'   \item Canopy attributes
#'    \itemize{
#'     \item 7: \code{SLA} - Specific leaf area (\eqn{m^2 kg^-1})
#'     \item 8: \code{CanCover} - Fraction of ground area covered by canopy (\eqn{-})
#'     \item 9: \code{LAI} - Canopy LAI (\eqn{m^2 m^-2})
#'    }
#'   \item Biomass pools
#'    \itemize{
#'     \item 10: \code{WF} - Foliage biomass (\eqn{t_DM ha^-1})
#'     \item 11: \code{WR} - Root biomass (\eqn{t_DM ha^-1})
#'     \item 12: \code{WS} - Stem biomass, including branches and bark (\eqn{t_DM ha^-1})
#'     \item 13: \code{WL} - Accumulated litter fall (also TotalLitter) (\eqn{t_DM ha^-1})
#'     \item 14: \code{TotalW} - Total biomass (\eqn{t_DM ha^-1})
#'     \item 15: \code{AvStemMass} - Mean stem biomass per tree (\eqn{t_DM ha^-1})
#'     \item 16: \code{fracBB} - Fraction of stem biomass as branch and bark (\eqn{t_DM ha^-1})
#'    }
#'   \item Growth modifiers
#'    \itemize{
#'     \item 17: \code{fAge} - Age dependent modifier (\eqn{-})
#'     \item 18: \code{fVPD} - VPD dependent modifier (\eqn{-})
#'     \item 19: \code{fT} - Temperature dependent modifier (\eqn{-})
#'     \item 20: \code{fCalpha} - CO2 dependent modifier for quantum efficiency (\eqn{-})
#'     \item 21: \code{fCg} - CO2 dependent modifier canopy conductance (\eqn{-})
#'     \item 22: \code{fFrost} - Frost dependent modifier (\eqn{-})
#'     \item 23: \code{fSW} - Soil water dependent modifier (\eqn{-})
#'     \item 24: \code{fNutr} - Nutrition dependent modifier (\eqn{-})
#'     \item 25: \code{PhysMod} - Physiological modifier of canopy conductance (\eqn{-})
#'    }
#'   \item Biomass production and allocation
#'    \itemize{
#'     \item 26: \code{GPP} - Gross primary production in current period (\eqn{t_DM ha^-1})
#'     \item 27: \code{NPP} - Net primary production in current period (\eqn{t_DM ha^-1})
#'     \item 28: \code{RadInt} - Total solar radiation intercepted by canopy (\eqn{MJ m^-2 month^-1})
#'     \item 29: \code{alphaC} - Canopy quantum efficiency after modifiers (\eqn{mol mol^-1})
#'     \item 30: \code{epsilon} - Light utilisation efficiency based on total biomass (\eqn{g_DM MJ^-1})
#'     \item 31: \code{CVI} - Stem volume increment in current period (\eqn{m^3 ha^-1})
#'     \item 32: \code{m} - FR modifier of root biomass allocation (\eqn{-})
#'     \item 33: \code{pR} - Fraction of NPP allocated to roots (\eqn{-})
#'     \item 34: \code{pS} - Fraction of NPP allocated to stems (\eqn{-})
#'     \item 35: \code{pF} - Fraction of NPP allocated to foliage (\eqn{-})
#'     \item 36: \code{pFS} - Ratio of foliage to stem biomass allocation (\eqn{-})
#'     \item 37: \code{gammaF} - Current leaf litterfall rate (\eqn{month^-1})
#'    }
#'   \item Litterfall & root turnover
#'    \itemize{
#'     \item 38: \code{lossWF} - Litterfall turnover (\eqn{-})
#'     \item 39: \code{lossWR} - Toot turnover (\eqn{-})
#'    }
#'   \item Stem mortality
#'    \itemize{
#'     \item 40: \code{wSmax} - Max. mean tree stem mass at current stocking (\eqn{kg tree^-1})
#'     \item 41: \code{gammaN} - Density independent mortality rate (\eqn{month^-1})
#'     \item 42: \code{Mortality} - Number of stems dying in current period (\eqn{trees ha^-1})
#'    }
#'   \item Water use
#'    \itemize{
#'     \item 43: \code{supIrrig} - “Supplemental” irrigation to maintain Os >= Osm (\eqn{mm})
#'     \item 44: \code{RunOff} - Water loss due to run-off or drainage (\eqn{mm})
#'     \item 45: \code{fRainInt} - Fraction of rainfall intercepted by canopy (\eqn{-})
#'     \item 46: \code{RainInt} - Rainfall intercepted by canopy in current period (\eqn{mm})
#'     \item 47: \code{CanCond} - Canopy conductance (\eqn{m s^-1})
#'     \item 48: \code{WUE} - Water use efficiency (\eqn{gDM mm^-1})
#'     \item 49: \code{EvapTransp} - Evapotranspiration rate in current period (\eqn{mm})
#'     \item 50: \code{Transp} - Monthly transpiration rate in current period (\eqn{mm})
#'     \item 51: \code{ASW} - Available soil water (\eqn{mm})
#'    }
#'   \item Soil model
#'    \itemize{
#'     \item 52: \code{NEP} -  (\eqn{-})
#'     \item 53: \code{Rhet} -  (\eqn{-})
#'     \item 54: \code{Yr_C} - Refractory pool for carbon (\eqn{tC ha^−1})
#'     \item 55: \code{Yl_C} - Labile pool for carbon (\eqn{tC ha^−1})
#'     \item 56: \code{O_C} - Humified poll for carbon (\eqn{tC ha^−1})
#'     \item 57: \code{Yr_N} - Refractory pool for nitrogen (\eqn{tC ha^−1})
#'     \item 58: \code{Yl_N} - Labile pool for nitrogen (\eqn{tC ha^−1})
#'     \item 59: \code{O_N} - Humified poll for nitrogen (\eqn{tC ha^−1})
#'    }
#' }
#'
#' @docType data
#' @keywords outputs
#' @name data_output_var
#' @format A vector of outpus. Please reffer to output ID (numerical value)
NULL
