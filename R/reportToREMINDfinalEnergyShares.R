#' Report to REMIND final energy shares f35_shFeCes
#'
#' @param fleetFEdemand Final energy demand on fleet level
#' @param timeResReporting time resolution reporting
#' @param demScen demand scenario
#' @param SSPscen SSP scenario
#' @param transportPolScen transport policy scenario
#' @param helpers list of helpers
#' @returns Final energy shares of transport fuel technologies in [-]
#'
#' @import data.table
#' @export
#'
reportToREMINDfinalEnergyShares <- function(fleetFEdemand, timeResReporting, demScen, SSPscen, transportPolScen, helpers) {

  FEdemByTech <- reportToREMINDfinalEnergyDemand(fleetFEdemand, timeResReporting, demScen, SSPscen, transportPolScen, helpers)
  FEshares <- FEdemByTech[, value := value/sum(value), by = c("tall", "all_regi", "all_in")]
  ## 7 decimals the lowest accepted value
  FEshares[, value := round(value, digits = 7)]
  FEshares[, value := ifelse(value == 0, 1e-7, value)]
  FEshares[, sumvalue := sum(value), by = c("tall", "all_regi", "all_in")]
  FEshares[, maxtech := ifelse(value == max(value), TRUE, FALSE), by =c("tall", "all_regi", "all_in")]

  ## attribute the variation to the maximum share value
  FEshares[sumvalue != 1 & maxtech == TRUE, value := value + (1 - sumvalue), by = c("tall", "all_regi")]
  ## remove temporary columns
  FEshares[, c("sumvalue", "maxtech") := NULL]#

  return(FEshares)
}
