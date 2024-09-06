#'Report to REMIND f35_fe2es
#'
#' @param fleetFEdemand final energy demand on fleet level
#' @param fleetESdemand energy service demand on fleet level
#' @param hybridElecShare share of electric driving for hybrid electric vehicles
#' @param timeResReporting time resolution reporting
#' @param demScen demand scenario
#' @param SSPscen SSP scenario
#' @param transportPolScen transport policy scenario
#' @param helpers list of helpers
#' @returns Energy efficiency of transport fuel technologies in [trn pkm/Twa or trn tkm/Twa]
#' @export
#' @author Johanna Hoppe
#' @import data.table

reportToREMINDenergyEfficiency <- function(fleetFEdemand,
                                           fleetESdemand,
                                           hybridElecShare,
                                           timeResReporting,
                                           demScen,
                                           SSPscen,
                                           transportPolScen,
                                           helpers) {
  MJtoTwa <- 3.169e-14

  ES <- reportToREMINDesDemand(fleetESdemand, hybridElecShare, timeResReporting, demScen, SSPscen, transportPolScen, helpers)
  FE <- reportToREMINDfinalEnergyDemand(fleetFEdemand, timeResReporting, demScen, SSPscen, transportPolScen, helpers)[, c("all_enty", "all_in") := NULL]
  setnames(FE, "value", "FE")
  setnames(ES, "value", "ES")
  f35_fe2es <- merge(ES, FE, by = intersect(names(FE), (names(ES))))
  # Insert small number instead of zero
  f35_fe2es <- f35_fe2es[!ES < 1e-7]
  f35_fe2es[, value := ES/FE][, c("ES", "FE") := NULL]
  f35_fe2es <- approx_dt(f35_fe2es, unique(f35_fe2es$tall), "tall", "value", extrapolate = TRUE)
  setcolorder(f35_fe2es, c("all_regi", "tall", "GDP_scenario", "DEM_scenario", "EDGE_scenario", "all_teEs", "value"))

  return(f35_fe2es)
}
