#'Report to REMIND f35_demByTech
#'
#' @param fleetFEdemand final energy demand on fleet level
#' @param timeResReporting time resolution reporting
#' @param demScen demand scenario
#' @param SSPscen SSP scenario
#' @param transportPolScen transport policy scenario
#' @param helpers list of helpers
#' @returns Final energy demand on CES level per transport fuel technology [TWa]
#' @export
#' @author Johanna Hoppe
#' @import data.table

reportToREMINDfinalEnergyDemand <- function(fleetFEdemand, timeResReporting, demScen, SSPscen, transportPolScen, helpers) {

  MJtoTwa <- 3.169e-14

  # f35_demByTech(tall, all_regi, all_GDPscen, all_demScen, all_EDGE_scenario, all_enty, all_in, all_teEs)                              # nolint: commented_code_linter
  f35_demByTech <- copy(fleetFEdemand)                                                                                                  # nolint: object_name_linter
  f35_demByTech <- f35_demByTech[!univocalName %in% c("Cycle", "Walk")]
  f35_demByTech <- f35_demByTech[period %in% timeResReporting]
  # convert to TWa
  f35_demByTech[, value := value * MJtoTwa * 10^12]
  demByTechMap <- unique(helpers$mapEdgeToREMIND[, c("all_enty", "all_in", "all_teEs", "univocalName", "technology")])
  demByTechMap <- demByTechMap[!is.na(all_teEs)]
  demByTechMap[technology %in% c("BEV", "Electric"), technology := "Electricity"]
  demByTechMap[technology == "FCEV", technology := "Hydrogen"]
  f35_demByTech <- merge(f35_demByTech, demByTechMap, by = c("univocalName", "technology"), all.x = TRUE)# nolint: object_name_linter
  f35_demByTech <- f35_demByTech[, .(value = sum(value)), by = c("region", "period", "all_enty", "all_in", "all_teEs")]
  checkForNAsAndDups(f35_demByTech, "f35_demByTech", "reportToREMINDfinalEnergyDemand()")
  f35_demByTech <- prepareForREMIND(f35_demByTech, demScen, SSPscen, transportPolScen)
  setnames(f35_demByTech, c("period", "region"), c("tall", "all_regi"))

  return(f35_demByTech)
}
