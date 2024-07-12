#'Report to REMIND p29_trpdemand
#'
#' @param fleetESdemand energy service demand on fleet level
#' @param hybridElecShare share of electric driving for hybrid electric vehicles
#' @param timeResReporting time resolution reporting
#' @param demScen demand scenario
#' @param SSPscen SSP scenario
#' @param transportPolScen transport policy scenario
#' @param helpers list of helpers
#' @returns Energy service demand per CES node in [trillion pkm/trillion tkm]
#' @export
#' @author Johanna Hoppe
#' @import data.table

reportToREMINDtrpdemand <- function(fleetESdemand, hybridElecShare, timeResReporting, demScen, SSPscen, transportPolScen, helpers) {

  # p29_trpdemand(tall, all_regi, all_GDPscen, all_demScen, all_EDGE_scenario, all_in)                                                                         # nolint: commented_code_linter
  # convert to TWa
  fleetESdemand <- copy(fleetESdemand)
  fleetESdemand <- fleetESdemand[period %in% timeResReporting]                                                                                                 # nolint: object_name_linter
  #split hybrid energy service demand
  fleetESdemandWoHybrid <- copy(fleetESdemand)
  hybrids <- fleetESdemandWoHybrid[technology == "Hybrid electric"]
  hybrids[, value := hybridElecShare * value][, technology := "BEV"]
  fleetESdemandWoHybrid[technology == "Hybrid electric", value := (1 - hybridElecShare) * value]
  fleetESdemandWoHybrid[technology == "Hybrid electric", technology := "Liquids"]
  fleetESdemandWoHybrid <- rbind(fleetESdemandWoHybrid, hybrids)
  byCols <- names(fleetESdemandWoHybrid)
  byCols <- byCols[!byCols %in% c("value")]
  fleetESdemandWoHybrid <- fleetESdemandWoHybrid[, .(value = sum(value)), by = eval(byCols)]
  # This needs to be done in order to be consistent with f35_demByTech and f35_fe2es
  f29_trpdemand <- fleetESdemandWoHybrid[!univocalName %in% c("Cycle", "Walk")]
  # convert billion pkm|tkm to trillion pkm|tkm
  f29_trpdemand[, value := value * 1e-3]
  trpdemandMap <- unique(helpers$mapEdgeToREMIND[!is.na(univocalName), c("all_in", "univocalName", "technology")])
  f29_trpdemand <- merge(f29_trpdemand, trpdemandMap, by = c("univocalName", "technology"), all.x = TRUE)                                                      # nolint: object_name_linter
  f29_trpdemand <- f29_trpdemand[, .(value = sum(value)), by = c("region", "period", "all_in")]
  checkForNAsAndDups(f29_trpdemand, "f29_trpdemand", "reportToREMINDtrpdemand()")
  f29_trpdemand <- prepareForREMIND(f29_trpdemand, demScen, SSPscen, transportPolScen)
  setnames(f29_trpdemand, c("period", "region"), c("tall", "all_regi"))

  return(f29_trpdemand)
}
