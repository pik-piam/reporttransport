#'Report to REMIND f35_fe2es
#'
#' @param fleetEnergyIntensity energy intensity on fleet level
#' @param scenSpecLoadFactor scenario specific load factor data
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

reportToREMINDenergyEfficiency <- function(fleetEnergyIntensity,
                                           scenSpecLoadFactor,
                                           fleetESdemand,
                                           hybridElecShare,
                                           timeResReporting,
                                           demScen,
                                           SSPscen,
                                           transportPolScen,
                                           helpers) {

  MJtoTwa <- 3.169e-14
  # f35_fe2es(tall, all_regi, all_GDPscen, all_demScen, EDGE_scenario, all_teEs)                                                       # nolint: commented_code_linter
  fleetEnergyIntensity <- copy(fleetEnergyIntensity)                                                                                    # nolint: object_name_linter
  loadFactor <- copy(scenSpecLoadFactor)[, c("variable", "unit") := NULL]
  setnames(loadFactor, "value", "loadFactor")
  f35_fe2es <- merge(fleetEnergyIntensity[period %in% timeResReporting], loadFactor[period %in% timeResReporting],
                     by = intersect(names(fleetEnergyIntensity), names(loadFactor)), all = TRUE)
  # REMIND does not know about active modes, so they are filtered out (they cannot be assigned to a technology in the CES tree)
  f35_fe2es <- f35_fe2es[!univocalName %in% c("Cycle", "Walk")]
  f35_fe2es[, value := value / loadFactor][, loadFactor := NULL][, unit := "MJ/(p|t)km"]
  # In order to deal with hybrids, a fuel column is needed
  f35_fe2es[, techWithoutHybrid := technology]
  f35_fe2es[technology == "Hybrid electric", techWithoutHybrid := "Liquids"]
  hybrids <- f35_fe2es[technology == "Hybrid electric"]
  hybrids[, techWithoutHybrid := "BEV"]
  f35_fe2es <- rbind(f35_fe2es, hybrids)
  #prepare weight
  #use FE demand as weight but keep hybrid electric in order to aggregate accurately
  weight <- copy(f35_fe2es)[, c("unit", "variable") := NULL]
  fleetESdemand <- copy(fleetESdemand)[, c("unit", "variable") := NULL]
  setnames(fleetESdemand, "value", "ESdemand")
  fleetESdemand[, techWithoutHybrid := technology]
  fleetESdemand[technology == "Hybrid electric", techWithoutHybrid := "Liquids"]
  hybrids <- fleetESdemand[technology == "Hybrid electric"]
  hybrids[, techWithoutHybrid := "BEV"]
  fleetESdemand <- rbind(fleetESdemand, hybrids)
  fleetESdemand[technology == "Hybrid electric" & techWithoutHybrid == "Liquids", ESdemand := ESdemand * (1 - hybridElecShare)]
  fleetESdemand[technology == "Hybrid electric" & techWithoutHybrid == "BEV", ESdemand := ESdemand * (hybridElecShare)]
  weight <- merge(weight, fleetESdemand, by = intersect(names(weight), names(fleetESdemand)))
  weight[, FEdemand := value * ESdemand][, c("value", "ESdemand") := NULL]
  byCols <- names(weight)
  byCols <- byCols[!byCols %in% c("value")]

  f35_fe2es[, value := 1 / value][, unit := "(p|t)km/MJ"]
  # convert to trn (pkm|tkm)/TWa
  f35_fe2es[, value := value * 10^-12/MJtoTwa]
  f35_fe2es <- merge(f35_fe2es, weight, by = intersect(names(f35_fe2es), names(weight)))                                  # nolint: object_name_linter
  fe2esMap <- unique(helpers$mapEdgeToREMIND[, c("all_teEs", "univocalName", "technology")])
  fe2esMap <- fe2esMap[!is.na(all_teEs)]
  setnames(fe2esMap, "technology", "techWithoutHybrid")
  f35_fe2es <- merge(f35_fe2es, fe2esMap, by = c("univocalName", "techWithoutHybrid"), all.x = TRUE)                                                         # nolint: object_name_linter
  f35_fe2es[, sumFE := sum(FEdemand), by = c("region", "period", "all_teEs")]
  # Remove weight if whole branch has zero demand to keep data
  f35_fe2es[sumFE == 0, FEdemand := 1]
  f35_fe2es[, sumFE := sum(FEdemand), by = c("region", "period", "all_teEs")]
  f35_fe2es <- f35_fe2es[, .(value = sum(value * FEdemand / sumFE)),  by = c("region", "period", "all_teEs")]                           # nolint: object_name_linter
  checkForNAsAndDups(f35_fe2es, "f35_fe2es", "reportToREMINDenergyEfficiency()")
  f35_fe2es <- prepareForREMIND(f35_fe2es, demScen, SSPscen, transportPolScen)
  setnames(f35_fe2es, c("period", "region"), c("tall", "all_regi"))

  return(f35_fe2es)
}
