#'Report to REMIND f35_fe2es
#'
#' @param fleetEnergyIntensity energy intensity on fleet level
#' @param helpers list of helpers
#' @param timeResReporting time resolution reporting
#' @returns Energy efficiency of transport fuel technologies in [trn pkm/Twa or trn tkm/Twa]
#' @export
#' @author Johanna Hoppe
#' @import data.table

reportToREMINDenergyEfficiency <- function(fleetEnergyIntensity, timeResReporting, helpers) {  

  # f35_fe2es(tall, all_regi, all_GDPscen, all_demScen, EDGE_scenario, all_teEs)                                                       # nolint: commented_code_linter
  fleetEnergyIntensity <- copy(fleetEnergyIntensity)                                                                                    # nolint: object_name_linter
  loadFactor <- copy(scenSpecLoadFactor)[, c("variable", "unit") := NULL]
  setnames(loadFactor, "value", "loadFactor")
  f35_fe2es <- merge(fleetEnergyIntensity[period %in% timeResReporting], loadFactor[period %in% timeResReporting],
                     by = intersect(names(fleetEnergyIntensity), names(loadFactor)), all = TRUE)
  f35_fe2es[, value := value / loadFactor][, loadFactor := NULL][, unit := "MJ/(p|t)km"]
  f35_fe2es[, value := 1 / value][, unit := "(p|t)km/MJ"]
  # convert to trn (pkm|tkm)/TWa
  f35_fe2es[, value := value * 10^-12/MJtoTwa]
  weightESdemand <- copy(fleetESdemandWoHybrid)
  setnames(weightESdemand, "value", "ESdemand")
  weightESdemand[, c("unit", "variable") := NULL]
  f35_fe2es <- merge(f35_fe2es, weightESdemand, by = intersect(names(f35_fe2es), names(weightESdemand)))                                  # nolint: object_name_linter
  fe2esMap <- unique(helpers$mapEdgeToREMIND[, c("all_teEs", "univocalName", "technology")])
  fe2esMap <- fe2esMap[!is.na(all_teEs)]
  f35_fe2es <- merge(f35_fe2es, fe2esMap, by = c("univocalName", "technology"), all.y = TRUE)                                                         # nolint: object_name_linter
  f35_fe2es[, sumES := sum(ESdemand), by = c("region", "period", "all_teEs")]
  # Remove weight if whole branch has zero demand to keep data
  f35_fe2es[sumES == 0, ESdemand := 1]
  f35_fe2es[, sumES := sum(ESdemand), by = c("region", "period", "all_teEs")]
  f35_fe2es <- f35_fe2es[, .(value = sum(value * ESdemand / sumES)),  by = c("region", "period", "all_teEs")]                           # nolint: object_name_linter
  checkForNAsDups(f35_fe2es, "f35_fe2es", "reportToREMINDenergyEfficiency()")
  setnames(f35_fe2es, c("period", "region"), c("tall", "all_regi"))  
  
  return(f35_fe2es)
}