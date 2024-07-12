#'Report to REMIND p35_esCapCost
#'
#' @param fleetCapCosts capital costs on fleet level
#' @param fleetESdemand energy service demand on fleet level
#' @param timeResReporting time resolution reporting
#' @param demScen demand scenario
#' @param SSPscen SSP scenario
#' @param transportPolScen transport policy scenario
#' @param helpers list of helpers

#' @returns Capital Costs per CES node in [2005US$/pkm or 2005US$/tkm]
#' @export
#' @author Johanna Hoppe
#' @import data.table

reportToREMINDcapitalCosts <- function(fleetCapCosts, fleetESdemand, timeResReporting, demScen, SSPscen, transportPolScen, helpers) {

  # f35_esCapCost(tall, all_regi, all_GDPscen, all_demScen, all_EDGE_scenario, all_teEs)                                                # nolint: commented_code_linter
  f35_esCapCost <- copy(fleetCapCosts)                                                                                                  # nolint: object_name_linter
  f35_esCapCost <- f35_esCapCost[period %in% timeResReporting]
  capCostMap <- unique(helpers$mapEdgeToREMIND[, c("all_teEs", "univocalName", "technology")])
  # Walk and Cycle are not mapped on all_teEs
  capCostMap <- capCostMap[!is.na(all_teEs)]
  f35_esCapCost <- merge(f35_esCapCost, capCostMap, by = c("univocalName", "technology"))                                               # nolint: object_name_linter
  # aggregate with fleet ES demand as weight
  weightESdemand <- copy(fleetESdemand)
  setnames(weightESdemand, "value", "ESdemand")
  weightESdemand[, c("unit", "variable") := NULL]
  f35_esCapCost <- merge(f35_esCapCost, weightESdemand, by = intersect(names(f35_esCapCost), names(weightESdemand)))
  f35_esCapCost[, sumES := sum(ESdemand), by = c("region", "period", "all_teEs")]
  # Remove weight if whole branch has zero demand to keep data
  f35_esCapCost[sumES == 0, ESdemand := 1]
  f35_esCapCost[, sumES := sum(ESdemand), by = c("region", "period", "all_teEs")]
  f35_esCapCost <- f35_esCapCost[, .(value = sum(value * ESdemand / sumES)),  by = c("region", "period", "all_teEs")]                                     # nolint: object_name_linter
  checkForNAsAndDups(f35_esCapCost, "f35_esCapCost", "reportToREMINDcapitalCosts()")
  f35_esCapCost <- prepareForREMIND(f35_esCapCost, demScen, SSPscen, transportPolScen)
  setnames(f35_esCapCost, c("period", "region"), c("tall", "all_regi"))

  return(f35_esCapCost)
}
