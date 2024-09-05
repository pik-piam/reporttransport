#'Report to REMIND p35_esCapCost
#'
#' @param fleetCapCosts capital costs on fleet level
#' @param fleetESdemand energy service demand on fleet level
#' @param hybridElecShare share of electric driving for hybrid electric vehicles
#' @param timeResReporting time resolution reporting
#' @param demScen demand scenario
#' @param SSPscen SSP scenario
#' @param transportPolScen transport policy scenario
#' @param helpers list of helpers

#' @returns Capital Costs per CES node in [2005US$/pkm or 2005US$/tkm]
#' @export
#' @author Johanna Hoppe
#' @import data.table
#' @importFrom rmndt approx_dt

reportToREMINDcapitalCosts <- function(fleetCapCosts, fleetESdemand, hybridElecShare, timeResReporting, demScen, SSPscen, transportPolScen, helpers) {

  # Note: In order to account for the influence of active modes, the capital costs per p|tkm are related to the adjusted
  # energy service demand per sector technology that is reported to REMIND and includes the energy service demand of active modes (split and distributed equally to all technologies)
  # f35_esCapCost(tall, all_regi, all_GDPscen, all_demScen, all_EDGE_scenario, all_teEs)                                                                   # nolint: commented_code_linter
  esCapCost <- fleetCapCosts[period %in% timeResReporting]                                                                                                # nolint: object_name_linter
  EDGETesDemand <- fleetESdemand[period %in% timeResReporting][, c("unit", "variable") := NULL]
  setnames(EDGETesDemand, "value", "esDem")
  esCapCost <- merge(esCapCost, EDGETesDemand, by = intersect(names(esCapCost), names(EDGETesDemand)))
  esCapCost[, value := value * esDem * 1e6] #[2005US$]
  # The absolute costs of hybrids should not get lost so they are attributed acording to the hybridElecShare
  esCapCostWoHybrid <- copy(esCapCost)
  hybrids <- esCapCostWoHybrid[technology == "Hybrid electric"]
  hybrids[, value := hybridElecShare * value][, technology := "BEV"]
  esCapCostWoHybrid[technology == "Hybrid electric", value := (1 - hybridElecShare) * value]
  esCapCostWoHybrid[technology == "Hybrid electric", technology := "Liquids"]
  esCapCostWoHybrid <- rbind(esCapCostWoHybrid, hybrids)
  byCols <- names(esCapCostWoHybrid)
  byCols <- byCols[!byCols %in% c("value")]
  esCapCost <- esCapCostWoHybrid[, .(value = sum(value)), by = eval(byCols)]
  # Map on REMIND energy service demand technologies
  capCostMap <- unique(helpers$mapEdgeToREMIND[, c("all_teEs", "univocalName", "technology")])
  # Walk and Cycle don't have capital costs and are not mapped directly on all_teEs
  capCostMap <- capCostMap[!is.na(all_teEs)]
  esCapCost <- merge(esCapCost, capCostMap, by = c("univocalName", "technology"))[, c("univocalName", "technology") := NULL]                                             # nolint: object_name_linter
  esCapCost <- esCapCost[, .(value = sum(value)), by = c("region", "period", "all_teEs")]
  REMINDesDemand <- reportToREMINDesDemand(fleetESdemand, hybridElecShare, timeResReporting, demScen, SSPscen, transportPolScen, helpers)
  REMINDesDemand[, c("GDP_scenario", "DEM_scenario", "EDGE_scenario") := NULL]
  setnames(REMINDesDemand, c("value", "tall", "all_regi"), c("REMINDesDem", "period", "region"))
  f35_esCapCost <- merge(esCapCost, REMINDesDemand, by = intersect(names(esCapCost), names(REMINDesDemand)))
  # Filter out zero demand values and interpolate them from other timesteps as they do not mean zero costs (they are not relevant, as the demand is zero)
  f35_esCapCost <- f35_esCapCost[!REMINDesDem < 1e-7]
  f35_esCapCost[, value := value/REMINDesDem * 1e-9][, REMINDesDem := NULL]#[2005US$/p|tkm]
  f35_esCapCost <- approx_dt(f35_esCapCost, unique(f35_esCapCost$period), "period", "value", extrapolate = TRUE)

  checkForNAsAndDups(f35_esCapCost, "f35_esCapCost", "reportToREMINDcapitalCosts()")
  f35_esCapCost <- prepareForREMIND(f35_esCapCost, demScen, SSPscen, transportPolScen)
  setnames(f35_esCapCost, c("period", "region"), c("tall", "all_regi"))

  return(f35_esCapCost)
}
