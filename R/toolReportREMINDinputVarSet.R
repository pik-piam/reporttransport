#'Report REMIND/iterative EDGE-T input data
#'
#' @param fleetESdemand energy service demand on fleet level
#' @param fleetFEdemand final energy demand on fleet level
#' @param fleetEnergyIntensity energy intensity on fleet level
#' @param fleetCapCosts annualized capital costs on fleet level
#' @param combinedCAPEXandOPEX CAPEX and OPEX on sales level in high temporal resolution
#' @param scenSpecPrefTrends scenario specific preference trends in high temporal resolution
#' @param scenSpecEnIntensity scenario specific energy intensity in high temporal resolution
#' @param initialIncoCosts initial inconvenience cost
#' @param annualMileage annual mileage in high temporal resolution
#' @param timeValueCosts time value cost equivalent in high temporal resolution
#' @param demScen demand scenario
#' @param SSPscen SSP scenario
#' @param transportPolScen transport policy scenario
#' @param timeResReporting time resolution reporting
#' @param helpers list with helpers
#'
#' @returns REMIND/iterative EDGE-T input data
#' @export
#' @author Johanna Hoppe
#' @import data.table

toolReportREMINDinputVarSet <- function(fleetESdemand,
                                        fleetFEdemand,
                                        fleetEnergyIntensity,
                                        loadFactor,
                                        fleetCapCosts,
                                        combinedCAPEXandOPEX,
                                        scenSpecPrefTrends,
                                        scenSpecEnIntensity,
                                        initialIncoCosts,
                                        annualMileage,
                                        timeValueCosts,
                                        hybridElecShare,
                                        demScen,
                                        SSPscen,
                                        transportPolScen,
                                        timeResReporting,
                                        helpers) {

  DEM_scenario <- GDP_scenario <- EDGE_scenario <- value <- sumES <- variable <- univocalName <- ESdemand <- NULL

  prepareForREMIND <- function(dt, demScen, SSPscen, transportPolScen) {                                                                # nolint: object_name_linter
    cols <- names(copy(dt))
    cols <- cols[!cols %in% c("region", "period", "value")]
    dt[, DEM_scenario := paste0("gdp_", demScen)]
    dt[, GDP_scenario := paste0("gdp_", SSPscen)]
    dt[, EDGE_scenario := transportPolScen]
    setcolorder(dt, c("region", "period", "GDP_scenario", "EDGE_scenario", "DEM_scenario", cols, "value"))
    return(dt)
  }

  #############################################################
  ## Input data for transport module GAMS code
  #############################################################
  # See needed inputs in REMIND/modules/35_transport/edge_esm/datainput.gms
  # Unit conversion
  MJtoTwa <- 3.169e-14                                                                                                                # nolint: object_name_linter

  # Capital costs for the transport system [2005US$/pkm or 2005US$/tkm]-------------------------------------------
  # p35_esCapCost(tall, all_regi, all_GDPscen, all_demScen, EDGE_scenario_all, all_teEs)                                                # nolint: commented_code_linter
  p35_esCapCost <- copy(fleetCapCosts)                                                                                                  # nolint: object_name_linter
  p35_esCapCost <- p35_esCapCost[period %in% timeResReporting]
  capCostMap <- unique(helpers$mapEdgeToREMIND[, c("all_teEs", "univocalName", "technology")])
  # Walk and Cycle are not mapped on all_teEs
  capCostMap <- capCostMap[!is.na(all_teEs)]
  p35_esCapCost <- merge(p35_esCapCost, capCostMap, by = c("univocalName", "technology"))                                               # nolint: object_name_linter
  p35_esCapCost <- p35_esCapCost[, .(value = sum(value)), by = c("region", "period", "all_teEs")]                                       # nolint: object_name_linter

  #Energy efficiency of transport fuel technologies [trn pkm/Twa or trn tkm/Twa]-----------------------------------
  # p35_fe2es(tall, all_regi, all_GDPscen, all_demScen, EDGE_scenario_all, all_teEs)                                                    # nolint: commented_code_linter
  fleetEnergyIntensity <- copy(fleetEnergyIntensity)                                                                                    # nolint: object_name_linter
  loadFactor <- copy(loadFactor)[, c("variable", "unit") := NULL]
  setnames(loadFactor, "value", "loadFactor")
  p35_fe2es <- merge(fleetEnergyIntensity[period %in% timeResReporting], loadFactor[period %in% timeResReporting],
                                by = intersect(names(fleetEnergyIntensity), names(loadFactor)), all = TRUE)
  p35_fe2es[, value := value / loadFactor][, loadFactor := NULL][, unit := "MJ/(p|t)km"]
  p35_fe2es[, value := 1 / value][, unit := "(p|t)km/MJ"]
  # convert to trn (pkm|tkm)/TWa
  p35_fe2es[, value := value * 10^-12/MJtoTwa]
  # aggregate with fleet ES demand as weight
  fleetESdemand <- copy(fleetESdemand)
  fleetESdemand <- fleetESdemand[period %in% timeResReporting]
  setnames(fleetESdemand, "value", "ESdemand")
  fleetESdemand[, c("unit", "variable") := NULL]
  p35_fe2es <- merge(p35_fe2es, fleetESdemand, by = intersect(names(p35_fe2es), names(fleetESdemand)))                                  # nolint: object_name_linter
  #Split hybrids
  hybrids <- p35_fe2es[technology == "Hybrid electric"]
  hybrids[, ESdemand := hybridElecShare * value][, technology := "BEV"]
  p35_fe2es[technology == "Hybrid electric", ESdemand := (1 - hybridElecShare) * value]
  p35_fe2es[technology == "Hybrid electric", technology := "Liquids"]
  p35_fe2es <- rbind(p35_fe2es, hybrids)
  fe2esMap <- unique(helpers$mapEdgeToREMIND[, c("all_teEs", "univocalName", "technology")])
  fe2esMap <- fe2esMap[!is.na(all_teEs)]
  p35_fe2es <- merge(p35_fe2es, fe2esMap, by = c("univocalName", "technology"), all.y = TRUE)                                                         # nolint: object_name_linter
  p35_fe2es[, sumES := sum(ESdemand), by = c("region", "period", "all_teEs")]
  # Remove weight if whole branch has zero demand to keep data
  p35_fe2es[sumES == 0, ESdemand := 1]
  p35_fe2es[, sumES := sum(ESdemand), by = c("region", "period", "all_teEs")]
  p35_fe2es <- p35_fe2es[, .(value = sum(value * ESdemand / sumES)),  by = c("region", "period", "all_teEs")]                           # nolint: object_name_linter

  # Final energy demand per transport fuel technology [TWa]-----------------------------------------------------
  # p35_demByTech(tall, all_regi, all_GDPscen, all_demScen, EDGE_scenario_all, all_enty, all_in, all_teEs)                              # nolint: commented_code_linter
  # convert to TWa
  p35_demByTech <- copy(fleetFEdemand)                                                                                                  # nolint: object_name_linter
  p35_demByTech <- p35_demByTech[!univocalName %in% c("Cycle", "Walk")]
  p35_demByTech <- p35_demByTech[period %in% timeResReporting]
  p35_demByTech[, value := value * MJtoTwa * 10^12]
  demByTechMap <- unique(helpers$mapEdgeToREMIND[, c("all_enty", "all_in", "all_teEs", "univocalName", "technology")])
  demByTechMap <- demByTechMap[!is.na(all_teEs)]
  demByTechMap[technology %in% c("BEV", "Electric"), technology := "Electricity"]
  demByTechMap[technology == "FCEV", technology := "Hydrogen"]
  p35_demByTech <- merge(p35_demByTech, demByTechMap, by = c("univocalName", "technology"), all.x = TRUE)# nolint: object_name_linter
  p35_demByTech <- p35_demByTech[, .(value = sum(value)), by = c("region", "period", "all_enty", "all_in", "all_teEs")]

  ####################################################################
  ## Input data for edgeTransport iterative that is coupled to REMIND
  ####################################################################
  # CAPEXandNonFuelOPEX
  # Fuel costs are added from the fulldata.gdx of the last REMIND iteration in the iterative script
  CAPEXandNonFuelOPEX <- copy(combinedCAPEXandOPEX)                                                                                     # nolint: object_name_linter
  CAPEXandNonFuelOPEX <- CAPEXandNonFuelOPEX[!variable == "Fuel costs"]                                                                 # nolint: object_name_linter
  # scenSpecPrefTrends
  # scenSpecLoadFactor
  # scenSpecEnIntensity
  # initialIncoCosts
  # annualMileage
  # timeValueCosts

  output <- list(
    p35_esCapCost = p35_esCapCost,
    p35_fe2es = p35_fe2es,
    p35_demByTech = p35_demByTech,
    CAPEXandNonFuelOPEX = CAPEXandNonFuelOPEX,
    scenSpecPrefTrends = scenSpecPrefTrends,
    scenSpecEnIntensity = scenSpecEnIntensity,
    initialIncoCosts = initialIncoCosts,
    annualMileage = annualMileage,
    timeValueCosts = timeValueCosts
  )

  output <- lapply(output, prepareForREMIND, demScen, SSPscen, transportPolScen)

  return(output)
}
