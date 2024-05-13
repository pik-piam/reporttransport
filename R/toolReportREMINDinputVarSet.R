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
#' @author Johanna Hoppe
#' @import data.table
#' @export

toolReportREMINDinputVarSet <- function(fleetESdemand,
                                        fleetFEdemand,
                                        fleetEnergyIntensity,
                                        fleetCapCosts,
                                        combinedCAPEXandOPEX,
                                        scenSpecPrefTrends,
                                        scenSpecEnIntensity,
                                        initialIncoCosts,
                                        annualMileage,
                                        timeValueCosts,
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
  EJtoTwa <- 31.71e-03                                                                                                                  # nolint: object_name_linter
  browser()
  # Capital costs for the transport system [2005US$/pkm or 2005US$/tkm]
  # p35_esCapCost(tall, all_regi, all_GDPscen, all_demScen, EDGE_scenario_all, all_teEs)                                                # nolint: commented_code_linter
  p35_esCapCost <- copy(fleetCapCosts)                                                                                                  # nolint: object_name_linter
  p35_esCapCost <- p35_esCapCost[period %in% timeResReporting]
  p35_esCapCost <- merge(p35_esCapCost, unique(helpers$mapEdgeToREMIND[, c("all_teEs", "univocalName", "technology")]),                 # nolint: object_name_linter
                         by = c("univocalName", "technology"))

  p35_esCapCost <- p35_esCapCost[, .(value = sum(value)), by = c("region", "period", "all_teEs")]                                       # nolint: object_name_linter

  # Aggregate energy efficiency of transport fuel technologies [trn pkm/Twa or trn tkm/Twa]
  # p35_fe2es(tall, all_regi, all_GDPscen, all_demScen, EDGE_scenario_all, all_teEs)                                                    # nolint: commented_code_linter
  p35_fe2es <- copy(fleetEnergyIntensity)                                                                                               # nolint: object_name_linter
  p35_fe2es <- p35_fe2es[period %in% timeResReporting]
  # convert to TWa/trn (pkm|tkm)
  p35_fe2es[, value := value * EJtoTwa * 1e03]
  # convert to trn (pkm|tkm)/TWa
  p35_fe2es[, value := 1 / value]
  p35_fe2es <- merge(p35_fe2es, unique(helpers$mapEdgeToREMIND[, c("all_teEs", "univocalName", "technology")]),                         # nolint: object_name_linter
                     by = c("univocalName", "technology"))
  # aggregate with fleet ES demand as weight
  fleetESdemand <- copy(fleetESdemand)
  fleetESdemand <- fleetESdemand[period %in% timeResReporting]
  setnames(fleetESdemand, "value", "ESdemand")
  fleetESdemand[, c("unit", "variable") := NULL]
  p35_fe2es <- merge(p35_fe2es, fleetESdemand, by = intersect(names(p35_fe2es), names(fleetESdemand)))                                  # nolint: object_name_linter
  p35_fe2es[, sumES := sum(ESdemand), by = c("region", "period", "all_teEs")]
  p35_fe2es <- p35_fe2es[, .(value = sum(value * ESdemand / sumES)),  by = c("region", "period", "all_teEs")]                           # nolint: object_name_linter

  # Aggregate FE Demand per transport fuel technology [TWa]
  # p35_demByTech(tall, all_regi, all_GDPscen, all_demScen, EDGE_scenario_all, all_enty, all_in, all_teEs)                              # nolint: commented_code_linter
  # convert to TWa
  p35_demByTech <- copy(fleetFEdemand)                                                                                                  # nolint: object_name_linter
  p35_demByTech <- p35_demByTech[period %in% timeResReporting]
  p35_demByTech[, value := value * EJtoTwa]                                                                                             # nolint: object_name_linter
  p35_demByTech <- merge(p35_demByTech,                                                                                                 # nolint: object_name_linter
                         unique(helpers$mapEdgeToREMIND[, c("all_enty", "all_in", "all_teEs",                                           # nolint: object_name_linter
                                                            "univocalName", "technology")]),
                         by = c("univocalName", "technology"))
  p35_demByTech <- p35_demByTech[, .(value = sum(value)), by = c("region", "period", "all_teEs")]                                       # nolint: object_name_linter

  # Input data for edgeTransport iterative that is coupled to REMIND--------------------------------------------
  ####################################################################
  ## Input data for edgeTransport iterative that is coupled to REMIND
  ####################################################################
  # -> can be supplied as it is
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
