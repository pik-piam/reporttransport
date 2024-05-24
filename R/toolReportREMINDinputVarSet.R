#'Report REMIND/iterative EDGE-T input data
#'
#' @param fleetESdemand energy service demand on fleet level
#' @param fleetFEdemand final energy demand on fleet level
#' @param fleetEnergyIntensity energy intensity on fleet level
#' @param fleetCapCosts annualized capital costs on fleet level
#' @param combinedCAPEXandOPEX CAPEX and OPEX on sales level in high temporal resolution
#' @param scenSpecLoadFactor scenario specific load factor data
#' @param scenSpecPrefTrends scenario specific preference trends in high temporal resolution
#' @param scenSpecEnIntensity scenario specific energy intensity in high temporal resolution
#' @param initialIncoCosts initial inconvenience cost
#' @param annualMileage annual mileage in high temporal resolution
#' @param timeValueCosts time value cost equivalent in high temporal resolution
#' @param hybridElecShare share of electric driving for hybrid electric vehicles
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
                                        scenSpecLoadFactor,
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

  # Energy service demand per CES node [trillion pkm/trillion tkm]-----------------------------------------------------
  # p29_trpdemand(tall, all_regi, all_GDPscen, all_demScen, all_EDGE_scenario, all_in)                              # nolint: commented_code_linter
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
  trpdemandMap <- unique(helpers$mapEdgeToREMIND[, c("all_in", "univocalName", "technology")])
  f29_trpdemand <- merge(f29_trpdemand, trpdemandMap, by = c("univocalName", "technology"), all.x = TRUE)# nolint: object_name_linter
  f29_trpdemand <- f29_trpdemand[, .(value = sum(value)), by = c("region", "period", "all_in")]
  checkForNAsDups(f29_trpdemand, "f29_trpdemand", "toolReportREMINDinputDataVarSet()")

  # Capital costs for the transport system [2005US$/pkm or 2005US$/tkm]-------------------------------------------
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
  checkForNAsDups(f35_esCapCost, "f35_esCapCost", "toolReportREMINDinputDataVarSet()")

  #Energy efficiency of transport fuel technologies [trn pkm/Twa or trn tkm/Twa]-----------------------------------
  # f35_fe2es(tall, all_regi, all_GDPscen, all_demScen, EDGE_scenario, all_teEs)                                                    # nolint: commented_code_linter
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
  checkForNAsDups(f35_fe2es, "f35_fe2es", "toolReportREMINDinputDataVarSet()")

  # Final energy demand per transport fuel technology [TWa]-----------------------------------------------------
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
  checkForNAsDups(f35_demByTech, "f35_demByTech", "toolReportREMINDinputDataVarSet()")

  ####################################################################
  ## Input data for edgeTransport iterative that is coupled to REMIND
  ####################################################################
  # CAPEXandNonFuelOPEX
  # Fuel costs are added from the fulldata.gdx of the last REMIND iteration in the iterative script
  CAPEXandNonFuelOPEX <- copy(combinedCAPEXandOPEX)                                                                                     # nolint: object_name_linter
  CAPEXandNonFuelOPEX <- CAPEXandNonFuelOPEX[!variable == "Fuel costs"]                                                                 # nolint: object_name_linter
  checkForNAsDups(CAPEXandNonFuelOPEX, "CAPEXandNonFuelOPEX", "toolReportREMINDinputDataVarSet()")
  # scenSpecPrefTrends
  checkForNAsDups(scenSpecPrefTrends, "scenSpecPrefTrends", "toolReportREMINDinputDataVarSet()")
  # scenSpecLoadFactor
  checkForNAsDups(scenSpecLoadFactor, "scenSpecLoadFactor", "toolReportREMINDinputDataVarSet()")
  # scenSpecEnIntensity
  checkForNAsDups(scenSpecEnIntensity, "scenSpecEnIntensity", "toolReportREMINDinputDataVarSet()")
  # initialIncoCosts
  checkForNAsDups(initialIncoCosts, "initialIncoCosts", "toolReportREMINDinputDataVarSet()")
  # annualMileage
  checkForNAsDups(annualMileage, "annualMileage", "toolReportREMINDinputDataVarSet()")
  # timeValueCosts
  checkForNAsDups(timeValueCosts, "timeValueCosts", "toolReportREMINDinputDataVarSet()")

  ####################################################################
  ## Additional information used from EDGE-T standalone in pik-piam
  ####################################################################
  # The LDV share in the total liquids is used to adjust the IEA values
  # to our differentiation of fedie/fepet in calcIO
  shares_LDV_transport <- copy(fleetFEdemand)[technology %in% c("Liquids") & period %in% timeResReporting]
  shares_LDV_transport <- shares_LDV_transport[, .(value = sum(value)), by = .(region, period, subsectorL1, subsectorL2)]
  weightGdp <- calcOutput("GDP", aggregate = FALSE)[, , paste0("gdp_", SSPscen)] |> time_interpolate(timeResReporting)
  weightGdp <- magpie2dt(weightGdp)
  weightGdp[, variable := NULL]
  setnames(weightGdp, c("value", "iso3c", "year"), c("weight", "iso", "period"))
  edgeTtoIsoMapping <- helpers$regionmappingISOto21to12[, c("countryCode", "regionCode21")]
  setnames(edgeTtoIsoMapping, c("countryCode", "regionCode21"), c("iso", "region"))
  shares_LDV_transport <- disaggregate_dt(shares_LDV_transport, edgeTtoIsoMapping, fewcol = "region",
                  manycol = "iso", datacols = c("period", "subsectorL1", "subsectorL2"), weights = weightGdp)
  shares_LDV_transport[, share_LDV_totliq := value[subsectorL2 == "trn_pass_road_LDV"] / sum(value), by = c("iso", "period")]
  shares_LDV_transport[, share_LDV_totroad := value[subsectorL2 == "trn_pass_road_LDV"] /
                         sum(value[subsectorL1 %in% c("trn_pass_road", "trn_freight_road")]),
                         by = c("iso", "period")]
  shares_LDV_transport[, c("subsectorL1", "subsectorL2", "value") := NULL]
  shares_LDV_transport <- unique(shares_LDV_transport)
  shares_LDV_transport <- melt(shares_LDV_transport, id.vars = c("iso", "period"))
  setnames(shares_LDV_transport, c("iso", "period", "variable"), c("region", "year", "sharetype"))
  shares_LDV_transport[, varname := "shares_LDV_transport"]
  shares_LDV_transport[, GDP_scenario := paste0("gdp_", SSPscen)]
  shares_LDV_transport[, EDGE_scenario := transportPolScen]
  setcolorder(shares_LDV_transport, c("region", "year", "GDP_scenario", "EDGE_scenario", "sharetype", "varname", "value"))

  output <- list(
    f35_esCapCost = f35_esCapCost,
    f35_fe2es = f35_fe2es,
    f35_demByTech = f35_demByTech,
    f29_trpdemand = f29_trpdemand,
    CAPEXandNonFuelOPEX = CAPEXandNonFuelOPEX,
    scenSpecPrefTrends = scenSpecPrefTrends,
    scenSpecLoadFactor = scenSpecLoadFactor,
    scenSpecEnIntensity = scenSpecEnIntensity,
    initialIncoCosts = initialIncoCosts,
    annualMileage = annualMileage,
    timeValueCosts = timeValueCosts
  )

  output <- lapply(output, prepareForREMIND, demScen, SSPscen, transportPolScen)
  output$shares_LDV_transport <- shares_LDV_transport

  return(output)
}
