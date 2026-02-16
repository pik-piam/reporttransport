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
#' @importFrom rmndt disaggregate_dt
#'
#' @returns REMIND/iterative EDGE-T input data
#' @export
#' @author Johanna Hoppe
#' @import data.table

reportREMINDinputVarSet <- function(fleetESdemand,
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

  ## Input data for transport module GAMS code----------------------------------------------------------------------------

  # See needed inputs in REMIND/modules/35_transport/edge_esm/datainput.gms
  # and REMIND/modules/29_CES_parameters/calibratedatainput.gms
  f35_esCapCost <- reportToREMINDcapitalCosts(fleetCapCosts, fleetESdemand, hybridElecShare, timeResReporting, demScen, SSPscen, transportPolScen, helpers)
  f35_fe2es <- reportToREMINDenergyEfficiency(fleetFEdemand, fleetESdemand, hybridElecShare, timeResReporting,
                                              demScen, SSPscen, transportPolScen, helpers)
  f35_demByTech <- reportToREMINDfinalEnergyDemand(fleetFEdemand, timeResReporting, demScen, SSPscen, transportPolScen, helpers)

  inputREMIND <- list(
    f35_esCapCost = f35_esCapCost,
    f35_fe2es = f35_fe2es,
    f35_demByTech = f35_demByTech
  )

  inputREMIND <- lapply(inputREMIND, approx_dt, timeResReporting, "tall", "value", extrapolate = TRUE)

  #p29_trpdemand(tall, all_regi, all_GDPscen, all_demScen, all_EDGE_scenario, all_in)
  fe2es <- copy(inputREMIND$f35_fe2es)
  setnames(fe2es, "value", "fe2es")
  f29_trpdemand <- merge(fe2es, inputREMIND$f35_demByTech, by = intersect(names(fe2es), names(f35_demByTech)))
  weightESdemand <- f29_trpdemand[, .(value = sum(fe2es * value)), by = c("tall", "all_regi", "GDP_scenario", "DEM_scenario", "EDGE_scenario", "all_teEs")]
  f29_trpdemand <- f29_trpdemand[, .(value = sum(fe2es * value)), by = c("tall", "all_regi", "GDP_scenario", "DEM_scenario", "EDGE_scenario", "all_in")]
  #Check for data consistency
  test <- reportToREMINDesDemand(fleetESdemand, hybridElecShare, timeResReporting, demScen, SSPscen, transportPolScen, helpers)
  setcolorder(test, names(weightESdemand))
  setkey(test, all_regi,tall,  all_teEs)
  setkey(weightESdemand, all_regi, tall, all_teEs)
  if (!all.equal(test, weightESdemand[tall %in% unique(test$tall)])) stop("The data set that is reported to REMIND is inconsistent. Please check reportREMINDinputVarSet()")
  inputREMIND[["f29_trpdemand"]] <- f29_trpdemand
  inputREMIND[["weightESdemand"]] <- weightESdemand

  ## Additional information used from EDGE-T standalone in pik-piam---------------------------------------------------------
  shares_LDV_transport <- toolReportsharesLDVtransport(fleetFEdemand, timeResReporting, demScen,
                                                       SSPscen, transportPolScen, helpers)

  inputREMIND <- append(inputREMIND, list(shares_LDV_transport = shares_LDV_transport))

  return(inputREMIND)
}
