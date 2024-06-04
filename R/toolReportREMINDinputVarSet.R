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
                                        helpers,
                                        files) {

  DEM_scenario <- GDP_scenario <- EDGE_scenario <- value <- sumES <- variable <- univocalName <- ESdemand <- NULL


  ## Input data for transport module GAMS code----------------------------------------------------------------------------

  # See needed inputs in REMIND/modules/35_transport/edge_esm/datainput.gms
  # and REMIND/modules/29_CES_parameters/calibratedatainput.gms
  f29_trpdemand <- reportToREMINDtrpdemand(fleetESdemand, helpers)
  f35_esCapCost <- reportToREMINDcapitalCosts(fleetCapCosts, helpers)
  f35_fe2es <- reportToREMINDenergyEfficiency(fleetEnergyIntensity, timeResReporting, helpers)
  f35_demByTech <- reportToREMINDfinalEnergyDemand(fleetFEdemand, timeResReporting, helpers)

  ## Input data for edgeTransport iterative that is coupled to REMIND--------------------------------------------------------

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

  ## Additional information used from EDGE-T standalone in pik-piam---------------------------------------------------------

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
