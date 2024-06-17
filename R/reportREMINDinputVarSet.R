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

  timeResReporting <- c(seq(1900,1985,5),
    seq(1990, 2060, by = 5),
    seq(2070, 2110, by = 10),
    2130, 2150)

  ## Input data for transport module GAMS code----------------------------------------------------------------------------

  # See needed inputs in REMIND/modules/35_transport/edge_esm/datainput.gms
  # and REMIND/modules/29_CES_parameters/calibratedatainput.gms
  f29_trpdemand <- reportToREMINDtrpdemand(fleetESdemand, hybridElecShare, timeResReporting, demScen, SSPscen, transportPolScen, helpers)
  f35_esCapCost <- reportToREMINDcapitalCosts(fleetCapCosts, fleetESdemand, timeResReporting, demScen, SSPscen, transportPolScen, helpers)
  f35_fe2es <- reportToREMINDenergyEfficiency(fleetEnergyIntensity, scenSpecLoadFactor, fleetESdemand, hybridElecShare, timeResReporting,
                                              demScen, SSPscen, transportPolScen, helpers)
  f35_demByTech <- reportToREMINDfinalEnergyDemand(fleetFEdemand, timeResReporting, demScen, SSPscen, transportPolScen, helpers)

  inputREMIND <- list(
    f35_esCapCost = f35_esCapCost,
    f35_fe2es = f35_fe2es,
    f35_demByTech = f35_demByTech,
    f29_trpdemand = f29_trpdemand
  )

  inputREMIND <- lapply(inputREMIND, approx_dt, timeResReporting, "tall", "value", extrapolate = TRUE)

  ## Input data for edgeTransport iterative that is coupled to REMIND--------------------------------------------------------

  # CAPEXandNonFuelOPEX
  # Fuel costs are added from the fulldata.gdx of the last REMIND iteration in the iterative script
  CAPEXandNonFuelOPEX <- copy(combinedCAPEXandOPEX)                                                                                     # nolint: object_name_linter
  CAPEXandNonFuelOPEX <- CAPEXandNonFuelOPEX[!variable == "Fuel costs"]                                                                 # nolint: object_name_linter
  checkForNAsDups(CAPEXandNonFuelOPEX, "CAPEXandNonFuelOPEX", "reportREMINDinputDataVarSet()")
  # scenSpecPrefTrends
  checkForNAsDups(scenSpecPrefTrends, "scenSpecPrefTrends", "reportREMINDinputDataVarSet()")
  # scenSpecLoadFactor
  checkForNAsDups(scenSpecLoadFactor, "scenSpecLoadFactor", "reportREMINDinputDataVarSet()")
  # scenSpecEnIntensity
  checkForNAsDups(scenSpecEnIntensity, "scenSpecEnIntensity", "reportREMINDinputDataVarSet()")
  # initialIncoCosts
  checkForNAsDups(initialIncoCosts, "initialIncoCosts", "reportREMINDinputDataVarSet()")
  # annualMileage
  checkForNAsDups(annualMileage, "annualMileage", "reportREMINDinputDataVarSet()")
  # timeValueCosts
  checkForNAsDups(timeValueCosts, "timeValueCosts", "reportREMINDinputDataVarSet()")

  ## Additional information used from EDGE-T standalone in pik-piam---------------------------------------------------------
  shares_LDV_transport <- toolReportsharesLDVtransport(fleetFEdemand, timeResReporting, demScen,
                                                       SSPscen, transportPolScen, helpers)

  inputIterative <- list(
    CAPEXandNonFuelOPEX = CAPEXandNonFuelOPEX,
    scenSpecPrefTrends = scenSpecPrefTrends,
    scenSpecLoadFactor = scenSpecLoadFactor,
    scenSpecEnIntensity = scenSpecEnIntensity,
    initialIncoCosts = initialIncoCosts,
    annualMileage = annualMileage,
    timeValueCosts = timeValueCosts
  )

  inputIterative <- lapply(inputIterative, prepareForREMIND, demScen, SSPscen, transportPolScen)
  input <- append(inputREMIND, inputIterative)
  input <- append(input, list(shares_LDV_transport = shares_LDV_transport))

  return(input)
}
