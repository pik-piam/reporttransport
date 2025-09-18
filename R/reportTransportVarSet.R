#'Report detailed transport variable set
#'
#' @param data List that contains the model results to report the detailed transport variable set
#' @param baseVarSet Basic output variable set
#'
#' @returns Detailed transport output variable set
#' @author Johanna Hoppe
#' @import data.table
#' @export

reportTransportVarSet <- function(data, baseVarSet) {

  fuel <- variable <- value <- constrYear <- period <- technology <- . <- ESdemand <- unit <- NULL

  # Report liquids and gases split----------------------------------------------------------------------
  varsFEcomposition <- baseVarSet$ext$fleetFEdemand[technology %in% c("Liquids", "Gases")]
  mixedCarrierSplit <- reportLiquidsAndGasesComposition(dtFE = varsFEcomposition,
                                                        gdxPath = data$gdxPath,
                                                        helpers = data$helpers)

  fleetFEdemandsplittedCarriers <- copy(baseVarSet$ext$fleetFEdemand[!technology %in% c("Liquids", "Gases")])
  fleetFEdemandsplittedCarriers[, fuel := NA]
  fleetFEdemandsplittedCarriers <- rbind(fleetFEdemandsplittedCarriers, mixedCarrierSplit$splittedCarriers)

  # Report emissions-------------------------------------------------------------------------------------
  cols <- names(fleetFEdemandsplittedCarriers)
  byCols <- cols[!cols %in% c("fuel", "value")]
  # For the tailpipe emissions the different fuel production routes are not taken into account ´
  # "It counts what comes out of the exhaust"
  FEtailpipe <- fleetFEdemandsplittedCarriers[technology %in% c("Liquids", "Gases")]                      # nolint: object_name_linter
  FEtailpipe <- FEtailpipe[, .(value = sum(value)), by = eval(byCols)]                                    # nolint: object_name_linter
  # For the demand emissions only the fuel from the fossil production route is taken into account ´
  FEfossil <- fleetFEdemandsplittedCarriers[technology %in% c("Liquids", "Gases") & fuel == "Fossil"]     # nolint: object_name_linter
  FEfossil <- FEfossil[, .(value = sum(value)), by = eval(byCols)]                                        # nolint: object_name_linter
  fleetEmissionsTailpipe <- reportEmissions(dtFE = FEtailpipe,
                                                gdxPath = data$gdxPath,
                                                prefix = "Tailpipe",
                                                helpers = data$helpers)
  fleetEmissionsDemand <- reportEmissions(dtFE = FEfossil,
                                              gdxPath = data$gdxPath,
                                              prefix = "Demand",
                                              helpers = data$helpers)
  fleetEmissions <- rbind(fleetEmissionsTailpipe, fleetEmissionsDemand)

  # Report vehicle sales-----------------------------------------------------------------------------------
  sales <- copy(data$fleetSizeAndComposition$fleetVehNumbersConstrYears[period == constrYear])
  sales[, variable := "Sales"][, constrYear := NULL]
  sales <- approx_dt(sales, unique(fleetEmissions$period), "period", "value", extrapolate = TRUE)

  # Report yearly investment costs-------------------------------------------------------------------------
  fleetES <- copy(baseVarSet$ext$fleetESdemand)
  fleetES[, c("variable", "unit") := NULL]
  setnames(fleetES, "value", "ESdemand")
  fleetYrlCosts <- merge(baseVarSet$int$fleetCost, fleetES,
                         by = intersect(names(baseVarSet$int$fleetCost), names(fleetES)))
  fleetYrlCosts[, value := value * ESdemand][, unit := "billion US$2017/yr"][, ESdemand := NULL]
  fleetYrlCosts[variable == "Capital costs", variable := "Annualized fleet investments"]

  fleetYrlCosts[variable == "Operating costs (total non-fuel)",
                variable := "Operating costs fleet (total non-fuel)"]
  fleetYrlCosts[variable == "Fuel costs",
                variable := "Operating costs fleet (fuel)"]
  aggregatedOperatingCosts <- fleetYrlCosts[variable %in% c("Operating costs fleet (total non-fuel)",
                                                            "Operating costs fleet (fuel)")]
  byCols <- names(aggregatedOperatingCosts)
  byCols <- byCols[!byCols %in% c("value", "variable")]
  aggregatedOperatingCosts <- aggregatedOperatingCosts[, .(value = sum(value)), by = byCols]
  aggregatedOperatingCosts[, variable := "Operating costs fleet"]
  fleetYrlCosts <- rbind(fleetYrlCosts, aggregatedOperatingCosts)

  # Report upfront capital cost for vehicle sales
  if (!is.null(data$upfrontCAPEXtrackedFleet)) {
    upfrontCAPEXtrackedFleet <- data$upfrontCAPEXtrackedFleet
    upfrontCAPEXtrackedFleet <- upfrontCAPEXtrackedFleet[, .(value = sum(value)), by = c("region", "period", "univocalName", "technology", "unit")]
    upfrontCAPEXtrackedFleet[, variable := "Purchase Price"]
    upfrontCAPEXtrackedFleet <- merge(upfrontCAPEXtrackedFleet, data$helpers$decisionTree,
                                         by = intersect(names(data$upfrontCAPEXtrackedFleet),
                                                        names(data$helpers$decisionTree)))
  }

  # Split extensive and intensive variables ---------------------------------------------------
  outputVarsExt <- list(FEsplittedCarriers = fleetFEdemandsplittedCarriers,
                        fleetESdemand = baseVarSet$ext$fleetESdemand,
                        fleetEmissions = fleetEmissions,
                        sales = sales,
                        stock = data$fleetSizeAndComposition$fleetVehNumbers,
                        fleetYrlCosts = fleetYrlCosts)
  outputVarsInt <- list(fleetEnergyIntensity = baseVarSet$int$fleetEnergyIntensity)
  if (!is.null(data$upfrontCAPEXtrackedFleet)) outputVarsInt <- c(outputVarsInt, list(upfrontCAPEXtrackedFleet = upfrontCAPEXtrackedFleet))
  outputVars <- list(ext = outputVarsExt,
                     int = outputVarsInt)

  return(outputVars)
}
