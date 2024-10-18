#'Report extended detailed transport variable set
#'
#' @param data List that contains the model results to report the extended detailed transport variable set
#' @param baseVarSet List that contains the basic variables
#' @param timeResReporting Time resolution for variable reporting
#'
#' @returns Extended detailed transport output variable set
#' @author Johanna Hoppe
#' @importFrom rmndt approx_dt
#' @import data.table
#' @export

reportExtendedTransportVarSet <- function(data, baseVarSet, timeResReporting) {

  constrYear <- variable <- period <- . <- value <- NULL

  # Report useful energy-----------------------------------------------------------------------
  fleetUEdemand <- reportUE(FEdemand = baseVarSet$ext$fleetFEdemand,
                                helpers = data$helpers)

  # Report vintages (stock without sales)-------------------------------------------------------
  vintages <- copy(data$fleetSizeAndComposition$fleetVehNumbersConstrYears[!period == constrYear])
  vintages[, variable := "Vintages"][, constrYear := NULL]
  cols <- names(vintages)
  vintages <- vintages[, .(value = sum(value)), by = eval(cols[!cols %in% c("value", "constrYear")])]
  vintages <- approx_dt(vintages, unique(fleetUEdemand$period), "period", "value", extrapolate = TRUE)

  # Report annualized TCO per pkm/tkm
  aggregatedCAPEX <- data$combinedCAPEXandOPEX[grepl(".*Capital.*", variable)]
  aggregatedCAPEX <- aggregatedCAPEX[, .(value = sum(value)), by = c("region", "period", "univocalName", "technology", "unit")]
  aggregatedCAPEX[, variable := "Capital costs"]
  combinedCAPEXandOPEX <- rbind(data$combinedCAPEXandOPEX[!grepl(".*Capital.*", variable)],
                                aggregatedCAPEX)
  combinedCAPEXandOPEX <- merge(combinedCAPEXandOPEX, data$helpers$decisionTree,
                                         by = intersect(names(data$combinedCAPEXandOPEX),
                                                        names(data$helpers$decisionTree)))
  combinedCAPEXandOPEX[, variable := paste0("TCO sales ", variable)]

  data$GDPppp[, variable := "GDP|PPP"][, value := value * 1e-3][, unit := "billion constant 2017 Int$PPP"]
  data$population[, variable := "Population"][, unit := "million"]
  data$GDPpcPPP[, variable := "GDPpc|PPP"][, value := value * 1e-3][, unit := "billion constant 2017 Int$PPP"]
  data$GDPMER[, variable := "GDP|MER"][, value := value * 1e-3][, unit := "billion constant 2017 Int$MER"]
  data$GDPpcMER[, variable := "GDPpc|MER"][, value := value * 1e-3][, unit := "billion constant 2017 Int$MER"]

  # Report transport input data if available
  inputData <- c("CAPEXother", "nonFuelOPEXtrackedFleet", "subsidies", "timeValueCosts", "annualMileage", "scenSpecLoadFactor",
                 "loadFactorRaw", "scenSpecEnIntensity", "energyIntensityRaw")
  inputData <- inputData[inputData %in% names(data)]
  inputData <- lapply(copy(data[inputData]), function(item, decisionTree) {item <- merge(item, decisionTree,
                                                                            by = intersect(names(item),
                                                                            names(decisionTree)), allow.cartesian = TRUE)},
                                                                            data$helpers$decisionTree)
  inputData$loadFactorRaw[, variable := paste0(variable, " raw")]
  inputData$energyIntensityRaw[, variable := paste0(variable, " raw")]

  # Split extensive and intensive variables ---------------------------------------------------
  outputVarsExt <- list(fleetUEdemand = fleetUEdemand,
                        vintages      = vintages,
                        GDPppp = data$GDPppp,
                        GDPMER = data$GDPMER,
                        population = data$population)
  outputVarsInt <- list(scenScpecPrefTrends = data$scenSpecPrefTrends[, level := NULL],
                        combinedCAPEXandOPEX = combinedCAPEXandOPEX,
                        CAPEXother = inputData$CAPEXother,
                        nonFuelOPEXtrackedFleet = inputData$nonFuelOPEXtrackedFleet,
                        timeValueCosts = inputData$timeValueCosts,
                        annualMileage = inputData$annualMileage,
                        loadFactor = inputData$scenSpecLoadFactor,
                        loadFactorRaw = inputData$loadFactorRaw,
                        energyIntensity = inputData$scenSpecEnIntensity,
                        energyIntensityRaw = inputData$energyIntensityRaw,
                        GDPpcPPP = data$GDPpcPPP,
                        GDPpcMER = data$GDPpcMER)
  outputVars <- list(ext = outputVarsExt,
                     int = outputVarsInt)
  return(outputVars)
}
