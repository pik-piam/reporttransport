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

  outputVarsExt <- list()
  outputVarsInt <- list()

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

  if (!is.null(data$GDPppp)) {
    data$GDPppp[grepl("mil\\..*", unit), value := value * 1e-3][, unit := gsub("mil\\.", "billion", unit)]
    outputVarsExt <- c(outputVarsExt, list(GDPppp = data$GDPppp))
  }
  if (!is.null(data$GDPpcPPP)) {
    data$GDPpcPPP[grepl("mil\\..*", unit), value := value * 1e-3][, unit := gsub("mil\\.", "billion", unit)]
    outputVarsInt <- c(outputVarsInt, list(GDPpcPPP = data$GDPpcPPP))
  }
  if (!is.null(data$GDPMER)) {
    data$GDPMER[grepl("mil\\..*", unit), value := value * 1e-3][, unit := gsub("mil\\.", "billion", unit)]
    outputVarsExt <- c(outputVarsExt, list(GDPMER = data$GDPMER))
  }
  if (!is.null(data$GDPpcMER)) {
    data$GDPpcMER[grepl("mil\\..*", unit), value := value * 1e-3][, unit := gsub("mil\\.", "billion", unit)]
    outputVarsInt <- c(outputVarsInt, list(GDPpcMER = data$GDPpcMER))
  }
  if (!is.null(data$population)) {
    outputVarsExt <- c(outputVarsExt, list(population = data$population))
  }

  # Report transport input data if available
  inputData <- c("timeValueCosts", "annualMileage", "scenSpecLoadFactor",
                 "loadFactorRaw", "scenSpecEnIntensity", "energyIntensityRaw")
  inputData <- inputData[inputData %in% names(data)]

  if (!is.null(inputData)) {
    inputData <- lapply(copy(data[inputData]), function(item, decisionTree) {item <- merge(item, decisionTree,
                                                                            by = intersect(names(item),
                                                                            names(decisionTree)), allow.cartesian = TRUE)},
                                                                            data$helpers$decisionTree)
    outputVarsInt <- c(outputVarsInt, inputData)
  }



  # Split extensive and intensive variables ---------------------------------------------------
  outputVarsExt <- c(outputVarsExt,
                     list(fleetUEdemand = fleetUEdemand,
                          vintages      = vintages)
  )

  outputVarsInt <- c(outputVarsInt,
                     list(scenSpecPrefTrends = data$scenSpecPrefTrends[, level := NULL],
                          combinedCAPEXandOPEX = combinedCAPEXandOPEX)
  )
  outputVars <- list(ext = outputVarsExt,
                     int = outputVarsInt)
  return(outputVars)
}
