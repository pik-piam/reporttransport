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

  # Switch from mixed time resolution to the reporting time resolution for all vars------------
  loadFactor <- copy(data$loadFactor)[period %in% timeResReporting]
  baseVarSet$ext$fleetFEdemand <- baseVarSet$ext$fleetFEdemand[period %in% timeResReporting]

  # Report useful energy-----------------------------------------------------------------------
  fleetUEdemand <- toolReportUE(FEdemand = baseVarSet$ext$fleetFEdemand,
                                helpers = data$helpers)

  # Report vintages (stock without sales)-------------------------------------------------------
  vintages <- copy(data$fleetSizeAndComposition$fleetVehNumbersConstrYears[!period == constrYear])
  vintages[, variable := "Vintages"][, constrYear := NULL]
  cols <- names(vintages)
  vintages <- vintages[, .(value = sum(value)), by = eval(cols[!cols %in% c("value", "constrYear")])]
  vintages <- approx_dt(vintages, timeResReporting, "period", "value", extrapolate = TRUE)
  loadFactor <- merge(loadFactor, data$helpers$decisionTree,
                      by = intersect(names(loadFactor), names(data$helpers$decisionTree)))

  # Split extensive and intensive variables ---------------------------------------------------
  outputVarsExt <- list(fleetUEdemand = fleetUEdemand,
                        vintages = vintages)
  outputVarsInt <- list(loadFactor = loadFactor)
  outputVars <- list(ext = outputVarsExt,
                     int = outputVarsInt)
  return(outputVars)
}
