#'Convert variables into model intercomparison format MIF
#'
#' @param vars Variables to be aggregated and converted into MIF format
#' @param GDPMER GDP on market exchange rate basis to be used as weight for regional aggregation
#' @param helpers List of helpers
#' @param scenario Scenario name in MIF entry
#' @param model Model name in MIF entry
#' @param gdx GDX file containing further regional aggregation levels
#' @param isTransportExtendedReported Switch to enable the extended transport variable set
#'
#' @returns Variables provided in different aggregation levels in MIF format
#' @author Johanna Hoppe
#' @importFrom quitte aggregate_map as.quitte
#' @importFrom remind2 toolRegionSubsets
#' @import data.table
#' @export

convertToMIF <- function(vars, GDPMER, helpers, scenario, model, gdx,  isTransportExtendedReported = FALSE) {       # nolint: object_name_linter

  rownum <- name <- fuel <- aggrReg <- variable <- reportName <- region <- univocalName <- technology <- NULL

  applyReportingNames <- function(vars, mapNames) {

    rename <- function(columns, mapNames) {
      dt <- data.table(name = c(columns))
      dt[, rownum := .I]
      dt <- merge(dt, mapNames, by = "name", allow.cartesian = TRUE, all.x = TRUE)
      # Important to prevent change of row order
      setkey(dt, "rownum")
      dt <- dt[!is.na(reportName), name := reportName]
      dt[grepl(".*tmp", name), name := NA]
      dt[!is.na(name), name := paste0("|", name)]

      return(dt$name)
    }
    colsUnchanged <- c("region", "period", "variable", "univocalName", "unit", "value")
    colOrder <- names(vars)
    colsToRename <- colOrder[!colOrder %in% colsUnchanged]
    dt <- vars[, ..colsToRename]
    dt <- dt[, lapply(.SD, rename, helpers$reportingNames)]
    varsNew <- cbind(vars[, ..colsUnchanged], dt)
    setcolorder(varsNew, colOrder)
    return(varsNew)
  }

  # Use ES demand as weight to aggregate over modes-------------------------------------------
  varsToMIFext <- rbindlist(vars$ext, fill = TRUE, use.names = TRUE)
  varsToMIFint <- vars$int[!names(vars) %in% c("FEsplitShares")]
  noAggregationvars <- vars$int[["FEsplitShares"]]
  varsToMIFint <- rbindlist(varsToMIFint, fill = TRUE, use.names = TRUE)

  # Apply variable naming convention----------------------------------------------------------
  varsToMIFext <- applyReportingNames(varsToMIFext, helpers$reportingNames)
  varsToMIFint[, fuel := NA]
  varsToMIFint <- applyReportingNames(varsToMIFint, helpers$reportingNames)

  # Regional aggregation----------------------------------------------------------------------
  ## Aggregation to world is always supplied
  mapWorld <- unique(varsToMIFext[, c("region")])[, aggrReg := "World"]
  worldDataExt <- as.data.table(aggregate_map(varsToMIFext, mapWorld, by = "region"))
  weight <- copy(varsToMIFext[variable == "ES"])
  weight[, c("variable", "unit", "fuel") := NULL]
  setnames(weight, "value", "weight")
  weightedInt <- merge(weight,varsToMIFint, by = intersect(names(varsToMIFint), names(weight)), all.x = TRUE)
  byCols <- names(weightedInt)
  byCols <- byCols[!byCols %in% c("region", "value", "weight")]
  weightedInt[, sum := sum(weight), by = eval(byCols)]
  weightedInt[sum == 0, weight := 1, by = eval(byCols)][, sum := NULL]
  worldDataInt <- weightedInt[, .(value = sum(value * (weight / sum(weight)))), by = eval(byCols)]
  worldDataInt[, region := "World"]
  varsToMIFint <- rbind(varsToMIFint, worldDataInt)
  varsToMIFext <- rbind(varsToMIFext, worldDataExt)

  ## Additional regions
  ## if regionSubsetList != NULL -> gdx provides 21 region resolution
  regionSubsetList <- toolRegionSubsets(gdx)
  if (!is.null(regionSubsetList)) {
    # ADD EU-27 region aggregation
    if ("EUR" %in% names(regionSubsetList)) {
      regionSubsetList <- c(regionSubsetList, list(
        "EU27" = c("ENC", "EWN", "ECS", "ESC", "ECE", "FRA", "DEU", "ESW")
      ))
    }
    # Create Mapping for region Aggregation out of region SubsetList
    namesReg <- names(regionSubsetList)
    regSubsetMap <- data.table()
    for (i in seq_along(namesReg)) {
      tmp <- data.table(region = regionSubsetList[[i]], aggrReg = namesReg[i])
      regSubsetMap <- rbind(regSubsetMap, tmp)
    }
    regSubsetDataExt <- as.data.table(aggregate_map(varsToMIFext[region %in% unique(regSubsetMap$region)],
                                                 regSubsetMap, by = "region"))
    weightedInt <- merge(varsToMIFint, regSubsetMap, by = intersect(names(varsToMIFint), names(regSubsetMap)), all.y = TRUE)
    weightedInt <- merge(weight, weightedInt, by = intersect(names(weightedInt), names(weight)), all.y = TRUE)
    byCols <- names(weightedInt)
    byCols <- byCols[!byCols %in% c("region", "value", "weight")]
    weightedInt[, sum := sum(weight), by = eval(byCols)]
    weightedInt[sum == 0, weight := 1, by = eval(byCols)][, sum := NULL]
    regSubsetDataInt <- weightedInt[, .(value = sum(value * (weight / sum(weight)))), by = eval(byCols)]
    setnames(regSubsetDataInt, "aggrReg", "region")

    varsToMIFint <- rbind(varsToMIFint, regSubsetDataInt)
    varsToMIFext <- rbind(varsToMIFext, regSubsetDataExt)
  }

  # Aggregate variables-----------------------------------------------------------------------
  toMIFext <- aggregateVariables(varsToMIFext, helpers$reportingAggregation)
  weight <- varsToMIFext[variable == "ES"]
  toMIFint <- aggregateVariables(varsToMIFint, helpers$reportingAggregation, weight)
  toMIFint <- rbind(noAggregationvars, toMIFint, fill = TRUE, use.names = TRUE)

  toMIF <- rbind(toMIFint, toMIFext)

  if (!is.null(vars$analytic)) {
    analyticVars <- rbindlist(vars$analytic, use.names = TRUE)
    analyticVars[, variable := paste0(univocalName, "|", technology, "|", variable)]
    analyticVars <- analyticVars[, c("region", "variable", "unit", "period", "value")]
    toMIF <- rbind(analyticVars, toMIF)
  }

  toMIF[, model := model][, scenario := scenario]
  toMIF <- as.quitte(toMIF)

  if (anyNA(toMIF)) stop("MIF output contains NAs.
                         Please check reportAndAggregatedMIF()")
  if (anyDuplicated(toMIF)) stop("MIF output contains duplicates.
                                 Please check reportAndAggregatedMIF()")

  return(toMIF)
}
