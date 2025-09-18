#'Convert variables into model intercomparison format MIF
#'
#' @param vars Variables to be aggregated and converted into MIF format
#' @param GDPMER GDP on market exchange rate basis to be used as weight for regional aggregation
#' @param helpers List of helpers
#' @param scenario Scenario name in MIF entry
#' @param model Model name in MIF entry
#' @param isTransportExtendedReported Switch to enable the extended transport variable set
#'
#' @returns Variables provided in different aggregation levels in MIF format
#' @author Johanna Hoppe
#' @importFrom quitte aggregate_map as.quitte
#' @import data.table
#' @export

convertToMIF <- function(vars, GDPMER, helpers, scenario, model, isTransportExtendedReported = FALSE) {       # nolint: object_name_linter
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
      dt[name == "", name := NA]
      dt[!is.na(name), name := paste0("|", name)]

      return(dt$name)
    }
    colsUnchanged <- c("region", "period", "variable", "univocalName", "unit", "value")
    colOrder <- names(vars)
    colsToRename <- colOrder[!colOrder %in% colsUnchanged]
    dt <- vars[, ..colsToRename]
    dt <- dt[, lapply(.SD, rename, helpers$reportingNames)]
    varsNew <- cbind(vars[, ..colsUnchanged], dt)
    # Remove spaces in vehicle types
    varsNew[, vehicleType := gsub(" \\(", "\\(", vehicleType)]
    setcolorder(varsNew, colOrder)
    return(varsNew)
  }

  noAggregationvars <- rbindlist(vars$int[c("GDPpcPPP", "GDPpcMER")], fill = TRUE, use.names = TRUE)
  varsToMIFint <- rbindlist(vars$int[!names(vars$int) %in% c("GDPpcPPP", "GDPpcMER")], fill = TRUE, use.names = TRUE)
  varsToMIFext <- rbindlist(vars$ext[!names(vars$ext) %in% c("GDPppp", "population", "GDPMER")], fill = TRUE, use.names = TRUE)

  # Regional aggregation----------------------------------------------------------------------
  ## Aggregation to world is always supplied
  mapWorld <- unique(vars$ext[[1]][, c("region")])[, aggrReg := "World"]
  worldDataExt <- lapply(vars$ext, function(x, mapWorld) as.data.table(aggregate_map(x, mapWorld, by = "region")), mapWorld)

  weight <- copy(vars$ext$fleetESdemand)
  weight[, c("variable", "unit") := NULL]
  setnames(weight, "value", "weight")

  #split shareweights
  if (!is.null(vars$int$scenSpecPrefTrends)) {
    Preferences <- list(
      PrefrenceFV = vars$int$scenSpecPrefTrends[variable == "Preference|FV"],
      PrefrenceS1S = vars$int$scenSpecPrefTrends[variable == "Preference|S1S"],
      PrefrenceS2S1 = vars$int$scenSpecPrefTrends[variable == "Preference|S2S1"],
      PrefrenceS3S2 = vars$int$scenSpecPrefTrends[variable == "Preference|S3S2"],
      PrefrenceVS3 = vars$int$scenSpecPrefTrends[variable == "Preference|VS3"]
    )
    vars$int <- vars$int[!names(vars$int) == "scenSpecPrefTrends"]
    vars$int <- append(vars$int, Preferences)
  }

  worldDataInt <- lapply(vars$int, function(x, weight) {
    byCols <- names(x)
    #sharweights include empty columns
    emptyColumns <- names(x)[sapply(x, function(x) all(is.na(x) | x == ""))]
    byCols <- byCols[!byCols %in% c("value") & byCols %in% names(weight) & !byCols %in% emptyColumns]
    weight <- weight[, .(weight = sum(weight)), by = eval(byCols)]
    weightedInt <- merge(x, weight, by = intersect(names(x), names(weight)), all.x = TRUE)
    byCols <- names(weightedInt)
    byCols <- byCols[!byCols %in% c("region", "value", "weight")]
    weightedInt[, sum := sum(weight), by = eval(byCols)]
    weightedInt[sum == 0, weight := 1, by = eval(byCols)][, sum := NULL]
    worldDataInt <- weightedInt[, .(value = sum(value * (weight / sum(weight)))), by = eval(byCols)]
    worldDataInt[, region := "World"]
    return(worldDataInt)}, weight)

  # Additional regions
  # EDGE-T is always running on 21 region resolution -> add H12 regions
  reg <- unique(varsToMIFext$region)
  regSubsetMap <- helpers$regionmappingISOto21to12[!regionCode12 %in% reg]
  regSubsetMap <- unique(regSubsetMap[, c("regionCode21", "regionCode12")])
  EU27 <- regSubsetMap[regionCode12 == "EUR" & !regionCode21 == "UKI"]
  EU27[, regionCode12 := "EU27"]
  regSubsetMap <- rbind(regSubsetMap, EU27)
  setnames(regSubsetMap, c("regionCode21", "regionCode12"), c("region", "aggrReg"))

  regSubsetDataExt <- lapply(vars$ext, function(x, regSubsetMap) {
    as.data.table(aggregate_map(x[region %in% unique(regSubsetMap$region)],
                                regSubsetMap, by = "region"))}, regSubsetMap)
  regSubsetDataInt <- lapply(vars$int, function(x, regSubsetMap, weight) {
    weightedInt <- merge(x, regSubsetMap, by = intersect(names(x), names(regSubsetMap)), all.y = TRUE)
    byCols <- names(x)
    #sharweights include empty columns
    emptyColumns <- names(x)[sapply(x, function(x) all(is.na(x) | x == ""))]
    byCols <- byCols[!byCols %in% c("value") & byCols %in% names(weight) & !byCols %in% emptyColumns]
    weight <- weight[, .(weight = sum(weight)), by = eval(byCols)]
    weightedInt <- merge(weight, weightedInt, by = intersect(names(weightedInt), names(weight)), all.y = TRUE)
    byCols <- names(weightedInt)
    byCols <- byCols[!byCols %in% c("region", "value", "weight")]
    weightedInt[, sum := sum(weight), by = eval(byCols)]
    weightedInt[sum == 0, weight := 1, by = eval(byCols)][, sum := NULL]
    regSubsetDataInt <- weightedInt[, .(value = sum(value * (weight / sum(weight)))), by = eval(byCols)]
    setnames(regSubsetDataInt, "aggrReg", "region")
    return(regSubsetDataInt)}, regSubsetMap, weight)

  noAggregationvars <- rbind(noAggregationvars,
                             rbindlist(regSubsetDataInt[c("GDPpcPPP", "GDPpcMER")], fill = TRUE, use.names = TRUE),
                             rbindlist(worldDataInt[c("GDPpcPPP", "GDPpcMER")], fill = TRUE, use.names = TRUE),
                             rbindlist(vars$ext[c("GDPppp", "population", "GDPMER")], fill = TRUE, use.names = TRUE),
                             rbindlist(regSubsetDataExt[c("GDPppp", "population", "GDPMER")], fill = TRUE, use.names = TRUE),
                             rbindlist(worldDataExt[c("GDPppp", "population", "GDPMER")], fill = TRUE, use.names = TRUE))

  varsToMIFint <- rbind(varsToMIFint,
                        rbindlist(regSubsetDataInt[!names(regSubsetDataInt) %in% c("GDPpcPPP", "GDPpcMER")], fill = TRUE, use.names = TRUE),
                        rbindlist(worldDataInt[!names(worldDataInt) %in% c("GDPpcPPP", "GDPpcMER")], fill = TRUE, use.names = TRUE))

  varsToMIFext <- rbind(varsToMIFext,
                        rbindlist(regSubsetDataExt[!names(regSubsetDataExt) %in% c("GDPppp", "population", "GDPMER")], fill = TRUE, use.names = TRUE),
                        rbindlist(worldDataExt[!names(worldDataExt) %in% c("GDPppp", "population", "GDPMER")], fill = TRUE, use.names = TRUE))


  # Apply variable naming convention----------------------------------------------------------
  varsToMIFext <- applyReportingNames(varsToMIFext, helpers$reportingNames)
  varsToMIFint[, fuel := NA]
  varsToMIFint <- applyReportingNames(varsToMIFint, helpers$reportingNames)

  # Aggregate variables-----------------------------------------------------------------------
  toMIFext <- aggregateVariables(varsToMIFext, helpers$reportingAggregation)
  weight <- varsToMIFext[variable == "ES"]
  toMIFint <- aggregateVariables(varsToMIFint, helpers$reportingAggregation, weight)

  if (!is.null(noAggregationvars)) {
    toMIF <- rbind(toMIFint, toMIFext, noAggregationvars)
  } else {toMIF <- rbind(toMIFint, toMIFext)}

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
