#'Aggregate variables
#'
#'This function aggregates a large set of variables according to the edge transport decision tree and
#'additional aggregation levels that are set manually.
#'The aggregation is applied to all variables simultaneously. Additional transport specific columns are
#'transferred into the variable column entry
#'
#' @param vars Data.table with variables to aggregate in modified quitte style format
#' @param mapAggregation Map containing levels for aggregation in addition to the levels of the decision tree
#' @param weight Weight to aggregate variables
#'
#' @returns Data.table with aggregated variables
#' @author Johanna Hoppe
#' @import data.table
#' @export

aggregateVariables <- function(vars, mapAggregation, weight = NULL) {

  rownum <- sum <- toKeep <- sum <- variableName <- variable <- cols <- value <-
    . <- fuel <- sector <- technology <- subsectorL1 <- NULL

  filterVarsToAggregate <- function(aggrvars, cols, aggrOrder, keep = NULL) {
    # Select entries that feature the aggregation level
    last <- cols[length(cols)]
    aggrvars <- aggrvars[!is.na(get(last))]
    # "keep" maintains the technology level and, if desired, also the fuel level whilst
    # aggregating to higher levels.
    # Modes that do not feature lower aggregation levels from last to technology are aggregated
    # already on technology level in previous steps -> need to be filtered out to prevent duplicates
    if (!is.null(keep)) {
      aggrvars <- aggrvars[!is.na(get(keep))]
      aggrvars[, rownum := .I]
      # Check whether all columns until technology are NA: if yes -> filter Out
      after <- aggrOrder[(match(i, aggrOrder) + 1) : (match("technology", aggrOrder) - 1)]
      filterDT <- aggrvars[, ..after]
      filterDT <- filterDT[, lapply(.SD, function(x){!(is.na(x))})]
      filterDT[, sum := rowSums(.SD)]
      filterDT[, toKeep := ifelse(sum == 0, FALSE, TRUE)]
      filterDT <- filterDT[, c("toKeep")][, rownum := .I]
      aggrvars <- merge(aggrvars, filterDT, by = "rownum")
      aggrvars <- aggrvars[toKeep == TRUE][, toKeep := NULL]
    }
    return(aggrvars)
  }

  applyWeight <- function(aggrvars, weight, byCols) {
    aggrvarsWeight <- merge(aggrvars, weight, by = intersect(names(aggrvars), names(weight)), all.x = TRUE)
    aggrvarsWeight[, sum := sum(weight), by = eval(byCols)]
    aggrvarsWeight[sum == 0, weight := 1, by = eval(byCols)][, sum := NULL]
    return(aggrvarsWeight)
  }

  aggregateLevel <- function(aggrvars, byCols, weight = NULL) {
    if (!is.null(weight)) {
      aggrvars <- applyWeight(aggrvars, weight, byCols)
      aggrvars <- aggrvars[, .(value = sum(value * (weight / sum(weight)))), by = eval(byCols)]
    } else {
      aggrvars <- aggrvars[, .(value = sum(value)), by = eval(byCols)]
    }
    return(aggrvars)
  }

  createVariableEntry <- function(aggrvars, cols) {
    varNameCols <- c(cols)
    varNames <- unique(aggrvars[, ..cols])
    varNames[, variableName := do.call(paste0, .SD), .SDcols = varNameCols]
    varNames[, variableName := gsub("NA", "", variableName)]
    aggrvars <- merge(aggrvars, varNames, by = intersect(names(aggrvars), names(varNames)))
    aggrvars[, variable := paste0(variable, "|Transport", variableName)][, variableName := NULL]
    aggrvars[, eval(cols) := NULL]
    return(aggrvars)
  }

  # Preparation ---------------------------------------------------------------------------------------------------
  # Test for duplicated entries to prevent double counting in the aggregation
  test <- copy(vars)
  test[, value := NULL]
  if (anyDuplicated(test)) stop("Variables for aggregation contain duplicates.
                                Check reportEdgeTransport() to prevent double counting")

  # Prepare vars
  vars <- merge(vars, mapAggregation, by = "univocalName", allow.cartesian = TRUE, all.x = TRUE)
  aggrOrder <- c("sector", "aggrActiveModes", "aggrRail", "subsectorL1", "subsectorL2",
                 "subsectorL3", "aggrVehSizes", "vehicleType", "technology", "fuel")
  keep <- c("region",  aggrOrder, "variable", "unit", "period", "value")
  vars <- vars[, ..keep]
  # Initialize aggregated vars
  aggregatedvars <- vars[0][, eval(aggrOrder) := NULL]

  # Prepare weight
  if (!is.null(weight)) {
    weight <- merge(weight, mapAggregation, by = "univocalName", allow.cartesian = TRUE, all.x = TRUE)
    keepCols <- keep[keep != "fuel"]
    weight <- weight[, ..keepCols]
    weight[, c("variable", "unit") := NULL]
    setnames(weight, "value", "weight")
  }

  # Aggregate each level of the decision tree --------------------------------------------------------------------
  for (i in seq(0, length(aggrOrder) - 1)) {
    aggrvars <- copy(vars)
    cols <- aggrOrder[1:(length(aggrOrder) - i)]
    if (cols[length(cols)] == "sector") aggrvars <- aggrvars[!variable %in% c("Sales", "Vintages", "Stock")]
    byCols <- c("region",  cols, "variable", "unit", "period")
    aggrvars <- filterVarsToAggregate(aggrvars, cols, aggrOrder)
    aggrvars <- aggregateLevel(aggrvars, byCols, weight)
    aggrvars <- createVariableEntry(aggrvars, cols)
    aggregatedvars <- rbind(aggregatedvars, aggrvars)
  }

  # Aggregate keeping technology level --------------------------------------------------------------------
  aggregateLeveltoTech <- c("sector", "aggrRail", "subsectorL1", "subsectorL2", "subsectorL3", "aggrVehSizes")
  for (i in aggregateLeveltoTech) {
    aggrvars <- copy(vars)
    cols <- aggrOrder[1:match(i, aggrOrder)]
    if (cols[length(cols)] == "sector") aggrvars <- aggrvars[!variable %in% c("Sales", "Vintages", "Stock")]
    byCols <- c("region",  cols, "technology", "variable", "unit", "period")
    aggrvars <- filterVarsToAggregate(aggrvars, cols, aggrOrder, keep = "technology")
    aggrvars <- aggregateLevel(aggrvars, byCols, weight)
    aggrvars <- createVariableEntry(aggrvars, c(cols, "technology"))
    aggregatedvars <- rbind(aggregatedvars, aggrvars)
  }

  # Aggregate keeping fuel level --------------------------------------------------------------------
  if (nrow(vars[!is.na(fuel)]) > 0) {
    aggregateLeveltoTech <- c("sector", "aggrRail", "subsectorL1", "subsectorL2", "subsectorL3", "aggrVehSizes")
    for (i in aggregateLeveltoTech) {
      aggrvars <- copy(vars)
      cols <- aggrOrder[1:match(i, aggrOrder)]
      byCols <- c("region",  cols, "technology", "fuel", "variable", "unit", "period")
      aggrvars <- filterVarsToAggregate(aggrvars, cols, aggrOrder, keep = "fuel")
      aggrvars <- aggregateLevel(aggrvars, byCols, weight)
      aggrvars <- createVariableEntry(aggrvars, c(cols, "technology", "fuel"))
      aggregatedvars <- rbind(aggregatedvars, aggrvars)
    }
  }

  exclude <- c("Sales", "Vintages", "Stock", "Load factor")
  varsForFurtherAggregation <- vars[!variable %in% exclude]

  # Aggregate Pass with bunkers --------------------------------------------------------------------
  aggrvars <- copy(varsForFurtherAggregation)
  aggrvars <- aggrvars[grepl(".*Pass.*", sector)]
  byCols <- c("region", "variable", "unit", "period")
  aggrvars <- aggregateLevel(aggrvars, byCols, weight)
  aggrvars[, variable := paste0(variable, "|Transport|Pass with bunkers")]
  aggregatedvars <- rbind(aggregatedvars, aggrvars)
  # Aggregate sectors with bunkers keeping technology level
  aggrvars <- copy(varsForFurtherAggregation)
  # Active modes need to be excluded as they dont have a technology
  aggrvars <- aggrvars[!is.na(technology)]
  aggrvars <- aggrvars[grepl(".*Pass.*", sector)]
  byCols <- c("region",  "technology", "variable", "unit", "period")
  aggrvars <- aggregateLevel(aggrvars, byCols, weight)
  aggrvars[, variable := paste0(variable, "|Transport|Pass with bunkers", technology)][, c("technology") := NULL]
  aggregatedvars <- rbind(aggregatedvars, aggrvars)

  # Aggregate Freight with bunkers --------------------------------------------------------------------
  aggrvars <- copy(varsForFurtherAggregation)
  aggrvars <- aggrvars[grepl(".*Freight.*", sector)]
  byCols <- c("region", "variable", "unit", "period")
  aggrvars <- aggregateLevel(aggrvars, byCols, weight)
  aggrvars[, variable := paste0(variable, "|Transport|Freight with bunkers")]
  aggregatedvars <- rbind(aggregatedvars, aggrvars)
  # Aggregate sectors with bunkers keeping technology level
  aggrvars <- copy(varsForFurtherAggregation)
  # Active modes need to be excluded as they dont have a technology
  aggrvars <- aggrvars[!is.na(technology)]
  aggrvars <- aggrvars[grepl(".*Freight.*", sector)]
  byCols <- c("region",  "technology", "variable", "unit", "period")
  aggrvars <- aggregateLevel(aggrvars, byCols, weight)
  aggrvars[, variable := paste0(variable, "|Transport|Freight with bunkers", technology)][, c("technology") := NULL]
  aggregatedvars <- rbind(aggregatedvars, aggrvars)

  # Aggregate transport overall without bunkers --------------------------------------------------------------------
  aggrvars <- copy(varsForFurtherAggregation)
  aggrvars <- aggrvars[sector %in% c("|Pass", "|Freight")]
  aggrvars[grepl("billion pkm/yr|billion tkm/yr", unit), unit := "billion (p|t)km/yr"]
  aggrvars[grepl("US\\$2017/pkm|US\\$2017/tkm", unit), unit := "US$2017/(p|t)km"]
  byCols <- c("region", "variable", "unit", "period")
  aggrvars <- aggregateLevel(aggrvars, byCols, weight)
  aggrvars[, variable := paste0(variable, "|Transport")]
  aggregatedvars <- rbind(aggregatedvars, aggrvars)
  # Aggregate transport overall without bunkers keeping technology level
  aggrvars <- copy(varsForFurtherAggregation)
  aggrvars <- aggrvars[sector %in% c("|Pass", "|Freight")]
  # Active modes need to be excluded as they dont have a technology
  aggrvars <- aggrvars[!is.na(technology)]
  aggrvars[grepl("billion pkm/yr|billion tkm/yr", unit), unit := "billion (p|t)km/yr"]
  aggrvars[grepl("US\\$2017/pkm|US\\$2017/tkm", unit), unit := "US$2017/(p|t)km"]
  byCols <- c("region", "technology", "variable", "unit", "period")
  aggrvars <- aggregateLevel(aggrvars, byCols, weight)
  aggrvars[, variable := paste0(variable, "|Transport", technology)][, c("technology") := NULL]
  aggregatedvars <- rbind(aggregatedvars, aggrvars)

  # Aggregate transport overall with bunkers --------------------------------------------------------------------
  aggrvars <- copy(varsForFurtherAggregation)
  aggrvars[grepl("billion pkm/yr|billion tkm/yr", unit), unit := "billion (p|t)km/yr"]
  aggrvars[grepl("US\\$2017/pkm|US\\$2017/tkm", unit), unit := "US$2017/(p|t)km"]
  byCols <- c("region", "variable", "unit", "period")
  aggrvars <- aggregateLevel(aggrvars, byCols, weight)
  aggrvars[, variable := paste0(variable, "|Transport with bunkers")]
  aggregatedvars <- rbind(aggregatedvars, aggrvars)
  # Aggregate transport overall keeping technology level
  aggrvars <- copy(varsForFurtherAggregation)
  # Active modes need to be excluded as they dont have a technology
  aggrvars <- aggrvars[!is.na(technology)]
  aggrvars[grepl("billion pkm/yr|billion tkm/yr", unit), unit := "billion (p|t)km/yr"]
  aggrvars[grepl("US\\$2017/pkm|US\\$2017/tkm", unit), unit := "US$2017/(p|t)km"]
  byCols <- c("region", "technology", "variable", "unit", "period")
  aggrvars <- aggregateLevel(aggrvars, byCols, weight)
  aggrvars[, variable := paste0(variable, "|Transport with bunkers", technology)][, c("technology") := NULL]
  aggregatedvars <- rbind(aggregatedvars, aggrvars)

  # Aggregate transport road --------------------------------------------------------------------
  aggrvars <- copy(varsForFurtherAggregation)
  aggrvars <- aggrvars[subsectorL1 == "|Road"]
  aggrvars[grepl("billion pkm/yr|billion tkm/yr", unit), unit := "billion (p|t)km/yr"]
  aggrvars[grepl("US\\$2017/pkm|US\\$2017/tkm", unit), unit := "US$2017/(p|t)km"]
  byCols <- c("region", "variable", "unit", "period")
  aggrvars <- aggregateLevel(aggrvars, byCols, weight)
  aggrvars[, variable := paste0(variable, "|Transport|Road")]
  aggregatedvars <- rbind(aggregatedvars, aggrvars)
  # Aggregate transport overall keeping technology level
  aggrvars <- copy(varsForFurtherAggregation)
  # Active modes need to be excluded as they dont have a technology
  aggrvars <- aggrvars[!is.na(technology) & subsectorL1 == "|Road"]
  aggrvars[grepl("billion pkm/yr|billion tkm/yr", unit), unit := "billion (p|t)km/yr"]
  aggrvars[grepl("US\\$2017/pkm|US\\$2017/tkm", unit), unit := "US$2017/(p|t)km"]
  byCols <- c("region", "technology", "variable", "unit", "period")
  aggrvars <- aggregateLevel(aggrvars, byCols, weight)
  aggrvars[, variable := paste0(variable, "|Transport|Road", technology)][, c("technology") := NULL]
  aggregatedvars <- rbind(aggregatedvars, aggrvars)

  # Aggregate transport rail --------------------------------------------------------------------
  aggrvars <- copy(varsForFurtherAggregation)
  aggrvars <- aggrvars[subsectorL1 %in% c("|Rail", "|HSR", "|non-HSR")]
  aggrvars[grepl("billion pkm/yr|billion tkm/yr", unit), unit := "billion (p|t)km/yr"]
  aggrvars[grepl("US\\$2017/pkm|US\\$2017/tkm", unit), unit := "US$2017/(p|t)km"]
  byCols <- c("region", "variable", "unit", "period")
  aggrvars <- aggregateLevel(aggrvars, byCols, weight)
  aggrvars[, variable := paste0(variable, "|Transport|Rail")]
  aggregatedvars <- rbind(aggregatedvars, aggrvars)
  # Aggregate rail keeping technology level
  aggrvars <- copy(varsForFurtherAggregation)
  aggrvars <- aggrvars[subsectorL1 %in% c("|Rail", "|HSR", "|non-HSR")]
  aggrvars[grepl("billion pkm/yr|billion tkm/yr", unit), unit := "billion (p|t)km/yr"]
  aggrvars[grepl("US\\$2017/pkm|US\\$2017/tkm", unit), unit := "US$2017/(p|t)km"]
  byCols <- c("region", "technology", "variable", "unit", "period")
  aggrvars <- aggregateLevel(aggrvars, byCols, weight)
  aggrvars[, variable := paste0(variable, "|Transport|Rail", technology)][, c("technology") := NULL]
  aggregatedvars <- rbind(aggregatedvars, aggrvars)
  # Aggregate rail keeping fuel level
  aggrvars <- copy(varsForFurtherAggregation)
  aggrvars <- aggrvars[subsectorL1 %in% c("|Rail", "|HSR", "|non-HSR") & !is.na(fuel)]
  aggrvars[grepl("billion pkm/yr|billion tkm/yr", unit), unit := "billion (p|t)km/yr"]
  aggrvars[grepl("US\\$2017/pkm|US\\$2017/tkm", unit), unit := "US$2017/(p|t)km"]
  byCols <- c("region", "technology", "fuel", "variable", "unit", "period")
  aggrvars <- aggregateLevel(aggrvars, byCols, weight)
  aggrvars[, variable := paste0(variable, "|Transport|Rail", technology, "|", fuel)][, c("technology", "fuel") := NULL]
  aggregatedvars <- rbind(aggregatedvars, aggrvars)

  # Aggregate aviation --------------------------------------------------------------------
  aggrvars <- copy(varsForFurtherAggregation)
  aggrvars <- aggrvars[grepl(".*Aviation.*", subsectorL1)]
  byCols <- c("region", "variable", "unit", "period")
  aggrvars <- aggregateLevel(aggrvars, byCols, weight)
  aggrvars[, variable := paste0(variable, "|Transport|Pass|Aviation")]
  aggregatedvars <- rbind(aggregatedvars, aggrvars)
  # Aggregate keeping technology level
  aggrvars <- copy(varsForFurtherAggregation)
  aggrvars <- aggrvars[grepl(".*Aviation.*", subsectorL1)]
  byCols <- c("region", "technology", "variable", "unit", "period")
  aggrvars <- aggregateLevel(aggrvars, byCols, weight)
  aggrvars[, variable := paste0(variable, "|Transport|Pass|Aviation", technology)][, c("technology") := NULL]
  aggregatedvars <- rbind(aggregatedvars, aggrvars)
  # Aggregate keeping fuel level
  aggrvars <- copy(varsForFurtherAggregation)
  aggrvars <- aggrvars[grepl(".*Aviation.*", subsectorL1) & !is.na(fuel)]
  byCols <- c("region", "technology", "fuel", "variable", "unit", "period")
  aggrvars <- aggregateLevel(aggrvars, byCols, weight)
  aggrvars[, variable := paste0(variable, "|Transport|Pass|Aviation", technology, "|", fuel)][, c("technology", "fuel") := NULL]
  aggregatedvars <- rbind(aggregatedvars, aggrvars)

  if (anyNA(aggregatedvars)) stop("Output variable contains NAs.
                                  Please check reportAndAggregatedMIF()")
  if (anyDuplicated(aggregatedvars[, c("region", "period", "variable")])) stop("Output variable contains Duplicates.
                                         Please check reportAndAggregatedMIF()")
  return(aggregatedvars)
}
