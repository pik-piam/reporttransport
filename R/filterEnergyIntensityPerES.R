#' Reduce the Energy intensity per service variables to the reported subset
#'
#' The Energy intensity per service is calculated and aggregated for all modes but only those
#' variables from helpersReportingEnergyIntensityPerES.csv are added to the MIF.
#'
#' @param dt Aggregated MIF variables.
#' @param varsToReport Variable name prefixes of the Energy intensity per service subtrees to keep.
#'
#' @returns Aggregated MIF variables without the unreported Energy intensity per service variables.
#' @import data.table
#' @noRd

filterEnergyIntensityPerES <- function(dt, varsToReport = energyIntensityPerESvarsToReport()) {
  variable <- isEnergyIntensityPerES <- isReported <- NULL

  dt <- copy(dt)
  dt[, isEnergyIntensityPerES := startsWith(variable, "Energy intensity per service|")]
  dt[, isReported := startsWithAny(variable, varsToReport)]
  dt <- dt[isEnergyIntensityPerES == FALSE | isReported == TRUE]
  dt[, c("isEnergyIntensityPerES", "isReported") := NULL]

  return(dt[])
}

#' Load the configured Energy intensity per service variables
#'
#' @returns Variable name prefixes of the reported subtrees.
#' @import data.table
#' @noRd

energyIntensityPerESvarsToReport <- function() {
  variable <- NULL

  varsToReport <- fread(system.file("helpersReportingEnergyIntensityPerES.csv",
                                    package = "reporttransport", mustWork = TRUE),
                        sep = ";", skip = 1)
  varsToReport <- varsToReport[!is.na(variable) & !variable == ""]

  return(varsToReport$variable)
}

#' Test for each element whether it starts with any of the supplied prefixes
#'
#' @param x Character vector to test.
#' @param prefixes Character vector of prefixes.
#'
#' @returns Logical vector of the length of x.
#' @noRd

startsWithAny <- function(x, prefixes) {
  matched <- rep(FALSE, length(x))
  for (prefix in prefixes) {
    matched <- matched | startsWith(x, prefix)
  }

  return(matched)
}
