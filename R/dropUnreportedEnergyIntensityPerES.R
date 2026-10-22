#' Drop the unreported Energy intensity per service variables
#'
#' The Energy intensity per service is calculated and aggregated for all modes but only those
#' variables from helpersReportingEnergyIntensityPerES.csv are added to the MIF.
#'
#' @param dt Aggregated MIF variables.
#'
#' @returns Aggregated MIF variables without the unreported Energy intensity per service variables.
#' @import data.table
#' @noRd

dropUnreportedEnergyIntensityPerES <- function(dt) {
  variable <- NULL

  startsWithAny <- function(x, prefixes) {
    matched <- rep(FALSE, length(x))
    for (prefix in prefixes) {
      matched <- matched | startsWith(x, prefix)
    }
    return(matched)
  }

  varsToReport <- fread(system.file("helpersReportingEnergyIntensityPerES.csv",
                                    package = "reporttransport", mustWork = TRUE),
                        sep = ";", skip = 1)
  varsToReport <- varsToReport[!is.na(variable) & variable != ""]$variable

  return(dt[!startsWith(variable, "Energy intensity per service|") | startsWithAny(variable, varsToReport)])
}
