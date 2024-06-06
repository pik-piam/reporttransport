#'Prepare data to report to REMIND
#'
#' @param dt data.table with data to report to REMIND
#' @param demScen demand scenario
#' @param SSPscen SSP scenario
#' @param transportPolScen transport policy scenario
#' @returns data table ready to report to REMIND
#' @export
#' @author Johanna Hoppe
#' @import data.table

prepareForREMIND <- function(dt, demScen, SSPscen, transportPolScen) {                                                                # nolint: object_name_linter
  cols <- names(copy(dt))
  cols <- cols[!cols %in% c("region", "period", "value")]
  dt[, DEM_scenario := paste0("gdp_", demScen)]
  dt[, GDP_scenario := paste0("gdp_", SSPscen)]
  dt[, EDGE_scenario := transportPolScen]
  setcolorder(dt, c("region", "period", "GDP_scenario", "DEM_scenario", "EDGE_scenario", cols, "value"))
  return(dt)
}
