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

#'Renames vertain variables to prevent duplicates for variables that are reported also in remind2
#'
#' @param vars MIF variables
#' @returns MIF variables that are renamed to "Transport edge" where necessary
#' @author Johanna Hoppe
#' @import data.table
#'
renameDuplicateVariables <- function(vars) {

  #Duplicates: Variables that are reported as well by remind2 for the GAMS side of transport
  # in order to prevent duplicates the variables need to be renamed here
  # After the variable harmonization fix between REMIND/EDGE-T is in place, this can be moved and instead the remind2 variables should be renamed

  varsToRename <- c("ES|Transport|Road",
                    "ES|Transport|Pass",
                    "ES|Transport|Freight",
                    "FE|Transport|Pass",
                    "FE|Transport|Freight",
                    "FE|Transport",
                    "FE|Transport|Pass|Liquids",
                    "FE|Transport|Pass|Hydrogen",
                    "FE|Transport|Pass|Gases",
                    "FE|Transport|Pass|Electricity",
                    "FE|Transport|Freight|Liquids",
                    "FE|Transport|Freight|Hydrogen",
                    "FE|Transport|Freight|Gases",
                    "FE|Transport|Freight|Electricity",
                    "FE|Transport|Liquids",
                    "FE|Transport|Hydrogen",
                    "FE|Transport|Gases",
                    "FE|Transport|Electricity",
                    "UE|Transport|Pass|Electricity",
                    "UE|Transport|Freight|Electricity",
                    "UE|Transport|Electricity",
                    "UE|Transport|Pass|Hydrogen",
                    "UE|Transport|Freight|Hydrogen",
                    "UE|Transport|Hydrogen",
                    "UE|Transport|Pass|Liquids",
                    "UE|Transport|Freight|Liquids",
                    "UE|Transport|Liquids")

  vars[variable %in% varsToRename, variable := gsub("Transport", "Transport edge", variable)]

  return(vars)
}
