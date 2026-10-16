#'Report the energy intensity per  p/tkm
#'
#'Dividing the energy intensity per veh km by the
#'load factor gives it per unit of energy service.
#'Passenger and freight share one variable name and receive the unit of their sector (MJ/pkm,
#'MJ/tkm). On the aggregation levels that both sectors share (e.g. |Transport|Road) the unit
#'collapses to MJ/(p|t)km, as aggregateVariables() does it for the energy service and the costs.
#'The energy intensity per energy service is calculated for all modes, so that the energy service
#'weighted aggregation of every level is correct. Which of the resulting variables are reported is
#'configured in helpersReportingEnergyIntensityPerES.csv.
#'
#' @param fleetEnergyIntensity Energy intensity linked to the vehicle fleet in MJ/vehkm
#' @param loadFactor Persons or tons per vehicle
#' @param helpers List of helpers
#'
#' @returns Energy intensity per service on fleet level
#' @author Jarusch Muessel
#' @import data.table
#' @export

reportEnergyIntensityPerES <- function(fleetEnergyIntensity, loadFactor, helpers) {

  value <- unit <- variable <- energyIntensity <- univocalName <- NULL

  energyIntensityPerES <- copy(fleetEnergyIntensity)[, c("variable", "unit") := NULL]
  setnames(energyIntensityPerES, "value", "energyIntensity")
  loadFactor <- copy(loadFactor)[, c("variable", "unit") := NULL]
  setnames(loadFactor, "value", "loadFactor")

  # Calculate the energy intensity per energy service ------------------------------------------
  energyIntensityPerES <- merge(energyIntensityPerES, loadFactor,
                                by = intersect(names(energyIntensityPerES), names(loadFactor)))
  energyIntensityPerES[, value := energyIntensity / loadFactor]
  energyIntensityPerES[, c("energyIntensity", "loadFactor") := NULL]
  # Active modes feature an energy intensity and a load factor of zero and would end up as NaN
  energyIntensityPerES[univocalName %chin% c("Cycle", "Walk"), value := 0]

  # Apply the unit of the respective sector ----------------------------------------------------
  # International Aviation features its own sector and is therefore not part of filterEntries$trn_pass
  passengerModes <- c(helpers$filterEntries$trn_pass, "International Aviation")
  energyIntensityPerES[, variable := "Energy intensity per service"]
  energyIntensityPerES[univocalName %chin% passengerModes, unit := "MJ/pkm"]
  energyIntensityPerES[!univocalName %chin% passengerModes, unit := "MJ/tkm"]

  if (anyNA(energyIntensityPerES)) stop("Energy intensity per service contains NAs.
                                        Please check reportEnergyIntensityPerES()")

  return(energyIntensityPerES)
}
