#' Harmonize variables reported by REMIND and EDGE-T.
#' REMIND reports energy service demand based on the last REMIND iteration before REMIND converged and EDGE-T based on the last EDGE-T iteration.
#' Depending on the number of REMIND iterations (<25/25-45/>45) until convergence, the last EDGE-T run happened up to (3/5/8) iterations ago
#' Therefore the Energy service demand on CES node level differs between REMIND and EDGE-T.
#' We want to harmonize ES und FE to equal the REMIND values.
#' Consequently, we want the detailed transport variables to be reported using the ES demand on CES node level from the
#' last REMIND iteration. We want to keep the energy intensity of REMIND, which is the energy intensity from the last EDGE-T run.
#' Therefore, we don't want to take the fuel prices from the last REMIND iteration and keep the vehicle sales and mode shares as they are
#' Deviations are stored in EDGE-T/trackConvergence.csv

#' @param ESdemandFVsalesLevel Energy service demand on sales level
#' @param fleetESdemand Energy service demand on fleet level
#' @param helpers EDGE-T helpers that include e.g. the decision tree
#' @returns harmonizedDemand harmonized energy service demand on FV (Fuel to vehicle) fleet level
#' @author Johanna Hoppe
#' @export

harmonizeREMINDvsEDGETenergyServiceDemand <- function(ESdemandFVsalesLevel, fleetESdemand, helpers) {
  # Read in energy service demand from last REMIND iteration
  gdx <- file.path(".", "fulldata.gdx")
  harmREMINDdemand <- edgeTransport::toolLoadREMINDesDemand(gdx, helpers)
  setnames(harmREMINDdemand, "value", "harmREMINDdemand")
  ESdemandFVfleetLevel <- rbind(ESdemandFVsalesLevel[!grepl("Bus.*|.*4W|.*freight_road.*", subsectorL3)],
                         fleetESdemand)
  #Change regional resolution to caluclate harmonizationfactors if necessary
  if (length(unique(harmREMINDdemand$region)) == 12) {
    regionMapping <- unique(copy(helpers$regionmappingISOto21to12)[, c("regionCode21", "regionCode12")])
    setnames(regionMapping, "regionCode21", "region")
    ESdemandToHarmonize <- merge(ESdemandFVfleetLevel, regionMapping, by = "region", all = TRUE, allow.cartesian = TRUE)
    ESdemandToHarmonize[, region := NULL]
    setnames(ESdemandToHarmonize, "regionCode12", "region")}
  ESdemandSectorfleetLevel <- ESdemandToHarmonize[, .(value = sum(value)), by = c("region", "period", "sector")]
  harmonizationFactors <- merge(harmREMINDdemand, ESdemandSectorfleetLevel, by = intersect(names(harmREMINDdemand), names(ESdemandSectorfleetLevel)))
  harmonizationFactors[, factor := harmREMINDdemand / value][, c("unit", "variable", "value", "harmREMINDdemand") := NULL]
  #Change regional resolution to distribute back harmonizationfactors if necessary
  if (length(unique(harmREMINDdemand$region)) == 12) {
    regionMapping <- unique(copy(helpers$regionmappingISOto21to12)[, c("regionCode21", "regionCode12")])
    setnames(regionMapping, "regionCode12", "region")
    harmonizationFactors <- merge(harmonizationFactors, regionMapping, by = "region", all = TRUE, allow.cartesian = TRUE)
    harmonizationFactors[, region := NULL]
    setnames(harmonizationFactors, "regionCode21", "region")
  }
  # Rescale fleet demand to top node energy service demand
  # Rework base reporting to receive fleet ESdemand directly
  # Replace old fleet ESdemand
  harmonizedDemand <- merge(ESdemandFVfleetLevel[period %in% unique(harmonizationFactors$period)],
                            harmonizationFactors, by = intersect(names(ESdemandFVfleetLevel), names(harmonizationFactors)),
                            allow.cartesian = TRUE, all.x = TRUE)
  harmonizedDemand[, value := value * factor][, factor := NULL]

  return(harmonizedDemand)
}
