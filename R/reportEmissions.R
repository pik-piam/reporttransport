#'Report emissions allocated to the transport sector.
#'Only direkt emissions from liquid or gaseous energy carriers used in ICEs are considered.
#'Indirect emissions from electricity and hydrogen are not allocated to the transport sector.
#'
#' @param dtFE Final energy data for liquids and gases
#' @param gdxPath Path to REMIND fulldata.gdx containing emission factors
#' @param prefix Prefix that specifies the emissions we are referring to in the variable name
#'               (either tailpipe or demand)
#' @param helpers List of helpers
#'
#' @returns Emissions data for provided values in dtFE
#' @author Johanna Hoppe
#' @importFrom gdxrrw rgdx rgdx.scalar
#' @import data.table
#' @export

reportEmissions <- function(dtFE, gdxPath, prefix, helpers) {

  emissionFactor <- period <- value <- to <- variable <- unit <- emissionType <- from <-
    univocalName <- technology <- NULL

  disaggregateFactor <- function(REMINDfactor, mapping) {
    setnames(REMINDfactor, "region", "regionCode12")
    disaggregateFactor <- merge(REMINDfactor, mapping,
                                by = "regionCode12", allow.cartesian = TRUE)
    setnames(disaggregateFactor, "regionCode21", "region")
    disaggregateFactor <- disaggregateFactor[, regionCode12 := NULL]
    return(disaggregateFactor)
  }

  # Get emission factors from REMIND gdx
  GtCtoGtCO2 <- rgdx.scalar(gdxPath, "sm_c_2_co2", ts = FALSE)                                                           # nolint: object_name_linter
  EJ2TWa <- rgdx.scalar(gdxPath, "sm_EJ_2_TWa", ts = FALSE)                                                              # nolint: object_name_linter
  emissionFactors <- rmndt::magpie2dt(gdx::readGDX(gdxPath, "pm_emifac", restore_zeros = FALSE))
  setnames(emissionFactors, c("all_regi", "all_enty", "all_enty1", "all_te", "all_enty2", "tall", "value"),
           c("region", "from", "to", "conversionTechnology", "emissionType", "period", "value"))

  numberOfRegions <- length(gdx::readGDX(gdxPath, "all_regi"))
  if (numberOfRegions == 12) {
    # store data of IND as an example of a non-aggregated region for testing
    # reorder colums for comparison
    testIND <- copy(emissionFactors)[region == "IND"]

    # de-aggregate from 12 to 21 regions if needed
    # using same share for all sub regions
    map <- unique(helpers$regionmappingISOto21to12[, c("regionCode12", "regionCode21")])
    emissionFactors <- disaggregateFactor(emissionFactors, map)

    # test: values for IND should stay unchanged
    # bring in same order, use data.frame for comparison to ignore keys
    cols <- names(emissionFactors)
    testIND <- testIND[, ..cols]
    testIND <- as.data.frame(testIND[do.call(order, testIND)])
    testINDafter <- emissionFactors[region == "IND"]
    testINDafter <- as.data.frame(testINDafter[do.call(order, testINDafter)])

    if (!isTRUE(all.equal(testIND, testINDafter, check.attributes = FALSE))) {
      stop("Error in disaggregation of emission Factors in reportEmissions()")
    }
  }

  # liquid fuels
  emissionFactors[from == "seliqfos" & to ==  "fedie" & emissionType == "co2",
                  emissionFactor := value * GtCtoGtCO2 * 1e3 * EJ2TWa]
  emissionFactors[from == "seliqfos" & to ==  "fepet" & emissionType == "co2",
                  emissionFactor := value * GtCtoGtCO2 * 1e3 * EJ2TWa]
  # gaseous fuels
  emissionFactors[from == "segafos" & to ==  "fegas" & emissionType == "co2",
                  emissionFactor := value * GtCtoGtCO2 * 1e3 * EJ2TWa]
  # unit MtCO2/EJ
  emissionFactors <- emissionFactors[!is.na(emissionFactor)]
  emissionFactors <- emissionFactors[, c("region", "period", "to", "emissionFactor")]

  ## attribute explicitly fuel used to the FE values
  dtFE <- copy(dtFE)
  dtFE[univocalName %in% c(helpers$filterEntries$trn_pass_road_LDV_4W,
                           helpers$filterEntries$trn_pass_road_LDV_2W) & technology == "Liquids", to := "fepet"]
  dtFE[!(univocalName %in% c(helpers$filterEntries$trn_pass_road_LDV_4W,
                             helpers$filterEntries$trn_pass_road_LDV_2W)) & technology == "Liquids", to := "fedie"]
  dtFE[technology == "Gases", to := "fegas"]

  emissionFactors[, period := as.double(as.character(period))]
  ## merge with emission factors
  emi <- merge(dtFE, emissionFactors, by = c("to", "region", "period"), all.x = TRUE)
  emi[is.na(emissionFactor), emissionFactor := 0]
  ## calculate emissions and attribute variable and unit names
  emi[, value := value * emissionFactor][, variable := paste0("Emi|CO2|Energy|", prefix)][, unit := "Mt CO2/yr"]
  emi[, c("to", "emissionFactor") := NULL]

  return(emi)
}
