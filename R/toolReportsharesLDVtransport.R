#'Report FE shares of LDV in transport liquids
#'
#' @param fleetFEdemand final energy demand on fleet level
#' @param timeResReporting time resolution reporting
#' @param demScen demand scenario
#' @param SSPscen SSP scenario
#' @param transportPolScen transport policy scenario
#' @param helpers list of helpers
#' @returns Final energy shares of LDV in transport liquids [-]
#' @export
#' @author Johanna Hoppe
#' @import data.table

toolReportsharesLDVtransport <- function(fleetFEdemand, timeResReporting, demScen, SSPscen, transportPolScen, helpers) {

  # The LDV share in the total liquids is used to adjust the IEA values
  # to our differentiation of fedie/fepet in calcIO
  sharesLDVtransport <- copy(fleetFEdemand)[technology %in% c("Liquids") & period %in% timeResReporting]
  sharesLDVtransport <- sharesLDVtransport[,  .(value = sum(value[subsectorL2 == "trn_pass_road_LDV"]) / sum(value)), by = c("region", "period")]
  edgeTtoIsoMapping <- helpers$regionmappingISOto21to12[, c("countryCode", "regionCode21")]
  setnames(edgeTtoIsoMapping, c("countryCode", "regionCode21"), c("iso", "region"))
  sharesLDVtransport <- disaggregate_dt(sharesLDVtransport, edgeTtoIsoMapping, fewcol = "region",
                                          manycol = "iso", datacols = c("period"))
  setnames(sharesLDVtransport, c("iso", "period"), c("region", "year"))
  sharesLDVtransport <- approx_dt(sharesLDVtransport, seq(1990, 2150), "year", "value", "region", extrapolate = TRUE)
  sharesLDVtransport[,  sharetype := "share_LDV_totliq"]
  sharesLDVtransport[, varname := "shares_LDV_transport"]
  sharesLDVtransport[, DEM_scenario := demScen]
  sharesLDVtransport[, GDP_scenario := SSPscen]
  sharesLDVtransport[, EDGE_scenario := transportPolScen]
  setcolorder(sharesLDVtransport, c("region", "year", "GDP_scenario", "EDGE_scenario", "DEM_scenario",  "sharetype", "varname", "value"))

  return(sharesLDVtransport)
}
