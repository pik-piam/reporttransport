#' Compare parameters in edget and REMIND and store their deviation to track model convergence
#'
#' @author Johanna Hoppe
#' @export

checkConvergence <- function() {
#---------------------------------------------------
# in a remind run the runfolder is the working dircetory
load("config.Rdata")

# ---- load data ----
gdx <- "fulldata.gdx"
mapEdgeToREMIND <- fread(system.file("extdata/helpersMappingEdgeTtoREMINDcategories.csv",
                                     package = "edgeTransport", mustWork = TRUE))
mapEdgeToREMIND <- mapEdgeToREMIND[!is.na(all_teEs)]
demScen <- cfg$gms$cm_demScen
SSPscen <- cfg$gms$cm_GDPpopScen
timeResReporting <- c(seq(1900,1985,5),
                      seq(1990, 2060, by = 5),
                      seq(2070, 2110, by = 10),
                      2130, 2150)
transportPolScen <- cfg$gms$cm_EDGEtr_scen

# Load edge-t data from runfolder
filesToLoad <- c("hybridElecShare",
                 "helpers")

if (length(filesToLoad) > 0) {
  filePaths <- list.files("./EDGE-T", recursive = TRUE, full.names = TRUE)
  pathFilesToLoad <- unlist(lapply(filesToLoad, function(x) {filePaths[grepl(paste0(x, ".RDS"), filePaths)]}))
  itemNames <- basename(pathFilesToLoad)
  itemNames <- sub("\\.RDS$", "", itemNames)
  addFiles <- lapply(pathFilesToLoad, readRDS)
  names(addFiles) <- itemNames
  data <- c(data, addFiles)
}

baseOutput <- reportEdgeTransport("./EDGE-T",
                                  isTransportReported = FALSE)

fleetESdemand <- baseOutput$ext$fleetESdemand
fleetFEdemand <- baseOutput$ext$fleetFEdemand


# ---- Energy intensity ----
  # 1: pm_fe2es Energy intensity of transport nodes parameter from the last REMIND iteration -> REMIND_val
  pm_fe2es <- gdx::readGDX(gdx, c("pm_fe2es"), field = "l", restore_zeros = FALSE)
  pm_fe2es <- pm_fe2es[, , unique(mapEdgeToREMIND$all_teEs)]
  pm_fe2es <- rmndt::magpie2dt(pm_fe2es, regioncol = "all_regi",
                               yearcol = "tall", datacols = "all_teEs", valcol = "REMINDvalue")

  # 2: Energy intensity reported by EDGE-T to REMIND in[trn pkm/Twa or trn tkm/Twa]
  # Currently EDGE-T always runs on 21 regions, but REMIND potentially runs only on 12 regions
  if (length(unique(pm_fe2es$all_regi)) == 12) {
    ESweight <- copy(fleetESdemand)[, c("unit", "variable") := NULL]
    setnames(ESweight, c("region", "value"), c("regionCode21", "weight"))
    dataColumns <- names(fleetESdemand)[!names(fleetESdemand) %in% c("region", "period", "value")]
    regionMap <- unique(data$helpers$regionmappingISOto21to12[, c("regionCode12", "regionCode21")])
    setnames(fleetESdemand, "region", "regionCode21")
    fleetESdemand <- rmndt::aggregate_dt(fleetESdemand, regionMap,
                                         fewcol = "regionCode12", manycol = "regionCode21",
                                         datacols = dataColumns, yearcol = "period")

    setnames(fleetESdemand, "regionCode12", "region")
    setnames(fleetFEdemand, "region", "regionCode21")
    fleetFEdemand <- rmndt::aggregate_dt(fleetFEdemand, regionMap,
                                         fewcol = "regionCode12", manycol = "regionCode21",
                                         datacols = dataColumns, yearcol = "period")
    setnames(fleetFEdemand, "regionCode12", "region")
  }

  f35_fe2es <- reportToREMINDenergyEfficiency(fleetFEdemand,
                                              fleetESdemand,
                                              data$hybridElecShare,
                                              timeResReporting,
                                              demScen,
                                              SSPscen,
                                              transportPolScen,
                                              data$helpers)
  f35_fe2es <- f35_fe2es[, c("all_regi", "all_teEs", "tall", "value")]
  setnames(f35_fe2es, "value", "EDGEvalue")
  testEnergyIntensity <- merge(f35_fe2es, pm_fe2es, by = c("all_regi", "all_teEs", "tall"), all.x = TRUE)
  # Select comparison years
  testEnergyIntensity <- testEnergyIntensity[tall >= 2005 & tall <= 2100]
  # Calculate deviation:
  # Last REMIND iteration vs last EDGE-T iteration
  testEnergyIntensity[, deviationAbsolute := abs(REMINDvalue - EDGEvalue)]
  testEnergyIntensity[, deviationRelativeToEDGET := abs(REMINDvalue - EDGEvalue)/EDGEvalue][, variable := "Energy Intensity"][, comparison := "Deviation last iteration REMIND vs last iteration EDGE-T"]
  numericCols <- c("REMINDvalue", "EDGEvalue", "deviationAbsolute", "deviationRelativeToEDGET")
  testEnergyIntensity[, (numericCols) := lapply(.SD, function(x) sprintf("%.2E", signif(x, 6))), .SDcols = numericCols]
  utils::write.table(testEnergyIntensity, file.path("EDGE-T", "trackREMINDvsEDGETparameterEnergyIntensity.csv"), row.names = FALSE, sep = ";", quote = FALSE)
  setnames(testEnergyIntensity, "all_teEs", "REMINDset")
# ---- Energy service demand ----

  # EDGE-T to REMIND
  EDGEtoREMINDes <- reportToREMINDesDemand(fleetESdemand,
                                           data$hybridElecShare,
                                           timeResReporting,
                                           demScen,
                                           SSPscen,
                                           transportPolScen,
                                           data$helpers)
  setnames(EDGEtoREMINDes, "value", "EDGEtoREMINDes")

  # vm_prodEs used in reportFE() for REMIND mif -> REMINDprodEs
  vm_prodEs <- gdx::readGDX(gdx, c("v_prodEs"), field = "l", restore_zeros = FALSE)
  vm_prodEs <- magpie2dt(vm_prodEs, regioncol = "all_regi",
                         yearcol = "tall", datacols = c("all_enty", "all_esty", "all_teEs"), valcol = "REMINDprodEs")
  vm_prodEs[, c("all_enty", "all_esty") := NULL]
  # REMIND to EDGE-T (vm_cesIO) -> REMINDcesIO
  vm_cesIO <- gdx::readGDX(gdx, c("vm_cesIO"), field = "l", restore_zeros = FALSE)
  vm_cesIO <- vm_cesIO[, , c("entrp_pass_sm", "entrp_pass_lo", "entrp_frgt_sm", "entrp_frgt_lo")]
  vm_cesIO <- magpie2dt(vm_cesIO, regioncol = "all_regi",
                        yearcol = "tall", datacols = "all_in", valcol = "REMINDcesIO")
  # last REMIND iteration energy service demand loaded by edget -> REMINDtoEDGEes
  REMINDtoEDGEes <- edgeTransport::toolLoadREMINDesDemand(gdx, data$helpers)
  map <- unique(mapEdgeToREMIND[, c("univocalName", "all_in")])
  decTree <- unique(data$helpers$decisionTree[, c("sector", "univocalName")])
  map <- merge(map, decTree, by = "univocalName", allow.cartesian = TRUE)
  map <- unique(map[, c("all_in", "sector")])
  REMINDtoEDGEes <- merge(REMINDtoEDGEes, map, by = "sector")[, sector := NULL]
  setnames(REMINDtoEDGEes, c("region", "period", "value"), c("all_regi", "tall", "REMINDtoEDGEes"))
  REMINDtoEDGEes <- REMINDtoEDGEes[, c("all_regi", "tall", "all_in", "REMINDtoEDGEes")]
  trillionToBillion <- 1e3
  REMINDtoEDGEes[, REMINDtoEDGEes := REMINDtoEDGEes / trillionToBillion]

  #check whether vm_cesIO and vm_prodEs are the same
  EDGEtoREMINDes <- merge(EDGEtoREMINDes, unique(mapEdgeToREMIND[, c("all_teEs", "all_in")]), by = "all_teEs", allow.cartesian = TRUE)
  EDGEtoREMINDes <- EDGEtoREMINDes[, .(EDGEtoREMINDes = sum(EDGEtoREMINDes)), by = c("all_regi", "tall", "all_in")]
  vm_prodEs <- merge(vm_prodEs, unique(mapEdgeToREMIND[, c("all_teEs", "all_in")]), by = "all_teEs", allow.cartesian = TRUE)
  SumVm_prodEs <- vm_prodEs[, .(SumREMINDprodEs = sum(REMINDprodEs)), by = c("all_regi", "tall", "all_in")]

  testES <- merge(vm_cesIO, SumVm_prodEs, by = c("all_regi", "tall", "all_in"))
  testES <- merge(testES, REMINDtoEDGEes, by = c("all_regi", "tall", "all_in"))
  testES <- merge(testES, EDGEtoREMINDes, by = c("all_regi", "tall", "all_in"))
  testES <- testES[tall >= 2005 & tall <= 2100]
  testES[, deviationAbsolute := abs(SumREMINDprodEs - REMINDcesIO)][, comparison := "REMIND vm_prodEs vs REMIND vm_cesIO"]
  testES[, deviationRelativeToEDGET := abs(SumREMINDprodEs - REMINDcesIO) / EDGEtoREMINDes]
  testES2 <- copy(testES)[, deviationAbsolute := abs(REMINDcesIO - EDGEtoREMINDes)][, comparison := "Last iteration REMIND vm_cesIO vs. last iteration EDGE-T (reported back to REMIND)"]
  testES2[, deviationRelativeToEDGET := abs(REMINDcesIO - EDGEtoREMINDes) / EDGEtoREMINDes]
  testES3 <- copy(testES)[, deviationAbsolute := abs(REMINDtoEDGEes - EDGEtoREMINDes)][, comparison := "Last iteration REMIND (loaded by EDGE-T) vs last iteration EDGE-T (reported back to REMIND)"]
  testES3[, deviationRelativeToEDGET := abs(REMINDtoEDGEes - EDGEtoREMINDes) / EDGEtoREMINDes]
  testES <- rbind(testES, testES2, testES3)
  numericCols <- c("REMINDcesIO", "SumREMINDprodEs", "REMINDtoEDGEes", "EDGEtoREMINDes", "deviationAbsolute", "deviationRelativeToEDGET")
  testES[, (numericCols) := lapply(.SD, function(x) sprintf("%.2E", signif(x, 6))), .SDcols = numericCols]
  testES[, variable := "Energy service demand"]
  utils::write.table(testES, file.path("EDGE-T", "trackREMINDvsEDGETparameterEnergyService.csv"), row.names = FALSE, sep = ";", quote = FALSE)
  setnames(testES, "all_in", "REMINDset")

  trackConvergence <- rbind(testEnergyIntensity[, c("all_regi", "tall", "REMINDset", "deviationAbsolute", "deviationRelativeToEDGET", "comparison", "variable")],
                            testES[, c("all_regi", "tall", "REMINDset", "deviationAbsolute", "deviationRelativeToEDGET", "comparison", "variable")])
  # Only report outliers that deviate more than 1% from EDGE-T value
  trackConvergence <- trackConvergence[deviationRelativeToEDGET > 0.01]

  utils::write.table(trackConvergence, file.path("EDGE-T", "trackConvergence.csv"), row.names = FALSE, sep = ";", quote = FALSE)
}
