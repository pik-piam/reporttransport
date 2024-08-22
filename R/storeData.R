#'Store EDGE-Transport model results
#'
#'This function creates the EDGE-Transport outputfolder and stores all outputfiles in the respective subfolders
#'
#' @param outputFolder Path to folder for storing output data
#' @param varsList Raw model results
#' @param ... Optional passing of additional variables
#'
#' @author Johanna Hoppe
#' @importFrom utils write.csv packageVersion
#' @import data.table
#' @export

storeData <- function(outputFolder, varsList = NULL, ...) {

  allocateFile <- function(varName) {
    subfolder <- NULL
    if (varName %in% c("hybridElecShare",
                       "histESdemand",
                       "energyIntensityRaw",
                       "loadFactorRaw",
                       "annualMileage",
                       "CAPEXtrackedFleet",
                       "nonFuelOPEXtrackedFleet",
                       "CAPEXother",
                       "nonFuelOPEXother",
                       "CAPEXandNonFuelOPEX",
                       "timeValueCosts",
                       "subsidies",
                       "GDPppp",
                       "population",
                       "helpers")) subfolder <- "1_InputDataRaw"
    if (varName %in% c("scenSpecPrefTrends",
                       "REMINDfuelCosts",
                       "REMINDfuelCostIterations",
                       "scenSpecLoadFactor",
                       "scenSpecEnIntensity",
                       "combinedCAPEXandOPEX",
                       "upfrontCAPEXtrackedFleet",
                       "initialIncoCosts")) subfolder <- "2_InputDataPolicy"
    if (varName %in% c("histPrefs")) subfolder <- "3_Calibration"
    if (varName %in% c("fleetSizeAndComposition",
                       "vehSalesAndModeShares",
                       "fleetVehNumbersIterations",
                       "endogenousCostsIterations",
                       "endogenousCosts",
                       "sectorESdemand",
                       "ESdemandFVsalesLevel",
                       "fleetVehiclesPerTech",
                       "harmFactors")) subfolder <- "4_Output"
    if (varName %in% c("REMINDinputData")) subfolder <- "5_REMINDinputData"
    if (is.null(subfolder)) stop(paste0("No subfolder assigned to ", varName))

    return(subfolder)
  }

  storeRDS <- function(varName, vars, outputFolder, subfolder = NULL) {
    if (is.null(subfolder)) subfolder <- allocateFile(varName)
    saveRDS(vars[[varName]], file.path(outputFolder, subfolder, paste0(varName, ".RDS")))
  }

  storeCSV <- function(varName, vars, outputFolder, subfolder = NULL) {
    if (is.null(subfolder)) subfolder <- allocateFile(varName)
    write.csv(vars[[varName]], file.path(outputFolder, subfolder, paste0(varName, ".csv")), row.names = FALSE)
  }
  vars <- list()
  if (!is.null(varsList)) vars <- varsList
  addVars <- list(...)
  vars <- append(vars, addVars)
  #########################################################################
  ## Create output folder and subfolders
  #########################################################################
  if (!dir.exists(outputFolder)) {
    dir.create(outputFolder, recursive = TRUE)
    dir.create(file.path(outputFolder, "1_InputDataRaw"))
    dir.create(file.path(outputFolder, "2_InputDataPolicy"))
    dir.create(file.path(outputFolder, "3_Calibration"))
    dir.create(file.path(outputFolder, "4_Output"))
  }

  #########################################################################
  ## Store special cases (files that should be stored in a different way
  ## than transferred in vars)
  #########################################################################
  # general data
  if (!(is.null(vars$SSPscen) & is.null(vars$transportPolScen) & is.null(vars$demScen))) {                          # nolint: vector_logic_linter
    cfg <- list(
      SSPscen = vars$SSPscen,
      transportPolScen = vars$transportPolScen,
      demScen = vars$demScen,
      timeStamp = format(Sys.time(), "%Y-%m-%d_%H.%M")
    )
    saveRDS(cfg, file.path(outputFolder, "cfg.RDS"))
    vars <- vars[!names(vars) %in% c("SSPscen", "transportPolScen", "demScen")]
  }
  if (!is.null(vars$gdxPath)) {
    file.copy(vars$gdxPath, file.path(outputFolder))
    vars <- vars[!names(vars) %in% c("gdxPath")]
  }

  # store calibration data if provided
  if (!is.null(vars$histPrefs)) {
    lapply(names(vars$histPrefs), storeRDS, vars$histPrefs, outputFolder, "3_Calibration")
    vars <- vars[!names(vars) %in% c("histPrefs")]
  }

  # store output data if provided
  if (!is.null(vars$fleetVehNumbersIterations)) {
    for (i in seq_along(vars$fleetVehNumbersIterations)) {
      # store fleetVehNumbers over iterations
      saveRDS(vars$fleetVehNumbersIterations[[i]], file.path(outputFolder, "4_Output",
                                                             paste0("fleetVehNumbersIteration", i, ".RDS")))
    }
    vars <- vars[!names(vars) %in% c("fleetVehNumbersIterations")]
  }
  if (!is.null(vars$endogenousCostsIterations)) {
    for (i in seq_along(vars$endogenousCostsIterations)) {
      saveRDS(vars$endogenousCostsIterations[[i]], file.path(outputFolder, "4_Output",
                                                             paste0("endogenousCostsIteration", i, ".RDS")))
    }
    vars <- vars[!names(vars) %in% c("endogenousCostsIterations")]
  }

  # store REMIND inputdata if provided
  if (!is.null(vars$REMINDinputData)) {
    if (!dir.exists(file.path(outputFolder, "5_REMINDinputData"))) {
      dir.create(file.path(outputFolder, "5_REMINDinputData"))
    }
    lapply(names(vars$REMINDinputData), storeCSV, vars$REMINDinputData, outputFolder, "5_REMINDinputData")
    vars <- vars[!names(vars) %in% c("REMINDinputData")]
  }

  ###########################################################################
  ## Remaining variables are allocated to a subfolder and stored as RDS files
  ###########################################################################
  if (!is.null(vars)) invisible(lapply(names(vars), storeRDS, vars, outputFolder))

}
