#'Report EDGE-Transport Model results
#'
#'This function reports the transport model results of an iterative or standalone run.
#'If not handed over in the function call, it first loads the transport model results from the stored RDS files.
#'Then it calculates the output variables and brings the data into the right format.
#'A basic output variables set is always calculated that is needed for all reporting packages.
#'With the help of switches, different reporting packages can be generated:
#'- isTransportReported activates the reduced reporting of transport variables in MIF format to be attached to
#'  a REMIND.mif.
#'  It includes the variables needed to create REMIND compareScenarios2 and report results for projects
#'- isTransportReported + isTransportExtendedReported activates further the extended reporting of transport variables
#'  and if isStored is activated as well, triggers the generation of a seperate transport.MIF.
#'  It includes the reduced reporting and additional transport variables for a detailed analysis of the transport
#'  sector using transportCompareScenarios
#'- isTransportReported + isTransportExtendedReported + isAnalyticsReported activates further the generation of
#'  additional variables
#'  for the analysis of the model behavior such as the inconvenience costs over iterations. They can be analyzed
#'  in the analytics sheet in compareScenariosTransport. It can be used in combination or without
#'  isTransportExtendedReported.
#'- isREMINDinputReported activates the reporting of REMIND input data from a standalone run. This mode is used
#'  in the REMIND input data generation with all other switches turned off. It can be also used in combination
#'  with the other switches.
#'
#' @param folderPath Path to the EDGE-Transport output folder of an iterative or standalone run
#' @param data List of model results. If not handed over, the data is loaded from the RDS files in the output folder
#' @param isTransportReported Switch for activating the reporting of transport data in MIF format
#' @param isTransportExtendedReported Switch for activating the reporting of detailed transport data im MIF format
#'                                    needed to create transportCompareScenarios
#' @param isAnalyticsReported Switch for activating reporting of model analytics data
#' @param isREMINDinputReported Switch for activating reporting of REMIND input data
#' @param isStored Switch for activating data storage and creating the transport.MIF file
#'
#' @returns The function either returns the REMINDinputData if isREMINDinputReported is
#'          enabled or the transport data in MIF format
#' @author Johanna Hoppe
#' @importFrom quitte write.mif
#' @import data.table
#' @export

reportEdgeTransport <- function(folderPath = file.path(".", "EDGE-T"), data = NULL, isTransportReported = TRUE,
                                    isTransportExtendedReported = FALSE, isAnalyticsReported = FALSE,
                                    isREMINDinputReported = FALSE, isStored = TRUE) {

  # If you want to change timeResReporting to timesteps outside the modeleled timesteps,
  # please add an interpolation step
  timeResReporting <-  c(seq(2005, 2060, by = 5), seq(2070, 2110, by = 10), 2130, 2150)

  #########################################################################
  ## Load data for reporting if data is not supplied in function call
  #########################################################################

  if (is.null(data)) {
    data <- list()
    # load files needed for all
    cfg <- readRDS(file.path(folderPath, "cfg.RDS"))
    data <- append(data, cfg[names(cfg) %in% c("SSPscen", "transportPolScen", "demScen")])
    data$hybridElecShare <- readRDS(file.path(folderPath, "1_InputDataRaw", "hybridElecShare.RDS"))
    data$helpers <- readRDS(file.path(folderPath, "1_InputDataRaw", "helpers.RDS"))
    data$combinedCAPEXandOPEX <- readRDS(file.path(folderPath, "2_InputDataPolicy", "combinedCAPEXandOPEX.RDS"))
    data$enIntensity <- readRDS(file.path(folderPath, "2_InputDataPolicy", "enIntensity.RDS"))
    data$loadFactor <- readRDS(file.path(folderPath, "2_InputDataPolicy", "loadFactor.RDS"))
    data$fleetSizeAndComposition <- readRDS(file.path(folderPath, "4_Output", "fleetSizeAndComposition.RDS"))
    data$ESdemandFVsalesLevel <- readRDS(file.path(folderPath, "4_Output", "ESdemandFVsalesLevel.RDS"))

    # load files for standard and extended transport reporting
    if (isTransportReported) {
      data$upfrontCAPEXtrackedFleet <- readRDS(file.path(folderPath, "2_InputDataPolicy",
                                                         "upfrontCAPEXtrackedFleet.RDS"))
      data$population <- readRDS(file.path(folderPath, "1_InputDataRaw",
                                           "population.RDS"))
      data$GDPppp <- readRDS(file.path(folderPath, "1_InputDataRaw",
                                           "GDPppp.RDS"))
      gdxPath <- list.files(path = folderPath, pattern = "\\.gdx$", full.names = TRUE)
      # Check if any files were found
      if (length(gdxPath) > 1) {
        gdxPath <- gdxPath[1]
        cat("More than one gdx file found. The following one was chosen\n")
        cat(gdxPath, sep = "\n")
      } else if (length(gdxPath) == 0) {
        stop("No gdx files found in the specified directory.\n")
      }
      data$gdxPath <- gdxPath
    }
    if (isAnalyticsReported) {
      # load files for analytic purposes
      fleetFilesIterations <- list.files(path    = file.path(folderPath, "4_Output"),
                                         pattern = "fleetVehNumbersIteration.*", full.names = TRUE)
      if (length(fleetFilesIterations) > 0) {
       data$fleetVehNumbersIterations <- lapply(fleetFilesIterations, readRDS)
      }
      endogenousCostFilesIterations <- list.files(path       = file.path(folderPath, "4_Output"),
                                                  pattern    = "endogenousCostsIteration.*",
                                                  full.names = TRUE)
      if (length(endogenousCostFilesIterations) > 0) {
        data$endogenousCostsIterations <- lapply(endogenousCostFilesIterations, readRDS)
      }
    }
    if (isREMINDinputReported) {
      # load files for REMIND input data only reporting
      data$annualMileage <- readRDS(file.path(folderPath, "1_InputDataRaw", "annualMileage.RDS"))
      data$timeValueCosts <- readRDS(file.path(folderPath, "1_InputDataRaw", "timeValueCosts.RDS"))
      data$prefTrends <- readRDS(file.path(folderPath, "2_InputDataPolicy", "prefTrends.RDS"))
      data$initialIncoCosts <- readRDS(file.path(folderPath, "2_InputDataPolicy", "initialIncoCosts.RDS"))
    }
  } else {
    data <- data
  }
  #########################################################################
  ## Report output variables
  #########################################################################

  # Base variable set that is needed to report REMIND input data and additional detailed transport data
  baseVarSet <- reportBaseVarSet(data = data, timeResReporting = timeResReporting)
  outputVars <- baseVarSet

  if (isTransportReported) {
    transportVarSet <- reportTransportVarSet(data             = data,
                                             baseVarSet       = baseVarSet,
                                             timeResReporting = timeResReporting)
    outputVars <- transportVarSet
    if (isTransportExtendedReported) {
      extendedTransportVarSet <- reportExtendedTransportVarSet(data             = data,
                                                               baseVarSet       = baseVarSet,
                                                               timeResReporting = timeResReporting)
      outputVars$ext <- append(outputVars$ext, extendedTransportVarSet$ext)
      outputVars$int <- append(outputVars$int, extendedTransportVarSet$int)
    }
    if (isAnalyticsReported) {
      if (!is.null(data$endogenousCostsIterations)) {
        analyticsVarSet <- reportAnalyticsVarSet(data = data, timeResReporting = timeResReporting)
        outputVars$analytic <- analyticsVarSet
      } else {
        message("Analytics data not stored in the run folder. Analytics reporting is skipped.")
      }

    }
  }

  #########################################################################
  ## Transfer output variables to MIF format
  #########################################################################
  if (isTransportReported) {
    reporting <- convertToMIF(vars                        = outputVars,
                              GDPMER                      = data$GDPMER,
                              helpers                     = data$helpers,
                              scenario                    = paste0(data$transportPolScen, " ", data$SSPscen),
                              model                       = "EDGE-T",
                              gdx                         = data$gdxPath,
                              isTransportExtendedReported = isTransportExtendedReported)

    if (isStored) write.mif(reporting, file.path(folderPath, "Transport.mif"))
  }

  #########################################################################
  ## Report REMIND input data
  #########################################################################
  if (isREMINDinputReported) {                                                                                              # nolint: object_name_linter
    REMINDinputData <- reportREMINDinputVarSet(fleetESdemand        = baseVarSet$ext$fleetESdemand,                     # nolint: object_name_linter
                                               fleetFEdemand        = baseVarSet$ext$fleetFEdemand,
                                               fleetEnergyIntensity = baseVarSet$int$fleetEnergyIntensity,
                                               fleetCapCosts        = baseVarSet$int$fleetCost[variable == "Capital costs"],
                                               combinedCAPEXandOPEX = data$combinedCAPEXandOPEX,
                                               scenSpecLoadFactor   = data$loadFactor,
                                               scenSpecPrefTrends   = data$prefTrends,
                                               scenSpecEnIntensity  = data$enIntensity,
                                               initialIncoCosts     = data$initialIncoCosts,
                                               annualMileage        = data$annualMileage,
                                               timeValueCosts       = data$timeValueCosts,
                                               hybridElecShare      = data$hybridElecShare,
                                               demScen              = data$demScen,
                                               SSPscen              = data$SSPscen,
                                               transportPolScen     = data$transportPolScen,
                                               timeResReporting     = timeResReporting,
                                               helpers              = data$helpers)

    reporting <- REMINDinputData
    if (isStored) storeData(outputFolder = folderPath, REMINDinputData = REMINDinputData)
  }

  return(reporting)
}
