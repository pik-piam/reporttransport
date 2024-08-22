#' Harmonize data to last REMIND iteration in coupled mode
#'
#' The two models are coupled via energy service demand, energy intensity and capital expenditure on CES node level.
#' As REMIND converges the output between EDGE-T and REMIND still slightly differs.
#' This function calculates harmonization factors that are applied on the EDGE-T energy intensity and final energy values.
#' Furthermore, the harmonization factors indicate how well the two models converged and can be used for analysis purposes.
#'
#' @param edgetOutputDir edgeTransport output directory
#' @param baseVarSet calculated base variable set including energy intensity and final energy on fleet level
#' @param data list of raw model output data
#'
#' @returns The function returns a list with the harmonized energy intensity and final energy data
#' @author Johanna Hoppe
#' @importFrom quitte read.quitte
#' @importFrom tibble tribble
#' @import data.table
#' @export

harmonizeOutput <- function(REMINDoutput, edgetOutputDir, baseVarSet, data) {

    ## Calculate FE harmonization factors that are then applied to the energy intensity
    harmMap <- tribble(
      ~variable,                                                  ~ all_teEs,
      "FE|Transport|Freight|Short-Medium distance|Electricity",    "te_eselt_frgt_sm",
      "FE|Transport|Freight|Short-Medium distance|Liquids",        "te_esdie_frgt_sm",
      "FE|Transport|Freight|Short-Medium distance|Hydrogen",       "te_esh2t_frgt_sm",
      "FE|Transport|Freight|Short-Medium distance|Gases",          "te_esgat_frgt_sm",
      "FE|Transport|Freight|Long distance|Diesel Liquids",         "te_esdie_frgt_lo",
      "FE|Transport|Pass|Short-Medium distance|Electricity",       "te_eselt_pass_sm",
      "FE|Transport|Pass|Short-Medium distance|Diesel Liquids",    "te_esdie_pass_sm",
      "FE|Transport|Pass|Short-Medium distance|Petrol Liquids",    "te_espet_pass_sm",
      "FE|Transport|Pass|Short-Medium distance|Hydrogen",          "te_esh2t_pass_sm",
      "FE|Transport|Pass|Short-Medium distance|Gases",             "te_esgat_pass_sm",
      "FE|Transport|Pass|Long distance|Diesel Liquids",            "te_esdie_pass_lo"
    )
    REMINDoutputToHarmonize <- REMINDoutput[variable %in% unique(harmMap$variable)]
    if (!length(unique(REMINDoutputToHarmonize$variable)) == length(unique(harmMap$variable))) {
      stop("Variables for EDGE-T harmonization are missing in the REMIND output. Please check reportEdgeTransport()")
    }
    demByTechMap <- unique(data$helpers$mapEdgeToREMIND[, c("all_teEs", "univocalName", "technology")])
    demByTechMap <- demByTechMap[!is.na(all_teEs)]
    demByTechMap[technology %in% c("BEV", "Electric"), technology := "Electricity"]
    demByTechMap[technology == "FCEV", technology := "Hydrogen"]
    EDGEToutputToHarmonize <- merge(baseVarSet$ext$fleetFEdemand[period %in% timeResReporting], demByTechMap, by = c("univocalName", "technology"), all.x = TRUE) # nolint: object_name_linter
    EDGEToutputToHarmonize <- EDGEToutputToHarmonize[period %in% timeResReporting & !subsectorL1 %in% c("Walk", "Cycle")]
    EDGEToutputToHarmonize[, variable := NULL][, unit := NULL]
    EDGEToutputToHarmonize <- merge(EDGEToutputToHarmonize, harmMap, by = "all_teEs", all.x = TRUE)
    EDGEToutputToHarmonize <- EDGEToutputToHarmonize[, .(value = sum(value)), by = c("region", "period", "variable", "all_teEs")]
    setnames(EDGEToutputToHarmonize, "value", "edget")
    setnames(REMINDoutputToHarmonize, "value", "remind")
    harmFactors <- merge(EDGEToutputToHarmonize, REMINDoutputToHarmonize, by = c("region", "period", "variable"), all.x = TRUE)
    harmFactors[, factor := ifelse(!remind < 1e-6 & !edget < 1e-6, remind/edget, 1)][, c("edget", "remind") := NULL]
    harmFactors[, variable := paste0("Harmonization factor|", variable)][, unit := "-"][, scenario:= data$scenarioName][, model:= data$modelName]
    storeData(edgetOutputDir, harmFactors)

    #Hybrids are listed in energy intensity but are attributed partly to electricity and petrol in the reporting of final energy
    #The liquids part of the final energy needs to be scaled differntly than the electricity part. This is not possible using the current approach.
    #Hence, the energy intensity of hybrids is not scaled and the FE is scaled in postprocessing. This leads to a small inconsistency in the dataset
    #of eneryg service demand, energy intensity and final energy

    #Apply factors
    harmonizedEnergyIntensity <- merge(baseVarSet$int$fleetEnergyIntensity[period %in% timeResReporting], 
      data$helpers$mapEdgeToREMIND[, c("all_teEs", "univocalName", "technology")], by = c("univocalName", "technology"), all.x = TRUE)
    harmonizedEnergyIntensity <- merge(harmonizedEnergyIntensity, harmFactors[, c("region", "period", "all_teEs", "factor")], 
      by = c("region", "period", "all_teEs"), all.x = TRUE)
    harmonizedEnergyIntensity[technology == "Hybrid electric", factor := 1]
    harmonizedEnergyIntensity[, value := ifelse(!subsectorL1 %in% c("Walk", "Cycle"), value * factor, value)][, c("factor", "all_teEs") := NULL]
    if (anyNA(harmonizedEnergyIntensity)) stop("Variable harmonization did not work. Please check harmonizeOutput() in reporttransport.")

    ## Harmonize final energy
    harmonizedFinalEnergy <- merge(baseVarSet$ext$fleetFEdemand[period %in% timeResReporting], 
      demByTechMap, by = c("univocalName", "technology"), all.x = TRUE)
    harmonizedFinalEnergy <- merge(harmonizedFinalEnergy, harmFactors[, c("region", "period", "all_teEs", "factor")], 
      by = c("region", "period", "all_teEs"), all.x = TRUE)
    harmonizedFinalEnergy[, value := ifelse(!subsectorL1 %in% c("Walk", "Cycle"), value * factor, value)]
    FEcheck <- harmonizedFinalEnergy[!subsectorL1 %in% c("Walk", "Cycle")]
    harmonizedFinalEnergy[, c("factor", "all_teEs") := NULL]    
    if (anyNA(harmonizedFinalEnergy)) stop("Variable harmonization did not work. Please check harmonizeOutput() in reporttransport.") 
    
    ## Check if harmonization worked and how big the deviation due to the hybrids is
    # test harmonization
    FEcheck <- FEcheck[, .(value = sum(value)), by = c("region", "period", "all_teEs")]
    FEcheck <- merge(FEcheck, harmMap, by = "all_teEs", all.x = TRUE)
    FEcheck <- merge(FEcheck, REMINDoutputToHarmonize, by = c("region", "period", "variable"), all.x = TRUE)
    FEcheck[, diff := ifelse(!remind < 1e-6 & !value < 1e-6, (value - remind) / value, 0)]
    if (max(FEcheck$diff > 1e-5)) stop("FE harmonization did not work. Please check harmonizeOutput()")
    
    # test the deviation (as if the dataset would be consistent)
    calculatedFinalEnergy <- reportFinalEnergy(harmonizedEnergyIntensity, baseVarSet$ext$fleetESdemand, data$scenSpecLoadFactor, data$hybridElecShare, data$helpers)
    test <- calculatedFinalEnergy[period %in% timeResReporting & !subsectorL1 %in% c("Walk", "Cycle")]
    test[, variable := NULL][, unit := NULL]
    test <- merge(test, demByTechMap, by = c("univocalName", "technology"), all.x = TRUE)
    test <- merge(test, harmMap, by = "all_teEs", all.x = TRUE)
    test <- test[, .(value = sum(value)), by = c("region", "period", "variable")]
    test <- merge(test, REMINDoutputToHarmonize, by = c("region", "period", "variable"), all.x = TRUE)
    test[, diff := ifelse(!remind < 1e-6 & !value < 1e-6, (value - remind) / value, 0)] 

    harmonizedVars <- list(
      harmonizedEnergyIntensity = harmonizedEnergyIntensity,
      harmonizedFinalEnergy = harmonizedFinalEnergy
    )

    return(harmonizedVars)
}
