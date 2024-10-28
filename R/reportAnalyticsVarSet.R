#'Report analytics variable set
#'
#' @param data List that contains the model results to report the analytics variable set
#' @param timeResReporting Timesteps to be reported
#'
#' @returns Analytics variable set
#' @author Johanna Hoppe
#' @import data.table
#' @export

reportAnalyticsVarSet <- function(data, timeResReporting) {
  updatedEndogenousCosts <- list()
  policyMask <- list()
  rawEndogenousCost <- list()
  allCostsFV <- list()
  allCostsVS3 <- list()
  allCostsS3S2 <- list()
  allCostsS2S1 <- list()
  allCostsS1S <- list()

  endogenousCostsData <- data[names(data)[grepl("endogenousCostsIteration.*", names(data))]]
  for (i in 1:length(endogenousCostsData)) {
    updatedEndogenousCosts[[i]] <- endogenousCostsData[[i]]$updatedEndogenousCosts
    policyMask[[i]] <- endogenousCostsData[[i]]$policyMask
    rawEndogenousCost[[i]] <- endogenousCostsData[[i]]$rawEndogenousCosts
  }

  updatedEndogenousCosts <- rbindlist(updatedEndogenousCosts)
  policyMask <- rbindlist(policyMask)
  rawEndogenousCost <- rbindlist(rawEndogenousCost)
  fleetVehNumbersIterations <- rbindlist(data[names(data)[grepl("fleetVehNumbersIteration[0-9]+", names(data))]])
  costsDiscreteChoiceData <- data[names(data)[grepl("costsDiscreteChoiceIteration[0-9]+", names(data))]]
  cols <- names(data$helpers$decisionTree)
  for (i in 1:length(costsDiscreteChoiceData)) {
    lapply(costsDiscreteChoiceData[[i]], function(dt, cols){
      colsdt <- names(dt)
      missingCols <- cols[!cols %in% intersect(colsdt, cols)]
      if (!length(missingCols) == 0) dt[, eval(missingCols) := ""]
    }, cols)
    allCostsFV[[i]] <- costsDiscreteChoiceData[[i]]$allCostsFV
    allCostsVS3[[i]] <- costsDiscreteChoiceData[[i]]$allCostsVS3
    allCostsS3S2[[i]] <- costsDiscreteChoiceData[[i]]$allCostsS3S2
    allCostsS2S1[[i]] <- costsDiscreteChoiceData[[i]]$allCostsS2S1
    allCostsS1S[[i]] <- costsDiscreteChoiceData[[i]]$allCostsS1S
  }
  analyticsData <- list(updatedEndogenousCosts, policyMask, rawEndogenousCost, fleetVehNumbersIterations, allCostsFV,
                        allCostsVS3, allCostsS3S2, allCostsS2S1, allCostsS1S)

  return(analyticsData)
}
