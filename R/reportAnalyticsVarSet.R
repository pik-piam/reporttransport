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

  for (i in seq_along(data$endogenousCostsIterations)) {
    updatedEndogenousCosts[[i]] <- data$endogenousCostsIterations[[i]]$updatedEndogenousCosts
    policyMask[[i]] <- data$endogenousCostsIterations[[i]]$policyMask
    rawEndogenousCost[[i]] <- data$endogenousCostsIterations[[i]]$rawEndogenousCost
  }

  updatedEndogenousCosts <- rbindlist(updatedEndogenousCosts)
  policyMask <- rbindlist(policyMask)
  rawEndogenousCost <- rbindlist(rawEndogenousCost)
  fleetVehNumbersIterations <- rbindlist(data$fleetVehNumbersIterations)

  analyticsData <- list(updatedEndogenousCosts, policyMask, rawEndogenousCost, fleetVehNumbersIterations)

  return(analyticsData)
}
