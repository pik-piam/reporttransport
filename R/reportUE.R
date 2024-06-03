#'Report variables in relation to the vehicle fleet.
#'
#' @param FEdemand Finale energy demand
#' @param helpers List with helpers
#'
#' @returns Useful energy demand
#' @author Johanna Hoppe
#' @import data.table
#' @export

reportUE <- function(FEdemand, helpers) {                                                                               # nolint: object_name_linter

  value <- variable <- UEefficiency <- univocalName <- NULL

  # Note that this is a really rough assumptions (as the aircarft burning hydrogen is getting the same efficiency than
  # a fuel cell electric truck)
  MappUE <- data.table(technology = c("Electricity", "Hydrogen", "Liquids", "Gases"),                                       # nolint: object_name_linter
                       UEefficiency = c(0.64, 0.25, 0.23, 0.23))
  UEdemand <- copy(FEdemand)                                                                                                # nolint: object_name_linter
  UEdemand <- merge(UEdemand, MappUE, by = "technology")                                                                    # nolint: object_name_linter
  UEdemand[univocalName %in% c("Cycle", "Walk"), UEefficiency := 1]
  UEdemand[, value := value * UEefficiency][, UEefficiency := NULL]
  UEdemand[, variable := "UE"]

  return(UEdemand)
}
