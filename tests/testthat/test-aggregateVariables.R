test_that("variable aggregation works", {

  testSummation <- function(aggrname, datatable, varlist){
    vars <- varlist[[aggrname]]
    #access the first element in vars
    if (length(unique(datatable[variable %in% vars]$variable)) < length(vars)){
      print(paste0("Missing variables in aggregation checks ", aggrname))
    } else if (length(unique(datatable[variable %in% vars]$variable)) > length(vars)) {
      print(paste0('Duplicate in aggregation check: ', aggrname, 'not summed up'))
    } else {datatable <- datatable[variable %in% vars,
                                   .(variable = aggrname,
                                     value = sum(value)),
                                   by = c("region", "period", "unit")]}
    return(datatable)
  }

 load(test_path("testVariableAggregation.RData"))

 # Check summation for extensive variables "ES" and "FE" by example
 extensiveTestVars <- aggregateVariables(vars[variable %in% c("ES", "FE")],
                                                   mapAggregation)
 # Examples
 varsl <- list(
   `FE|Transport|Pass with bunkers` = c("FE|Transport|Bunkers|Pass|International Aviation",
                                        "FE|Transport|Pass|Domestic Aviation",
                                        "FE|Transport|Pass|Rail|HSR",
                                        "FE|Transport|Pass|Rail|non-HSR",
                                        "FE|Transport|Pass|Road|Bus",
                                        "FE|Transport|Pass|Road|LDV|Four Wheelers",
                                        "FE|Transport|Pass|Road|LDV|Three Wheelers",
                                        "FE|Transport|Pass|Road|LDV|Two Wheelers"),
   `FE|Transport|Pass` = c("FE|Transport|Pass|Domestic Aviation",
                           "FE|Transport|Pass|Rail|HSR",
                           "FE|Transport|Pass|Rail|non-HSR",
                           "FE|Transport|Pass|Road|Bus",
                           "FE|Transport|Pass|Road|LDV|Four Wheelers",
                            "FE|Transport|Pass|Road|LDV|Three Wheelers",
                           "FE|Transport|Pass|Road|LDV|Two Wheelers"),
   `FE|Transport|Freight with bunkers` = c("FE|Transport|Bunkers|Freight|International Shipping",
                                           "FE|Transport|Freight|Domestic Shipping",
                                           "FE|Transport|Freight|Rail",
                                           "FE|Transport|Freight|Road"),
   `FE|Transport|Freight` = c("FE|Transport|Freight|Domestic Shipping",
                              "FE|Transport|Freight|Rail",
                              "FE|Transport|Freight|Road"),
   `FE|Transport` =  c("FE|Transport|Electricity",
                       "FE|Transport|Hydrogen",
                       "FE|Transport|Gases",
                       "FE|Transport|Liquids"),
   `FE|Transport|Bunkers|Freight|International Shipping|Liquids` = c("FE|Transport|Bunkers|Freight|International Shipping|Liquids|Fossil",
                                                                     "FE|Transport|Bunkers|Freight|International Shipping|Liquids|Hydrogen",
                                                                     "FE|Transport|Bunkers|Freight|International Shipping|Liquids|Biomass"),
   `ES|Transport|Pass with bunkers` = c("ES|Transport|Bunkers|Pass|International Aviation",
                                        "ES|Transport|Pass|Domestic Aviation",
                                        "ES|Transport|Pass|Rail|HSR",
                                        "ES|Transport|Pass|Rail|non-HSR",
                                        "ES|Transport|Pass|Road|Bus",
                                        "ES|Transport|Pass|Road|LDV|Four Wheelers",
                                        "ES|Transport|Pass|Road|LDV|Three Wheelers",
                                        "ES|Transport|Pass|Road|LDV|Two Wheelers",
                                        "ES|Transport|Pass|Non-motorized|Walk",
                                        "ES|Transport|Pass|Non-motorized|Cycle"),
   `ES|Transport|Pass` = c("ES|Transport|Pass|Domestic Aviation",
                           "ES|Transport|Pass|Rail|HSR",
                           "ES|Transport|Pass|Rail|non-HSR",
                           "ES|Transport|Pass|Road|Bus",
                           "ES|Transport|Pass|Road|LDV|Four Wheelers",
                            "ES|Transport|Pass|Road|LDV|Three Wheelers",
                           "ES|Transport|Pass|Road|LDV|Two Wheelers",
                           "ES|Transport|Pass|Non-motorized|Walk",
                           "ES|Transport|Pass|Non-motorized|Cycle"),
   `ES|Transport|Freight with bunkers` = c("ES|Transport|Bunkers|Freight|International Shipping",
                                           "ES|Transport|Freight|Domestic Shipping",
                                           "ES|Transport|Freight|Rail",
                                           "ES|Transport|Freight|Road"),
   `ES|Transport|Freight` = c("ES|Transport|Freight|Domestic Shipping",
                              "ES|Transport|Freight|Rail",
                              "ES|Transport|Freight|Road"))

    summationCheck <- sapply(names(varsl),
                          testSummation,
                          datatable = extensiveTestVars,
                          varlist = varsl,
                          simplify = FALSE,
                          USE.NAMES = TRUE)

    summationCheck <- rbindlist(summationCheck, use.names = TRUE)
    setcolorder(summationCheck, c("region", "variable", "unit", "period", "value"))
    extensiveTestVars <- extensiveTestVars[variable %in% names(varsl)]
    expect_equal(extensiveTestVars,
                 summationCheck,
                 ignore.col.order = TRUE,
                 ignore.row.order = TRUE)

    # Check intensive variable aggregation by example for "Energy intensity|Transport|Pass"
    weight <- vars[variable == "ES"]
    aggregatedEnergyIntensity <- aggregateVariables(vars[variable == "Energy intensity"],
                                                    mapAggregation,
                                                    weight)
    expect_equal(aggregatedEnergyIntensity[variable == "Energy intensity|Transport|Pass"],
                 exampleOutputEnergyIntensity,
                 ignore.col.order = TRUE,
                 ignore.row.order = TRUE)

})
