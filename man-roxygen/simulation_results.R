#' @param simulationResults Object of type `SimulationResults` or a list of
#'   `SimulationResults` objects produced by calling `runSimulations` on a
#'   `Simulation` object. When a list is provided, the function processes each
#'   result and returns a named list of outputs.
#' @param quantitiesOrPaths Quantity instances (element or vector) typically
#'   retrieved using `getAllQuantitiesMatching` or quantity path (element or
#'   vector of strings) for which the results are to be returned. (optional)
#'   When providing the paths, only absolute full paths are supported (i.e., no
#'   matching with '*' possible). If quantitiesOrPaths is `NULL` (default
#'   value), returns the results for all output defined in the results.
#' @param population population used to calculate the `simulationResults`
#'   (optional). This is used only to add the population covariates to the
#'   resulting data table.
#' @param individualIds `numeric` IDs of individuals for which the results
#'   should be extracted. By default, all individuals from the results are
#'   considered. If the individual with the provided ID is not found, the ID is
#'   ignored.
