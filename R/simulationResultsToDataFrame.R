#' Converts a `SimulationResults` objects to a data.frame
#'
#' @inheritParams getOutputValues
#'
#' @examples
#' library(ospsuite)
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#'
#' # Running an individual simulation
#' # results is an instance of `SimulationResults`
#' results <- runSimulations(sim)
#'
#' # convert to a dataframe
#' simulationResultsToDataFrame(results)
#' @return
#'
#' SimulationResults object as data.frame with columns IndividualId, Time, paths,
#' simulationValues, unit, dimension, TimeUnit.
#'
#' @export

simulationResultsToDataFrame <- function(simulationResults,
                                         quantitiesOrPaths = NULL,
                                         population = NULL,
                                         individualIds = NULL) {
  # no need to validating the input because this will be done by
  # getOutputValues() anyways
  simList <- getOutputValues(
    simulationResults = simulationResults,
    quantitiesOrPaths = quantitiesOrPaths,
    population        = population,
    individualIds     = individualIds
  )

  # convert data to long format with a new column for paths
  df_data <- tidyr::pivot_longer(
    simList$data,
    cols = -c("IndividualId", "Time"),
    names_to = "paths",
    values_to = "simulationValues"
  )

  # extract units and dimensions for paths in a separate dataframe
  # iterate over the list (in .x) using index (names for list elements)
  # and apply function in .f to each elements
  # the result will be a list of dataframes, which will be bound into
  # a single dataframe with the _dfr variant of this function
  df_meta <- purrr::imap_dfr(
    .x  = simList$metaData,
    .f  = ~ as.data.frame(.x, row.names = NULL),
    .id = "paths"
  )

  # leave out time units and dimensions since it is not a path
  # they will be added at a later stage
  df_meta <- dplyr::filter(df_meta, paths != "Time")

  # combine dataframe with simulated data and meta data
  df <- dplyr::left_join(df_data, df_meta, by = "paths")

  # now add previously left out time meta data to the combined dataframe
  df <- dplyr::bind_cols(
    df,
    data.frame(
      "TimeUnit" = simList$metaData$Time$unit[[1]],
      "TimeDimension" = simList$metaData$Time$dimension[[1]]
    )
  )

  # if molecular weight is not present, extract it and add as a new column
  if (!"molWeight" %in% names(df)) {
    # For each path, extract the molecular weight based on that path string
    #
    # This involves first grouping and nesting the data by path. Note that
    # `nest()` here will have a better performance than `rowwise()`. E.g., if
    # there are 100 rows, `rowwise()` will run the compuation 100 times, while
    # with `nest()`, the computation only be carried for the same number of
    # times as the number of `paths` present.
    #
    # And then adding a new column for molecular weight.
    #
    # When you call `molWeightFor()`, it returns the value in the base unit -
    # which is `kg/µmol`. This is not the unit the user would expect, so we
    # convert it first to the common unit `g/mol`
    df <- df %>%
      dplyr::group_by(paths) %>%
      tidyr::nest() %>%
      dplyr::mutate(
        molWeight = ospsuite::toUnit(
          quantityOrDimension = ospDimensions$`Molecular weight`,
          values              = simulationResults$simulation$molWeightFor(paths),
          targetUnit          = ospUnits$`Molecular weight`$`g/mol`
        )
      ) %>%
      tidyr::unnest(cols = c(data)) %>%
      dplyr::ungroup()
  }

  # return the combined dataframe
  return(df)
}
