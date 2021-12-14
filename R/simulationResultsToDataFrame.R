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
#'
#' @return
#'
#' SimulationResults object as data.frame with columns IndividualId, Time, paths,
#' simulationValues, unit, dimension, TimeUnit, TimeDimension.
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
    population = population,
    individualIds = individualIds
  )

  # convert data to long format with a new column for paths
  df_data <- simList$data %>%
    tidyr::pivot_longer(
      cols = -c("IndividualId", "Time"),
      names_to = "paths",
      values_to = "simulationValues"
    )

  # extract units and dimensions for paths in a separate dataframe
  # leave out time units and dimensions since it is not a path
  df_meta <- purrr::imap_dfr(
    .x  = simList$metaData,
    .f  = ~as.data.frame(.x, row.names = NULL),
    .id = "paths"
  ) %>%
    dplyr::filter(paths != "Time")

  # combine these dataframes
  df <- dplyr::left_join(df_data, df_meta, by = "paths")

  # add time unit and dimnensions now to the combined dataframe
  df <- dplyr::bind_cols(
    df,
    as.data.frame(simList$metaData$Time, col.names = c("TimeUnit", "TimeDimension"))
  )

  # return the combined dataframe
  return(df)
}
