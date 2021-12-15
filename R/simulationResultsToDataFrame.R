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
    population = population,
    individualIds = individualIds
  )

  # convert data to long format with a new column for paths
  df_data <- .convert_to_long(
    simList$data,
    cols = setdiff(names(simList$data), c("IndividualId", "Time")),
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
  df_meta <- dplyr::filter(df_meta, paths != "Time")

  # combine dataframe with simulated data and meta data
  df <- dplyr::left_join(df_data, df_meta, by = "paths")

  # add time unit to the this combined dataframe
  # the time dimension is "Time", which is redundant and is thus left out
  df <- dplyr::bind_cols(
    df,
    data.frame("TimeUnit" = simList$metaData$Time$unit[[1]])
  )

  # return the combined dataframe
  return(df)
}

# custom function to convert dataframe from wide to long format
# simplified version of
# https://github.com/easystats/datawizard/blob/master/R/data_reshape.R
# IP is author on this package

.convert_to_long <- function(data,
                             cols = "all",
                             names_to = "Name",
                             values_to = "Value") {
  if (is.character(cols) && length(cols) == 1) {
    if (cols == "all") {
      # If all, take all
      cols <- names(data)
    } else {
      # Surely, a regex
      cols <- grep(cols, names(data), value = TRUE)
    }
  }

  # If numeric, surely the index of the cols
  if (is.numeric(cols)) {
    cols <- names(data)[cols]
  }

  # Compatibility with tidyr
  if (names_to != names_to) names_to <- names_to

  # save attribute of each variable
  variable_attr <- lapply(data, attributes)

  # Create Index column as needed by reshape
  data[["_Row"]] <- as.numeric(row.names(data))

  # Reshape
  long <- stats::reshape(data,
    varying = cols,
    idvar = "_Row",
    v.names = values_to,
    timevar = names_to,
    direction = "long"
  )

  # Sort the dataframe (to match pivot_longer's output)
  long <- long[order(long[["_Row"]], long[[names_to]]), ]

  # Remove or rename the row index
  long[["_Row"]] <- NULL

  # Re-insert col names as levels
  long[[names_to]] <- cols[long[[names_to]]]

  # Reset row names
  row.names(long) <- NULL

  # Remove reshape attributes
  attributes(long)$reshapeLong <- NULL

  # add back attributes where possible
  for (i in colnames(long)) {
    attributes(long[[i]]) <- variable_attr[[i]]
  }

  long
}
