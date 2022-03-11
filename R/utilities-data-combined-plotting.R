#' Extract "observed versus predicted" dataframe from `DataCombined` object
#'
#' @param dataCombined A single instance of `DataCombined` class.
#'
#' @import dplyr
#'
#' @export
dataCombinedToObsVsPredDataFrame <- function(dataCombined) {
  # Validate both the object and the dataframe it returns
  df <- .validateDataCombinedDataFrame(dataCombined)

  # Unlike the dataframe returned by `$toDataFrame()` method, this dataframe
  # needs to be in wide - and not tidy - format because the `xValues` for
  # observed versus simulated datasets need to be paired for plotting.
  #
  # Every plotting library function that creates a scatterplot expects the
  # data to be in wide format, with `x`-and `y`-axes variables represented by
  # *individual* columns.
  #
  # But the issue is that if both of these wide dataframes need to be joined at
  # some point, they should also have unique names, otherwise they will be
  # stacked on top of each other by joining functions. Therefore, it is
  # essential to provide a unique column naming pattern for each *type* of data.
  obsData <- df %>%
    dplyr::filter(dataType == "observed") %>%
    dplyr::rename_with(~ paste0(.x, "Observed"), -dplyr::matches("^group$"))

  simData <- df %>%
    dplyr::filter(dataType == "simulated") %>%
    dplyr::rename_with(~ paste0(.x, "Predicted"), -dplyr::matches("^group$"))

  # There are two steps to pairing observed and simulated data:
  #
  # Step-1: Look for an exact match between time measurements.
  # Step-2: If there is no match, interpolate value by averaging the current and
  # the previous value.
  #
  # *WARNING*:
  #
  # While looking for an exact match, don't set tolerance to `0` or to
  # `.Machine$double.eps`. Why?
  #
  # dplyr::near(sqrt(2) ^ 2, 2, .Machine$double.eps^0.5) returns `TRUE`
  # dplyr::near(sqrt(2) ^ 2, 2, .Machine$double.eps) returns `FALSE`
  # dplyr::near(sqrt(2) ^ 2, 2, 0) returns `FALSE`
  #
  # Default chosen by {dplyr} is quite sensible.
  pairedData <- dplyr::left_join(obsData, simData, by = c("group")) %>%
    dplyr::group_by(group) %>%
    dplyr::arrange(xValuesObserved, xValuesPredicted) %>%
    dplyr::mutate(
      yValuesPredicted = dplyr::case_when(
        # exact match
        dplyr::near(xValuesObserved, xValuesPredicted) ~ yValuesPredicted,
        # interpolation otherwise
        TRUE ~ (yValuesPredicted + dplyr::lag(yValuesPredicted)) / 2
      )
    ) %>%
    dplyr::ungroup()

  # Joining data above creates multiple combinations, which are spurious and only
  # the first observation should be selected.
  pairedData <- pairedData %>%
    dplyr::group_by(group, xValuesObserved) %>%
    dplyr::slice_sample(n = 1L) %>%
    dplyr::ungroup()

  # Not all columns are either needed or useful for the user (at least in the
  # plotting context), and so reorder and clean paired dataframe accordingly
  pairedData %>%
    # Remove columns where *all* rows are `NA`s (empty columns, i.e.)
    dplyr::select(where(~ !all(is.na(.x)))) %>%
    dplyr::select(
      # Keep important details needed for plotting upfront
      "group",
      dplyr::matches("xValues", ignore.case = FALSE),
      dplyr::matches("yValues", ignore.case = FALSE),
      # All other relevant details after that
      dplyr::matches("unit|dimension|error"),
      dplyr::matches("name")
    )
}

#' Validating `DataCombined` object for creating paired dataframe
#'
#' @details
#'
#' Function checks that the dataframe returned by `$toDataFrame()` method meet
#' the following criteria:
#'
#' - entered object is an instance of `DataCombined` class
#' - the combined dataframe is not empty
#' - there are valid groupings present in the combined dataframe
#' - both simulated and observed datasets are present to be paired
#'
#' Additionally, it modifies the combined dataframe by removing rows (read
#' datasets) that don't belong to any group since such datasets can't be paired.
#'
#' @keywords internal
#' @noRd
.validateDataCombinedDataFrame <- function(dataCombined) {

  # Object validation ----------------

  validateIsOfType(dataCombined, "DataCombined", nullAllowed = FALSE)
  validateIsSameLength(objCount(dataCombined), 1L)

  # Dataframe validation -------------

  df <- dataCombined$toDataFrame()

  if (is.null(df)) {
    stop("There are currently no datasets in `DataCombined` object. You can add them with `$addDataSets()` and/or `$addSimulationResults()` methods.")
  }

  # Datasets which are not part of any grouping can't be paired
  df <- df %>% dplyr::filter(!is.na(group))

  if (length(unique(df$group)) == 0L) {
    stop("There are currently no grouping specified in `DataCombined` object. You can set them with `$setGroups()` method.")
  }

  if (length(unique(df$dataType)) == 1L && unique(df$dataType) == "simulated") {
    stop("Both observed and simulated data needed, but only simulated data is present in `DataCombined` object.")
  }

  if (length(unique(df$dataType)) == 1L && unique(df$dataType) == "observed") {
    stop("Both observed and simulated data needed, but only observed data is present in `DataCombined` object.")
  }

  return(df)
}
