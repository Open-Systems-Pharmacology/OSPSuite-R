#' Convert data frame to common units
#'
#' @description
#'
#' When multiple (observed and/or simulated) datasets are present in a data
#' frame, they are likely to have different units. This function helps to
#' convert them to a common unit specified by the user.
#'
#' This is especially helpful while plotting since the quantities from different
#' datasets to be plotted on the X-and Y-axis need to have same units to be
#' meaningfully compared.
#'
#' @return A data frame with measurement columns transformed to have common units.
#'
#' @param data A data frame (or a tibble).
#' @param xUnit,yUnit Target units for `xValues` and `yValues`, respectively. If
#'   not specified (`NULL`), first of the existing units in the respective
#'   columns (`xUnit` and `yUnit`) will be selected as the common unit. For
#'   available dimensions and units, see `ospsuite::ospDimensions` and
#'   `ospsuite::ospUnits`, respectively.
#'
#' @seealso toUnit
#'
#' @examples
#'
#' # small dataframe to illustrate the conversion
#' (df <- dplyr::tibble(
#'   xValues = c(15, 30, 60), xUnit = "min", xDimension = "Time",
#'   yValues = c(0.25, 45, 78), yUnit = c("", "%", "%"), yDimension = c("Fraction", "Fraction", "Fraction"),
#'   molWeight = c(10, 10, 10)
#' ))
#'
#' ospsuite:::.unitConverter(df)
#' ospsuite:::.unitConverter(df, xUnit = ospUnits$Time$h)
#' ospsuite:::.unitConverter(df, yUnit = ospUnits$Fraction$`%`)
#' ospsuite:::.unitConverter(df, xUnit = ospUnits$Time$h, yUnit = ospUnits$Fraction$`%`)
#'
#' @keywords internal
#' @noRd
.unitConverter <- function(data, xUnit = NULL, yUnit = NULL) {
  # No validation of inputs for this non-exported function.
  # All validation will take place in the `DataCombined` class itself.

  # The observed and simulated data should have the same units for
  # visual/graphical comparison.
  #
  # Therefore, if target units are not specified by the user, we need to choose
  # one ourselves. For no special reason, the first element from a vector of
  # unique units will be selected: one for X-axis, and one for Y-axis, i.e.
  xTargetUnit <- xUnit %||% unique(data$xUnit)[[1]]
  yTargetUnit <- yUnit %||% unique(data$yUnit)[[1]]

  # xUnit --------------------------

  xDataList <- split(data, data$xUnit)
  data <- dplyr::bind_rows(lapply(xDataList, .xUnitConverter, xTargetUnit))

  # yUnit ----------------

  yDataList <- split(data, data$yUnit)
  data <- dplyr::bind_rows(lapply(yDataList, .yUnitConverter, yTargetUnit))

  # yUnit error ----------------

  if ("yErrorValues" %in% names(data)) {
    yErrorDataList <- split(data, data$yErrorUnit)
    data <- dplyr::bind_rows(lapply(yErrorDataList, .yErrorUnitConverter, yTargetUnit))
  }

  data
}

#' @keywords internal
#' @noRd
.xUnitConverter <- function(xData, xTargetUnit) {
  xData$xValues <- ospsuite::toUnit(
    quantityOrDimension = xData$xDimension[[1]],
    values = xData$xValues,
    targetUnit = xTargetUnit,
    sourceUnit = xData$xUnit[[1]]
  )

  xData$xUnit <- xTargetUnit

  xData
}

#' @keywords internal
#' @noRd
.yUnitConverter <- function(yData, yTargetUnit) {
  yData$yValues <- toUnit(
    quantityOrDimension = yData$yDimension[[1]],
    values = yData$yValues,
    targetUnit = yTargetUnit,
    sourceUnit = yData$yUnit[[1]],
    molWeight = yData$molWeight[[1]],
    molWeightUnit = ospUnits$`Molecular weight`$`g/mol`
  )

  yData$yUnit <- yTargetUnit

  yData
}

#' @keywords internal
#' @noRd
.yErrorUnitConverter <- function(yData, yTargetUnit) {
  yData$yErrorValues <- toUnit(
    quantityOrDimension = yData$yDimension[[1]],
    values = yData$yErrorValues,
    targetUnit = yTargetUnit,
    sourceUnit = yData$yErrorUnit[[1]],
    molWeight = yData$molWeight[[1]],
    molWeightUnit = ospUnits$`Molecular weight`$`g/mol`
  )

  yData$yErrorUnit <- yTargetUnit

  yData
}
