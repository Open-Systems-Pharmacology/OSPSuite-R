#' Convert to common units
#'
#' @description
#'
#' When multiple (observed and/or simulated) datasets are present in a data
#' frame, they are likely to have different units. This function helps to
#' convert them to a common unit specified by the user.
#'
#' This is especially helpful while plotting since the quantities from different
#' datasets to be plotted on the x-and y-axis need to have same units to be
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
#'   molWeight = c(12.5, 10, 5)
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

  # The observed and simulated data should have same units.
  #
  # Therefore, if target units are not specified, we need to choose one for
  # consistency. For no special reason, first element from a vector of unique
  # units will be selected.
  targetXUnit <- xUnit %||% unique(data$xUnit)[[1]]
  targetYUnit <- yUnit %||% unique(data$yUnit)[[1]]

  xList <- split(data, data$xUnit)
  data <- purrr::map_dfr(xList, ~.groupwiseXConvert(.x, targetXUnit))

  yList <- split(data, data$yUnit)
  data <- purrr::map_dfr(yList, ~.groupwiseYConvert(.x, targetYUnit))

  return(data)
}

# helpers ---------------------------

#' @keywords internal
#' @noRd
.groupwiseXConvert <- function(data, targetUnit) {
  dimension <- data$xDimension[[1]]
  sourceUnit <- data$xUnit[[1]]

  data %>%
    dplyr::mutate(
      xValues = toUnit(
        quantityOrDimension = dimension,
        values              = xValues,
        targetUnit          = targetUnit,
        sourceUnit          = sourceUnit
      ),
      xUnit = targetUnit
    )
}

#' @keywords internal
#' @noRd
.groupwiseYConvert <- function(data, targetUnit) {
  dimension <- data$yDimension[[1]]
  sourceUnit <- data$yUnit[[1]]
  molecularWeight <- data$molWeight[[1]]

  data %>%
    dplyr::mutate(
      yValues = toUnit(
        quantityOrDimension = dimension,
        values              = yValues,
        targetUnit          = targetUnit,
        sourceUnit          = sourceUnit,
        molWeight           = molecularWeight,
        molWeightUnit       = ospUnits$`Molecular weight`$`g/mol`
      ),
      yUnit = targetUnit
    )
}
