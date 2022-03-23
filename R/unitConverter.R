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
  xTargetUnit <- xUnit %||% unique(data$xUnit)[[1]]
  yTargetUnit <- yUnit %||% unique(data$yUnit)[[1]]

  data %>%
    dplyr::mutate(.rowid = dplyr::row_number()) %>%
    dplyr::mutate(
      xTargetUnit = xTargetUnit,
      yTargetUnit = yTargetUnit
    ) %>%
    tidyr::pivot_longer(
      dplyr::matches("^x|^y"),
      names_pattern  = "([xy])([A-Z].+)",
      names_to       = c("Variable", ".value"),
      values_to      = "Values",
      values_drop_na = TRUE
    ) %>%
    tidyr::nest(data = c(Values)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Values = list(
      toUnit(
        quantityOrDimension = Dimension,
        values              = purrr::pluck(data, 1L),
        targetUnit          = TargetUnit,
        sourceUnit          = Unit,
        molWeight           = molWeight,
        molWeightUnit       = ospUnits$`Molecular weight`$`g/mol`
      )
    )) %>%
    dplyr::ungroup() %>%
    tidyr::unnest(cols = c(Values)) %>%
    dplyr::select(-data, -Unit) %>%
    dplyr::rename(Unit = TargetUnit) %>%
    tidyr::pivot_wider(
      names_from  = Variable,
      values_from = dplyr::matches("Values|Unit|Dimension"),
      names_glue  = "{Variable}{.value}"
    ) %>%
    dplyr::select(
      dplyr::matches("Values|Unit|Dimension"),
      dplyr::everything(),
      -c(".rowid")
    )
}
