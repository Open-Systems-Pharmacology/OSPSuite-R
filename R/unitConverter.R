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
#'   not specified (`NULL`), one of the existing units in the respective columns
#'   (`xUnit` and `yUnit`) will be selected as the common unit. For available
#'   dimensions and units, see `ospsuite::ospDimensions` and
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
#' unitConverter(df)
#'
#' unitConverter(df, xUnit = ospUnits$Time$h)
#'
#' unitConverter(df, yUnit = ospUnits$Fraction$`%`)
#'
#' unitConverter(df, xUnit = ospUnits$Time$h, yUnit = ospUnits$Fraction$`%`)
#'
#' @export
unitConverter <- function(data, xUnit = NULL, yUnit = NULL) {
  # validate inputs
  validateIsOfType(data, "data.frame")
  validateIsCharacter(xUnit, nullAllowed = TRUE)
  validateIsCharacter(yUnit, nullAllowed = TRUE)

  # The observed and simulated data should have same units.
  #
  # Therefore, if target units are not specified, we need to choose one for
  # consistency. For no special reason, the first non-missing units will be
  # used.
  targetXUnit <- xUnit %||% unique(data$xUnit)[[1]]
  targetYUnit <- yUnit %||% unique(data$yUnit[!is.na(data$yUnit)])[[1]]

  # *WARNING*: Do not change the order of two `mutate()` statements.
  #
  # Since the old `xUnit` and `yUnit` columns are need for unit conversion,
  # those columns can be updated only after conversions have taken place.
  data %>%
    dplyr::rowwise() %>% # transform values to common units
    dplyr::mutate(
      xValues = toUnit(
        quantityOrDimension = xDimension,
        values              = xValues,
        targetUnit          = targetXUnit,
        sourceUnit          = xUnit
      ),
      yValues = toUnit(
        quantityOrDimension = yDimension,
        values              = yValues,
        targetUnit          = targetYUnit,
        sourceUnit          = yUnit,
        molWeight           = molWeight,
        molWeightUnit       = ospUnits$`Molecular weight`$`g/mol`
      )
    ) %>% # update the columns with common units
    dplyr::mutate(
      xUnit = targetXUnit,
      yUnit = targetYUnit
    ) %>%
    dplyr::ungroup()
}
