validateHasUnit <- function(quantity, unit) {
  ospsuite.utils::validateIsOfType(quantity, Quantity)
  ospsuite.utils::validateIsString(unit)
  if (quantity$hasUnit(unit)) {
    return()
  }
  stop(messages$errorUnitNotDefined(quantity$name, quantity$dimension, unit))
}

