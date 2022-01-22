validateHasUnit <- function(quantity, unit) {
  validateIsOfType(quantity, Quantity)
  validateIsString(unit)
  if (quantity$hasUnit(unit)) {
    return()
  }
  stop(messages$errorUnitNotDefined(quantity$name, quantity$dimension, unit))
}

