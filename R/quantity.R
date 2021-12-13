WITH_DIMENSION_EXTENSION <- "OSPSuite.Core.Domain.WithDimensionExtensions"
WITH_DISPLAY_UNIT_EXTENSION <- "OSPSuite.Core.Domain.WithDisplayUnitExtensions"
QUANTITY_EXTENSIONS <- "OSPSuite.Core.Domain.QuantityExtensions"

#' @title Quantity
#' @docType class
#' @description  A quantity of the model (with unit, value) such as a Parameter or an Amount
#' @format NULL
#' @keywords internal
Quantity <- R6::R6Class(
  "Quantity",
  cloneable = FALSE,
  inherit = Entity,
  active = list(
    #' @field value The value of the quantity in unit
    value = function(value) {
      private$wrapProperty("Value", value)
    },
    #' @field unit The base unit in which the quantity value is defined (Read-Only)
    unit = function(value) {
      private$.unit <- private$wrapExtensionMethodCached(WITH_DIMENSION_EXTENSION, "BaseUnitName", "unit", private$.unit, value)
      return(private$.unit)
    },
    #' @field displayUnit The unit in which the quantity value is usually displayed (Read-Only)
    displayUnit = function(value) {
      private$wrapExtensionMethod(WITH_DISPLAY_UNIT_EXTENSION, "DisplayUnitName", "displayUnit", value)
    },
    #' @field dimension The dimension in which the quantity is defined  (Read-Only)
    dimension = function(value) {
      private$.dimension <- private$wrapExtensionMethodCached(WITH_DIMENSION_EXTENSION, "DimensionName", "dimension", private$.dimension, value)
      return(private$.dimension)
    },
    #' @field  allUnits the list of all supported units (Read-Only)
    allUnits = function(value) {
      # Optimized implementation to avoid constant marshalling with .NET. We saved the array of units once the first time it is accessed
      private$.allUnits <- private$wrapExtensionMethodCached(WITH_DIMENSION_EXTENSION, "AllUnitNames", "allUnits", private$.allUnits, value)
      return(private$.allUnits)
    },
    #' @field quantityType The type of the quantity (Read-Only)
    quantityType = function(value) {
      private$wrapReadOnlyProperty("QuantityTypeAsString", value)
    },
    #' @field formula An instance of a `Formula` object used by this quantity (Read-Only)
    formula = function(value) {
      private$readOnlyProperty("formula", value, private$.formula)
    },
    #' @field isTable Returns `TRUE` if the formula used by this quantity is a table formula otherwise `FALSE`
    isTable = function(value) {
      private$readOnlyProperty("isTable", value, self$formula$isTable)
    },
    #' @field isConstant Returns `TRUE` if the formula used by this quantity is a constant formula otherwise `FALSE`
    isConstant = function(value) {
      private$readOnlyProperty("isConstant", value, self$formula$isConstant)
    },
    #' @field isFormula Returns `TRUE` if the formula used by this quantity is an explicit formula (e.g an equation) otherwise `FALSE`
    isFormula = function(value) {
      private$readOnlyProperty("isFormula", value, self$formula$isExplicit)
    },
    #' @field isDistributed Returns `TRUE` if the quantity represents a quantity with an underlying distribution otherwise `FALSE`
    isDistributed = function(value) {
      private$readOnlyProperty("isDistributed", value, self$formula$isDistributed)
    },
    #' @field formulaString Returns the equation of the formula for a quantity using an explicit formula (e.g. `isFormula == TRUE`) or `NULL` for a quantity that does not use an explicit formula.
    formulaString = function(value) {
      private$readOnlyProperty("formulaString", value, self$formula$formulaString)
    },
    #' @field isFixedValue Returns `TRUE` if the formula was overridden by a constant value, otherwise `FALSE`
    isFixedValue = function(value) {
      private$wrapProperty("IsFixedValue", value)
    }
  ),
  private = list(
    .formula = NULL,
    .allUnits = NULL,
    .unit = NULL,
    .dimension = NULL,
    printQuantity = function(valueCaption = "Value") {
      private$printClass()
      private$printLine("Path", self$path)
      self$printQuantityValue(valueCaption)
      self$formula$printFormula()
      if (!self$isConstant && !self$isDistributed) {
        private$printLine("Value overrides formula", self$isFixedValue)
      }
      invisible(self)
    }
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @param ref .NET Instance
    #' @return A new `Quantity` object.
    initialize = function(ref) {
      super$initialize(ref)
      # Cannot use property Formula directly from the quantity because of new override in Distributed Parameter
      formula <- private$wrapExtensionMethod(QUANTITY_EXTENSIONS, "GetFormula")
      private$.formula <- Formula$new(formula)
      if (self$isTable) {
        private$.formula <- TableFormula$new(formula)
      }
    },
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printQuantity()
      private$printLine("Quantity Type", self$quantityType)
    },
    #' @description
    #' Print the name of the quantity and its value
    printValue = function() {
      self$printQuantityValue(self$name)
    },
    #' @description
    #' Print the the value and unit of the quantity
    #' @param  caption Text to prepend to the value
    printQuantityValue = function(caption) {
      if (self$unit == "") {
        private$printLine(caption, formatNumerics(self$value))
      } else {
        private$printLine(caption, paste0(formatNumerics(self$value), " [", self$unit, "]"))
      }
    },
    #' @description
    #' Convert value from unit to the base unit and sets the value in base unit.
    #' @param value Value to set. If unit is null, we assume that the value is in base unit
    #' @param unit Optional unit in which the value is given.
    setValue = function(value, unit = NULL) {
      validateIsNumeric(value)
      validateIsString(unit, nullAllowed = TRUE)
      if (!is.null(unit)) {
        unit <- encodeUnit(unit)
        validateHasUnit(self, unit)
        value <- rClr::clrCallStatic(WITH_DIMENSION_EXTENSION, "ConvertToBaseUnit", self$ref, value, unit)
      }
      self$value <- value
    },
    #' @description
    #' Returns `TRUE` if the quantity supports the given unit otherwise `FALSE`.
    #' For the list of supported units, use `allUnits`
    #' @param unit Unit to check
    hasUnit = function(unit) {
      validateIsString(unit)
      any(self$allUnits == unit)
    },
    #' @description
    #' Ensures that the quantity uses the value computed by its formula. It is a shortcut for `self$isFixedValue <- false`.
    reset = function() {
      self$isFixedValue <- FALSE
    }
  )
)
