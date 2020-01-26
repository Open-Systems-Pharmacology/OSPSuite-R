WITH_DIMENSION_EXTENSION <- "OSPSuite.Core.Domain.WithDimensionExtensions"
WITH_DISPLAY_UNIT_EXTENSION <- "OSPSuite.Core.Domain.WithDisplayUnitExtensions"
QUANTITY_EXTENSIONS <- "OSPSuite.Core.Domain.QuantityExtensions"

#' @title Quantity
#' @docType class
#' @description  A quantity of the model (with unit, value) such as a Parameter or an Amount
#' @format NULL
Quantity <- R6::R6Class(
  "Quantity",
  inherit = Entity,

  active = list(
    #' @field value The value of the quantity in unit
    value = function(value) {
      private$wrapProperty("Value", value)
    },
    #' @field unit The base unit in which the quantity value is defined (Read-Only)
    unit = function(value) {
      private$wrapExtensionMethod(WITH_DIMENSION_EXTENSION, "BaseUnitName", "unit")
    },
    #' @field displayUnit The unit in which the quantity value is usually displayed (Read-Only)
    displayUnit = function(value) {
      private$wrapExtensionMethod(WITH_DISPLAY_UNIT_EXTENSION, "DisplayUnitName", "displayUnit")
    },
    #' @field dimension The dimension in which the quantity is defined  (Read-Only)
    dimension = function(value) {
      private$wrapExtensionMethod(WITH_DIMENSION_EXTENSION, "DimensionName", "dimension")
    },
    #'@field  allUnits the list of all supported units
    allUnits = function(value) {
      private$wrapExtensionMethod(WITH_DIMENSION_EXTENSION, "AllUnitNames", allUnits)
    },
    #' @field quantityType The type of the quantity (Read-Only)
    quantityType = function(value) {
      private$wrapReadOnlyProperty("QuantityType", value)
    },
    #' @field formula An instance of a \code{Formula} object used by this quantity (Read-Only)
    formula = function(value) {
      private$readOnlyProperty("formula", value, private$.formula)
    },
    #' @field isTable Returns \code{TRUE} if the formula used by this quantity is a table formula otherwise \code{FALSE}
    isTable = function(value) {
      private$readOnlyProperty("isTable", value, self$formula$isTable)
    },
    #' @field isConstant Returns \code{TRUE} if the formula used by this quantity is a constant formula otherwise \code{FALSE}
    isConstant = function(value) {
      private$readOnlyProperty("isConstant", value, self$formula$isConstant)
    },
    #' @field isFormula Returns \code{TRUE} if the formula used by this quantity is an explicit formula (e.g an equation) otherwise \code{FALSE}
    isFormula = function(value) {
      private$readOnlyProperty("isFormula", value, self$formula$isExplicit)
    },
    #' @field isDistributed Returns \code{TRUE} if the quantity represents a quantity with an underlying distribution otherwise \code{FALSE}
    isDistributed = function(value) {
      private$readOnlyProperty("isDistributed", value, self$formula$isDistributed)
    },
    #' @field formulaString Returns the equation of the formula for a quantity using an explicit formula (e.g. \code{isFormula == TRUE}) or \code{NULL} for a quantity that does not use an explicit formula.
    formulaString = function(value) {
      private$readOnlyProperty("formulaString", value, self$formula$formulaString)
    },
    #' @field isFixedValue Returns \code{TRUE} of the formua was overriden by a constant value otherwise \code{FALSE}
    isFixedValue = function(value) {
      private$wrapProperty("IsFixedValue", value)
    }
  ),
  private = list(
    .formula = NULL,
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
    initialize = function(ref) {
      super$initialize(ref)
      # Cannot use property Formula directly from the quantity because of new override in Distributed Parameter
      formula <- private$wrapExtensionMethod(QUANTITY_EXTENSIONS, "GetFormula")
      private$.formula <- Formula$new(formula)
      if (self$isTable) {
        private$.formula <- TableFormula$new(formula)
      }
    },
    print = function(...) {
      private$printQuantity()
      private$printLine("Quantity Type", getQuantityTypeAsString(self$quantityType))
    },
    printValue = function() {
      self$printQuantityValue(self$name)
    },
    printQuantityValue = function(caption) {
      private$printLine(caption, paste0(formatNumerics(self$value), " [", self$unit, "]"))
    },
    #'  @description
    #'  Convert value from unit to the base unit and sets the value in base unit.
    #'  @param value Value to set. If unit is null, we assume that the value is in base unit
    #'  @param unit Optional unit in which the value is given.
    setValue = function(value, unit = NULL) {
      validateIsNumeric(value)
      if (!is.null(unit)) {
        validateHasUnit(self, unit)
        value <- rClr::clrCallStatic(WITH_DIMENSION_EXTENSION, "ConvertToBaseUnit", self$ref, value, unit)
      }
      self$value <- value
    },
    #' @description
    #' Returns \code{TRUE} if the quantity supports the given unit otherwise \code{FALSE}.
    #' For the list of supported units, use \code{allUnits}
    #' @param unit Unit to check
    hasUnit = function(unit) {
      validateIsString(unit)
      rClr::clrCallStatic(WITH_DIMENSION_EXTENSION, "HasUnit", self$ref, unit)
    },
    #'  @description
    #'  Ensures that the quantity uses the value computed by its formula. It is a shortcut for \code{self$isFixedValue <- false}.
    reset = function() {
      self$isFixedValue <- FALSE
    }
  )
)
