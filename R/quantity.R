WITH_DIMENSION_EXTENSION <- "OSPSuite.Core.Domain.WithDimensionExtensions"
QUANTITY_EXTENSIONS <- "OSPSuite.Core.Domain.QuantityExtensions"

#' @title Quantity
#' @docType class
#' @description  A quantity of the model (with unit, value) such as a Parameter or an Amount
#' @field value The value of the quantity in unit
#' @field unit The base unit in which the quantity is defined (Read-Only)
#' @field dimension The dimension in which the quantity is defined  (Read-Only)
#' @field quantityType The type of the quantity (Read-Only)
#' @field formula An instance of a \code{Formula} object used by this quantity (Read-Only)
#' @field isTable Returns \code{TRUE} if the formula used by this quantity is a table formula otherwise \code{FALSE}
#' @field isConstant Returns \code{TRUE} if the formula used by this quantity is a constant formula otherwise \code{FALSE}
#' @field isFormula Returns \code{TRUE} if the formula used by this quantity is an explicit formula (e.g an equation) otherwise \code{FALSE}
#' @field isDistributed Returns \code{TRUE} if the quantity represents a quantity with an underlying distribution otherwise \code{FALSE}
#' @field formulaString Returns the equation of the formula for a quantity using an explicit formula (e.g. \code{isFormula == TRUE}) or \code{NULL} for a quantity that does not use an explicit formula.
#' @field isFixedValue Returns \code{TRUE} of the formua was overriden by a constant value otherwise \code{FALSE}
#' @section Methods:
#' \describe{
#'   \item{setValue(value, unit=NULL)}{Convert value from unit to the base unit and sets the value in base unit. If unit is null, we assume that the value is in base unit}
#'   \item{hasUnit(unit)}{Returns \code{TRUE} if the quantity supports the given unit otherwise \code{FALSE}. For the list of supported units, use \code{allUnits}}
#'   \item{allUnits()}{Returns the list of all supported units}
#'   \item{reset()}{Ensures that the quantity uses the value computed by its formula. It is a shortcut for \code{self$isFixedValue <- false}. }
#'   }
#' @format NULL
Quantity <- R6::R6Class(
  "Quantity",
  inherit = Entity,

  active = list(
    value = function(value) {
      private$wrapProperty("Value", value)
    },
    unit = function(value) {
      private$wrapExtensionMethod(WITH_DIMENSION_EXTENSION, "BaseUnitName", "baseUnit")
    },
    dimension = function(value) {
      private$wrapExtensionMethod(WITH_DIMENSION_EXTENSION, "DimensionName", "dimension")
    },
    quantityType = function(value) {
      private$wrapReadOnlyProperty("QuantityType", value)
    },
    formula = function(value) {
      private$readOnlyProperty("formula", value, private$.formula)
    },
    isTable = function(value) {
      private$readOnlyProperty("isTable", value, self$formula$isTable)
    },
    isConstant = function(value) {
      private$readOnlyProperty("isConstant", value, self$formula$isConstant)
    },
    isFormula = function(value) {
      private$readOnlyProperty("isFormula", value, self$formula$isExplicit)
    },
    isDistributed = function(value) {
      private$readOnlyProperty("isDistributed", value, self$formula$isDistributed)
    },
    formulaString = function(value) {
      private$readOnlyProperty("formulaString", value, self$formula$formulaString)
    },
    isFixedValue = function(value) {
      private$wrapProperty("IsFixedValue", value)
    }
  ),
  private = list(
    .formula = NULL,
    printQuantity = function() {
      private$printClass()
      private$printLine("Path", self$path)
      self$printQuantityValue("Value")
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
      private$printLine("Quantity Type", paste(getQuantityTypesStringFromInt(self$quantityType), collapse = ", "))
    },
    printValue = function() {
      self$printQuantityValue(self$name)
    },
    printQuantityValue = function(caption) {
      private$printLine(caption, paste0(formatNumerics(self$value), " [", self$unit, "]"))
    },
    setValue = function(value, unit = NULL) {
      validateIsNumeric(value)
      if (!is.null(unit)) {
        validateHasUnit(self, unit)
        value <- rClr::clrCallStatic(WITH_DIMENSION_EXTENSION, "ConvertToBaseUnit", self$ref, value, unit)
      }
      self$value <- value
    },
    hasUnit = function(unit) {
      validateIsString(unit)
      rClr::clrCallStatic(WITH_DIMENSION_EXTENSION, "HasUnit", self$ref, unit)
    },
    allUnits = function() {
      rClr::clrCallStatic(WITH_DIMENSION_EXTENSION, "AllUnitNames", self$ref)
    },
    reset = function() {
      self$isFixedValue <- FALSE
    }
  )
)
