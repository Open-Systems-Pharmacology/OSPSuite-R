WITH_DIMENSION_EXTENSION <- "OSPSuite.Core.Domain.WithDimensionExtensions"

#' @title Quantity
#' @docType class
#' @description  A quantity of the model (with unit, value) such as a Parameter or an Amount
#' @field value The value of the quantity in unit
#' @field unit The base unit in which the quantity is defined (Read-Only)
#' @field dimension The dimension in which the quantity is defined  (Read-Only)
#' @section Methods:
#' \describe{
#'   \item{setValue(value, unit=NULL)}{Convert value from unit to the base unit and sets the value in base unit. If unit is null, we assume that the value is in base unit}
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
    }
  ),
  private = list(
    printQuantity = function() {
      private$printClass()
      private$printLine("Path", self$path)
      self$printQuantityValue("Value")
      invisible(self)
    }
  ),
  public = list(
    print = function(...) {
      private$printQuantity()
      private$printLine("Quantity Type", getEnumKey(QuantityType, self$quantityType))
    },
    printValue = function() {
      self$printQuantityValue(self$name)
    },
    printQuantityValue = function(caption) {
      private$printLine(caption, paste0(formatNumerics(self$value), " [", self$unit, "]"))
    },
    setValue = function(value, unit = NULL) {
      validateIsNumeric(value)
      if (!is.null(unit)){
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
    }
  )
)
