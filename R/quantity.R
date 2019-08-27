#' @title Quantity
#' @docType class
#' @description  A quantity of the model (with unit, value) such as a Parameter or an Amount
#' @field value The value of the quantity in unit
#' @field unit The base unit in which the quantity is defined (Read-Only)
#' @field dimension The dimension in which the quantity is defined  (Read-Only)
#' @format NULL
Quantity <- R6::R6Class(
  "Quantity",
  inherit = Entity,
  active = list(
    value = function(value) {
      private$wrapProperties("Value", value)
    },
    unit = function(value) {
      private$wrapExtensionMethod("OSPSuite.Core.Domain.WithDimensionExtensions", "BaseUnitName", "baseUnit")
    },
    dimension = function(value) {
      private$wrapExtensionMethod("OSPSuite.Core.Domain.WithDimensionExtensions", "DimensionName", "dimension")
    },
    quantityType = function(value) {
      private$wrapReadOnlyProperties("QuantityType", value)
    }
  ),
  private = list(
    printQuantity = function() {
      private$printClass()
      private$printLine("Path", self$path)
      private$printLine("Value", paste0(formatNumerics(self$value), " [", self$unit, "]"))
      invisible(self)
    }
  ),
  public = list(
    print = function(...) {
      private$printQuantity()
      private$printLine("Quantity Type", getEnumKey(QuantityType, self$quantityType))
    }
  )
)
