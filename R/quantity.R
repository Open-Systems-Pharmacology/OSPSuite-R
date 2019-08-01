#' @title Quantity
#' @docType class
#' @description  A quantity of the model (with unit, value) such as a Parameter or an Amount
#'
#' @field value The value of the quantity
Quantity <- R6Class(
  "Quantity",
  inherit = Entity,
  active = list(
    value = function(value) {
      private$wrapProperties("Value", value)
    },
    displayUnit = function(value) {
      private$wrapExtensionMethod("OSPSuite.Core.Domain.WithDisplayUnitExtensions", "DisplayUnitName", "displayUnit")
    },
    unit = function(value) {
      private$wrapExtensionMethod("OSPSuite.Core.Domain.WithDimensionExtensions", "BaseUnitName", "baseUnit")
    },
    dimension = function(value) {
      private$wrapExtensionMethod("OSPSuite.Core.Domain.WithDimensionExtensions", "DimensionName", "dimension")
    },
    valueInDisplayUnit = function(value) {
      private$wrapProperties("ValueInDisplayUnit", value)
    }
  ),
  private = list(
    printQuantity = function() {
      private$printClass()
      private$printLine("Path", self$path)
      private$printLine("Value", paste0(format(self$value, digits = 5, nsmall = 2), " [", self$unit, "]"))
      invisible(self)
    }
  )
)
