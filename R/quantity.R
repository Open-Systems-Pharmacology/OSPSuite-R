#' @title Quantity
#' @docType class
#' @description  A quantity of the model (with unit, value) such as a Parameter or an Amount
#' @field value The value of the quantity in unit
#' @field unit The base unit in which the quantity is defined (Read-Only)
#' @field dimension The dimension in which the quantity is defined  (Read-Only)
#' @field persitable A boolean indicating whether a value will be defined in the simulated results for this quantity
#' @format NULL
Quantity <- R6Class(
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
    persistable = function(value) {
      private$wrapProperties("Persistable", value)
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
