#' @title PKParameter
#' @docType class
#' @description Standard PK Parameters defined in the OSPSuite
PKParameter <- R6::R6Class(
  "PKParameter",
  inherit = DotNetWrapper,
  cloneable = FALSE,
  active = list(
    #' @field name Name of the PK-Parameter
    name = function(value) {
      private$.wrapProperty("Name", value)
    },
    #' @field displayName Display Name of the PK-Parameter. If not set, Name will be used
    displayName = function(value) {
      private$.wrapProperty("DisplayName", value)
    },
    #' @field dimension Dimension instance used by the PK-Parameter (Read-Only)
    dimension = function(value) {
      private$.readOnlyProperty(
        "Dimension",
        value,
        private$.dimension()$get("Name")
      )
    },
    #' @field unit Unit of the PK-Parameter (Read-Only)
    unit = function(value) {
      private$.wrapReadOnlyProperty("BaseUnit", value)
    },
    #' @field displayUnit Display Unit used for the PK-Parameter
    displayUnit = function(value) {
      private$.wrapProperty("DisplayUnit", value)
    }
  ),
  private = list(
    .dimension = function() {
      self$get("Dimension")
    }
  ),
  public = list(
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      ospsuite.utils::ospPrintClass(self)
      ospsuite.utils::ospPrintItems(list(
        "Name" = self$name,
        "DisplayName" = self$displayName,
        "Dimension" = self$dimension,
        "DisplayUnit" = self$displayUnit
      ))
    }
  )
)
