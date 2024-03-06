#' Standard PK-Parameters types defined in OSPSuite
#' This is only used to defined how a user defined PK Parameter should be calculated
#'
#' @export
StandardPKParameter <- enum(c(
  Unknown = 0,
  C_max = 1,
  C_max_norm = 2,
  C_min = 3,
  C_min_norm = 4,
  t_max = 5,
  t_min = 6,
  C_trough = 7,
  C_trough_norm = 8,
  AUC_tEnd = 9,
  AUC_tEnd_norm = 10,
  AUCM_tEnd = 11,
  AUC_inf = 12,
  AUC_inf_norm = 13,
  AUC_tEnd_inf = 14,
  AUC_tEnd_inf_norm = 15,
  CL = 16,
  MRT = 17,
  FractionAucEndToInf = 18,
  Thalf = 19,
  Vss = 20,
  Vd = 21,
  Tthreshold = 22
))

#' @title PKParameter
#' @docType class
#' @description Standard PK Parameters defined in the OSPSuite
PKParameter <- R6::R6Class("PKParameter",
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
      private$.readOnlyProperty("Dimension", value, private$.dimension()$get("Name"))
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
      private$printClass()
      private$printLine("Name", self$name)
      private$printLine("DisplayName", self$displayName)
      private$printLine("Dimension", self$dimension)
      private$printLine("DisplayUnit", self$displayUnit)
      invisible(self)
    }
  )
)
