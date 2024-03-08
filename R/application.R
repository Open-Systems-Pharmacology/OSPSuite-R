#' @title Application
#' @docType class
#' @description  Application representing one instance of an application in a Simulation
#'
#' @format NULL
Application <- R6::R6Class(
  "Application",
  cloneable = FALSE,
  inherit = DotNetWrapper,
  active = list(
    #' @field startTime Start time of application instance (Read-Only)
    startTime = function(value) {
      if (missing(value)) {
        .toObjectType(private$wrapReadOnlyProperty("StartTime", value), Parameter)
      }
    },
    #' @field infusionTime Infusion time of application instance or null if undefined (Read-Only)
    infusionTime = function(value) {
      if (missing(value)) {
        .toObjectType(private$wrapReadOnlyProperty("InfusionTime", value), Parameter)
      }
    },
    #' @field drugMass Applied drugmass of the application instance of null if undefined (Read-Only)
    drugMass = function(value) {
      if (missing(value)) {
        .toObjectType(private$wrapReadOnlyProperty("DrugMass", value), Parameter)
      }
    }
  ),
  public = list(
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$.printClass()
      self$startTime$printValue()

      if (!is.null(self$infusionTime)) {
        self$infusionTime$printValue()
      }

      if (!is.null(self$drugMass)) {
        self$drugMass$printValue()
      }

      invisible(self)
    }
  )
)
