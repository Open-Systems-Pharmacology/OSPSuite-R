#' @title Simulation
#' @docType class
#' @description  An OSPSuite simulation
#'
#' @field root Root container of the simulation (read-only)
#' @field path Path of the root container of the simulation (read-only)
#' @field settings SImulationSettings object for the simulation (read-only)
Simulation <- R6::R6Class(
  "Simulation",
  inherit = ObjectBase,
  active = list(
    root = function(value) {
      if (missing(value)) {
        model <- rClr::clrGet(self$ref, "Model")
        root <- rClr::clrGet(model, "Root")
        Container$new(root)
      } else {
        stop(messages$errorPropertyReadOnly("root"), call. = FALSE)
      }
    },
    path = function(value) {
      if (missing(value)) {
        self$root$path
      } else {
        stop(messages$errorPropertyReadOnly("path"), call. = FALSE)
      }
    },
    settings = function(value) {
      if (missing(value)) {
        buildConfiguration <- rClr::clrGet(self$ref, "BuildConfiguration")
        settings <- rClr::clrGet(buildConfiguration, "SimulationSettings")
        SimulationSettings$new(settings)
      } else {
        stop(messages$errorPropertyReadOnly("settings"), call. = FALSE)
      }
    }
  ),
  public = list(
    print = function(...) {
      private$printClass()
      private$printLine("Name", self$name)
      invisible(self)
    }
  )
)
