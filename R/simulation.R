#' @title Simulation
#' @docType class
#' @description  An OSPSuite simulation
#' @field root Root container of the simulation (read-only)
#' @field path Path of the root container of the simulation (read-only)
#' @field settings SimulationSettings object for the simulation (read-only)
#' @field solver SimulationSolver object for the simulation (read-only)
#' @field sourceFile Path to the file the simulation was loaded from (read-only)
#' @format NULL
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
    },
    solver = function(value) {
      if (missing(value)) {
        self$settings$solver
      } else {
        stop(messages$errorPropertyReadOnly("settings"), call. = FALSE)
      }
    },
    sourceFile = function(value) {
      if (missing(value)) {
        private$sourceFileValue
      } else {
        stop(messages$errorPropertyReadOnly("sourceFile"), call. = FALSE)
      }
    }
  ),
  public = list(
    initialize = function(ref, sourceFile = NULL) {
      super$initialize(ref)
      private$sourceFileValue <- sourceFile
    },
    print = function(...) {
      private$printClass()
      private$printLine("Name", self$name)
      private$printLine("Source file", self$sourceFile)
      invisible(self)
    }
  ),
  private = list(
    sourceFileValue = NULL
  )
)
