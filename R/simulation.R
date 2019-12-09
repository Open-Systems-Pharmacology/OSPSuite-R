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
        private$throwPropertyIsReadonly("root")
      }
    },
    path = function(value) {
      private$readOnlyProperty("path", value, self$root$path)
    },
    settings = function(value) {
      if (missing(value)) {
        private$.settings
      } else {
        private$throwPropertyIsReadonly("settings")
      }
    },
    solver = function(value) {
      private$readOnlyProperty("solver", value, private$.settings$solver)
    },
    outputSchema = function(value) {
      private$readOnlyProperty("outputSchema", value, private$.settings$outputSchema)
    },
    outputSelections = function(value) {
      private$readOnlyProperty("outputSelections", value, private$.settings$outputSelections)
    },
    sourceFile = function(value) {
      private$readOnlyProperty("sourceFile", value, private$.sourceFile)
    }
  ),
  public = list(
    initialize = function(ref, sourceFile = NULL) {
      super$initialize(ref)
      private$.sourceFile <- sourceFile
      private$.buildConfiguration <- rClr::clrGet(self$ref, "BuildConfiguration")
      private$.settings <- SimulationSettings$new(rClr::clrGet(private$.buildConfiguration, "SimulationSettings"))
    },
    allMoleculeNames = function() {
      rClr::clrCall(private$.buildConfiguration, "AllPresentEndogenousStationaryMoleculeNames")
    },
    print = function(...) {
      private$printClass()
      private$printLine("Name", self$name)
      private$printLine("Source file", self$sourceFile)
      invisible(self)
    }
  ),
  private = list(
    .sourceFile = NULL,
    .buildConfiguration = NULL,
    .settings = NULL
  )
)
