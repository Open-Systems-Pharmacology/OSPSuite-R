#' @title Simulation
#' @docType class
#' @description  An OSPSuite simulation
#' @format NULL
Simulation <- R6::R6Class(
  "Simulation",
  inherit = ObjectBase,
  active = list(
    #' @field root Root container of the simulation (read-only)
    root = function(value) {
      if (missing(value)) {
        model <- rClr::clrGet(self$ref, "Model")
        root <- rClr::clrGet(model, "Root")
        Container$new(root)
      } else {
        private$throwPropertyIsReadonly("root")
      }
    },
    #' @field path Path of the root container of the simulation (read-only)
    path = function(value) {
      private$readOnlyProperty("path", value, self$root$path)
    },
    #' @field settings SimulationSettings object for the simulation (read-only)
    settings = function(value) {
      if (missing(value)) {
        private$.settings
      } else {
        private$throwPropertyIsReadonly("settings")
      }
    },
    #' @field solver SimulationSolver object for the simulation (read-only)
    solver = function(value) {
      private$readOnlyProperty("solver", value, private$.settings$solver)
    },
    #' @field outputSchema outputSchema object for the simulation (read-only)
    outputSchema = function(value) {
      private$readOnlyProperty("outputSchema", value, private$.settings$outputSchema)
    },
    #' @field outputSelections outputSelections object for the simulation (read-only)
    outputSelections = function(value) {
      private$readOnlyProperty("outputSelections", value, private$.settings$outputSelections)
    },
    #' @field sourceFile Path to the file the simulation was loaded from (read-only)
    sourceFile = function(value) {
      private$readOnlyProperty("sourceFile", value, private$.sourceFile)
    }
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @param ref Reference to .NET simulation object
    #' @param sourceFile (Optional) File used to load the simulation
    #' @return A new `Simulation` object.
    initialize = function(ref, sourceFile = NULL) {
      super$initialize(ref)
      private$.sourceFile <- sourceFile
      private$.buildConfiguration <- rClr::clrGet(self$ref, "BuildConfiguration")
      private$.settings <- SimulationSettings$new(rClr::clrGet(private$.buildConfiguration, "SimulationSettings"))
    },
    #' @description
    #' Returns all endogenous molecule names defined in the simulation.
    #' This is a typically a molecule that is individual specific such as en Enzyme, Protein, Transporter, FcRn etc.
    allEndogenousMoleculeNames = function() {
      rClr::clrCall(private$.buildConfiguration, "AllPresentEndogenousStationaryMoleculeNames")
    },
    #' @description
    #' Returns all xenobiotic molecule names defined in the simulation.
    #' This is typically a molecule that is being simulated such as Compound, Inhibitor, DrugComplex.
    allXenobioticMoleculeNames = function() {
      rClr::clrCall(private$.buildConfiguration, "AllPresentXenobioticFloatingMoleculeNames")
    },
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
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
