MODEL_CORE_SIMULATION_EXTENSIONS <- "OSPSuite.Core.Domain.ModelCoreSimulationExtensions"

#' @title Simulation
#' @docType class
#' @description  An OSPSuite simulation
#' @format NULL
Simulation <- R6::R6Class(
  "Simulation",
  cloneable = FALSE,
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
    #' @field solver SimulationSolver object for the simulation (read-only)
    solver = function(value) {
      if (missing(value)) {
        private$.settings$solver
      }
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
    #' Returns the name of all endogenous stationary molecules defined in the simulation. (e.g. with the flag IsStationary = TRUE)
    #' This is a typically a molecule that is individual specific such as en Enzyme, Protein, Transporter, FcRn etc.
    allEndogenousStationaryMoleculeNames = function() {
      rClr::clrCall(private$.buildConfiguration, "AllPresentEndogenousStationaryMoleculeNames")
    },
    #' @description
    #' Returns the name of all xenobiotoc floating molecules defined in the simulation. (e.g. with the flag IsStationary = FALSE)
    #' This is typically a molecule that is being explicitly simulated such as Compound, Inhibitor, DrugComplex.
    allXenobioticFloatingMoleculeNames = function() {
      rClr::clrCall(private$.buildConfiguration, "AllPresentXenobioticFloatingMoleculeNames")
    },
    #' @description
    #' Returns the name of all stationary molecules defined in the simulation. (e.g. with the flag IsStationary = TRUE)
    allStationaryMoleculeNames = function() {
      rClr::clrCall(private$.buildConfiguration, "AllPresentStationaryMoleculeNames")
    },
    #' @description
    #' Returns the name of all floating molecules defined in the simulation. (e.g. with the flag IsStationary = FALSE)
    allFloatingMoleculeNames = function() {
      rClr::clrCall(private$.buildConfiguration, "AllPresentFloatingMoleculeNames")
    },
    #' @description
    #' Returns the mol weight value (in core unit) associated to the quantity with given path or NA if not found
    #' @param quantityPath Path of quantity used to retrieve the molecular weight
    molWeightFor = function(quantityPath) {
      validateIsString(quantityPath)
      mw <- rClr::clrCall(self$ref, "MolWeightFor", quantityPath)
      mw %||% NA
    },
    #' @description
    #' Returns the applications ordered by start time associated to the quantity with path `quantityPath` or an empty list if not found
    #' @param quantityPath Path of quantity used to retrieve the applications (e.g. applications resulting in this quantity being applied)
    allApplicationsFor = function(quantityPath) {
      validateIsString(quantityPath)
      netApplicationParameters <- rClr::clrCallStatic(MODEL_CORE_SIMULATION_EXTENSIONS, "AllApplicationParametersOrderedByStartTimeForQuantityPath", self$ref, quantityPath)
      toObjectType(netApplicationParameters, Application)
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
