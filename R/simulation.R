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
        model <- self$get("Model")
        root <- model$get("Root")
        Container$new(root)
      } else {
        private$.throwPropertyIsReadonly("root")
      }
    },
    #' @field path Path of the root container of the simulation (read-only)
    path = function(value) {
      private$.readOnlyProperty("path", value, self$root$path)
    },
    #' @field solver SimulationSolver object for the simulation (read-only)
    solver = function(value) {
      if (missing(value)) {
        private$.settings$solver
      }
    },
    #' @field outputSchema outputSchema object for the simulation (read-only)
    outputSchema = function(value) {
      private$.readOnlyProperty(
        "outputSchema",
        value,
        private$.settings$outputSchema
      )
    },
    #' @field outputSelections outputSelections object for the simulation (read-only)
    outputSelections = function(value) {
      private$.readOnlyProperty(
        "outputSelections",
        value,
        private$.settings$outputSelections
      )
    },
    #' @field sourceFile Path to the file the simulation was loaded from (read-only)
    sourceFile = function(value) {
      private$.readOnlyProperty("sourceFile", value, private$.sourceFile)
    },
    #' @field name Name of the simulation
    name = function(value) {
      if (missing(value)) {
        return(self$get("Name"))
      } else {
        validateIsString(value)
        # Trim the name of ws
        value <- trimws(value, which = "both")
        # Check if the name actually changes
        if (self$name == value) {
          return(invisible(self))
        }

        # Check for forbidden names
        forbiddenNames <- .getIllegalSimulationNames(self)
        if (value %in% forbiddenNames) {
          stop(messages$forbiddenSimulationName(value, self))
        }

        # Check for illegal characters
        illegalChars <- .getIllegalCharacters()
        if (any(stringr::str_detect(value, paste0("[", illegalChars, "]")))) {
          stop(messages$illegalCharactersInName(value))
        }
        self$set("Name", value)
      }
    },
    #' @field configuration An object of the type `SimulationConfiguration`,
    #' describing the modules used for the simulation, selected Parameter Values (PV) and Initial Conditions (IC), and molecule calculation methods (read-only).
    configuration = function(value) {
      # OSP Version number that is required for this feature
      supportedVersion <- 12
      if (missing(value)) {
        # Convert to numeric as the returned value is a string
        simVersion <- as.numeric(self$get("Creation")$get("Version"))
        if (simVersion < supportedVersion) {
          stop(messages$errorFeatureNotSupportedBySimulation("SimulationConfiguration", simVersion, supportedVersion))
        }
        netObj <- self$get("Configuration")
        return(SimulationConfiguration$new(netObj))
      } else {
        private$.throwPropertyIsReadonly("configuration")
      }
    }
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @param netObject Reference to `NetObject` .NET simulation object
    #' @param sourceFile (Optional) File used to load the simulation
    #' @return A new `Simulation` object.
    initialize = function(netObject, sourceFile = NULL) {
      super$initialize(netObject)
      private$.sourceFile <- sourceFile
      private$.settings <- SimulationSettings$new(self$get("Settings"))
    },
    #' @description
    #' Sets the buildConfiguration for this simulation
    setBuildConfiguration = function(buildConfiguration) {
      private$.buildConfiguration <- buildConfiguration
    },
    #' @description
    #' Returns the name of all endogenous stationary molecules defined in the simulation. (e.g. with the flag IsStationary = TRUE)
    #' This is a typically a molecule that is individual specific such as en Enzyme, Protein, Transporter, FcRn etc.
    allEndogenousStationaryMoleculeNames = function() {
      private$.buildConfiguration$call(
        "AllPresentEndogenousStationaryMoleculeNames"
      )
    },
    #' @description
    #' Returns the name of all xenobiotic floating molecules defined in the simulation. (e.g. with the flag IsStationary = FALSE)
    #' This is typically a molecule that is being explicitly simulated such as Compound, Inhibitor, DrugComplex.
    allXenobioticFloatingMoleculeNames = function() {
      private$.buildConfiguration$call(
        "AllPresentXenobioticFloatingMoleculeNames"
      )
    },
    #' @description
    #' Returns the name of all stationary molecules defined in the simulation. (e.g. with the flag IsStationary = TRUE)
    allStationaryMoleculeNames = function() {
      private$.buildConfiguration$call("AllPresentStationaryMoleculeNames")
    },
    #' @description
    #' Returns the name of all floating molecules defined in the simulation. (e.g. with the flag IsStationary = FALSE)
    allFloatingMoleculeNames = function() {
      private$.buildConfiguration$call("AllPresentFloatingMoleculeNames")
    },
    #' @description
    #' Returns the mol weight value (in core unit) associated to the quantity with given path or NA if not found
    #' @param quantityPath Path of quantity used to retrieve the molecular weight
    molWeightFor = function(quantityPath) {
      validateIsString(quantityPath)
      mw <- self$call("MolWeightFor", quantityPath)
      mw %||% NA_real_
    },
    #' @description
    #' Returns the applications ordered by start time associated to the quantity with path `quantityPath` or an empty list if not found
    #' @param quantityPath Path of quantity used to retrieve the applications (e.g. applications resulting in this quantity being applied)
    allApplicationsFor = function(quantityPath) {
      validateIsString(quantityPath)
      netApplicationParameters <- rSharp::callStatic(
        MODEL_CORE_SIMULATION_EXTENSIONS,
        "AllApplicationParametersOrderedByStartTimeForQuantityPath",
        self,
        quantityPath
      )
      .toObjectType(netApplicationParameters, Application)
    },
    #' @description
    #' Print the object to the console
    #' @param printClassProperties Logical, whether to print class properties (default: `FALSE`). If `TRUE`, calls first the `print` method of the parent class.
    #' Useful for debugging.
    #' @param ... Rest arguments.
    print = function(printClassProperties = FALSE, ...) {
      if (printClassProperties) {
        super$print(...)
      }
      ospsuite.utils::ospPrintClass(self)
      ospsuite.utils::ospPrintItems(list(
        "Name" = self$name,
        "Source file" = self$sourceFile
      ))
    }
  ),
  private = list(
    .sourceFile = NULL,
    .buildConfiguration = NULL,
    .settings = NULL
  )
)
