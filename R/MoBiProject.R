#' @title MoBi Project
#' @docType class
#' @description  A MoBi project, containing modules, building blocks, simulations, etc.
#' @format NULL
MoBiProject <- R6::R6Class(
  "MoBiProject",
  cloneable = FALSE,
  inherit = ObjectBase,
  active = list(
    #' @field sourceFile Path to the file the project was loaded from (read-only)
    sourceFile = function(value) {
      private$.readOnlyProperty("sourceFile", value, private$.sourceFile)
    },
    #' @field simulationNames Names of the simulations that are present in the project (read-only)
    simulationNames = function(value) {
      if (missing(value)) {
        # Get all simulation names using ProjectTask
        values <- .callProjectTask(property = "AllSimulationNames", self)
        return(values)
      } else {
        private$.throwPropertyIsReadonly("simulationNames")
      }
    },

    #' @field moduleNames Names of the modules that are present in the project (read-only)
    moduleNames = function(value) {
      if (missing(value)) {
        values <- .callProjectTask(property = "AllModuleNames", self)
        return(values)
      } else {
        private$.throwPropertyIsReadonly("moduleNames")
      }
    },
    #' @field parameterIdentificationNames Names of the parameter identifications
    #' that are present in the project (read-only)
    parameterIdentificationNames = function(value) {
      if (missing(value)) {
        values <- .callProjectTask(
          property = "AllParameterIdentificationNames",
          self
        )
        return(values)
      } else {
        private$.throwPropertyIsReadonly("parameterIdentificationNames")
      }
    },
    #' @field individualNames Names of the individuals that are present in the project (read-only)
    individualNames = function(value) {
      if (missing(value)) {
        values <- .callProjectTask(property = "AllIndividualNames", self)
        return(values)
      } else {
        private$.throwPropertyIsReadonly("individualNames")
      }
    },
    #' @field expressionProfilesNames Names of the expression profiles that are
    #' present in the project (read-only)
    expressionProfilesNames = function(value) {
      if (missing(value)) {
        values <- .callProjectTask(property = "AllExpressionProfileNames", self)
        return(values)
      } else {
        private$.throwPropertyIsReadonly("expressionProfilesNames")
      }
    },
    #' @field defaultSimulationSettings Default simulation settings of the project (read-only)
    defaultSimulationSettings = function(value) {
      if (missing(value)) {
        simSettings <- self$get("SimulationSettings")
        return(SimulationSettings$new(simSettings))
      } else {
        private$.throwPropertyIsReadonly("defaultSimulationSettings")
      }
    }
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #'
    #' Should not be directly used. Instead, use function `loadMoBiProject()`
    #' to load a project.
    #' @param netObject Reference to `NetObject` .NET MoBi-project object
    #' @param sourceFile (Optional) File used to load the project
    #' @returns A new `MoBiProject` object.
    initialize = function(netObject, sourceFile = NULL) {
      super$initialize(netObject)
      private$.sourceFile <- sourceFile
    },

    #' @description
    #' Load a simulation from the project
    #'
    #' @param simulationName Name of the simulation.
    #' @param stopIfNotFound If `TRUE` (default), an error is thrown if the simulation with the
    #' given name is not present in the project. If `FALSE`, `NULL` is returned.
    #' @returns A `Simulation` object, if the simulation with the given name is present in the
    #' project. `NULL` if no such simulation is available and `stopIfNotFound = FALSE`.
    getSimulation = function(simulationName, stopIfNotFound = TRUE) {
      validateIsCharacter(simulationName)
      simulation <- .callProjectTask(
        property = "SimulationByName",
        self,
        simulationName
      )
      if (is.null(simulation)) {
        if (stopIfNotFound) {
          stop(messages$errorSimulationNotFound(simulationName))
        }
        return(NULL)
      }

      sim <- Simulation$new(simulation)
      return(sim)
    },

    #' @description
    #' Get observed data present in the project.
    #'
    #' @param dataSetNames Optional. List of names of observed data sets to retrieve
    #' from project. If `NULL`, all data sets are returned.
    #' @param stopIfNotFound If `TRUE` (default), an error is thrown if any of the specified
    #' data sets is not present in the project. Otherwise, `NULL` is returned for the data sets that are not found.
    #'
    #' @returns A named list of `DataSet` objects.
    getObservedData = function(dataSetNames = NULL, stopIfNotFound = TRUE) {
      obsDataNet <- .callProjectTask(property = "AllObservedDataSets", self)

      dataSets <- vector("list", length(obsDataNet))
      names <- vector("list", length(obsDataNet))
      for (idx in seq_along(obsDataNet)) {
        dataSet <- obsDataNet[[idx]]
        name <- dataSet$get("Name")
        # Continue if names were specified but the name of the current data set
        # is not in the list of names
        if (!is.null(dataSetNames) && !(name %in% dataSetNames)) {
          next
        }
        # Create DataSet object
        dataSets[[idx]] <- DataSet$new(name = name, dataRepository = dataSet)
        names[[idx]] <- name
      }
      names(dataSets) <- names
      # Remove all NULLs
      dataSets <- Filter(Negate(is.null), dataSets)
      missingNames <- setdiff(dataSetNames, names)
      if (length(missingNames) > 0 && stopIfNotFound) {
        stop(messages$errorDataSetsNotPresentInProject(missingNames))
      }

      return(dataSets)
    },

    #' @description
    #' Get modules present in the project.
    #'
    #' @param names Optional. Names of the modules to retrieve. If `NULL`, all modules are returned.
    #' @param stopIfNotFound If `TRUE` (default), an error is thrown if any of the specified
    #' modules is not present in the project. Otherwise, `NULL` is returned for the modules that are not found.
    #' @returns A named list of `MoBiModule` objects.
    getModules = function(names = NULL, stopIfNotFound = TRUE) {
      if (is.null(names)) {
        names <- self$moduleNames
      }
      modules <- lapply(names, function(name) {
        module <- .callProjectTask(property = "ModuleByName", self, name)
        if (is.null(module)) {
          if (stopIfNotFound) {
            stop(messages$modulesNotPresentInProject(name))
          }
          return(NULL)
        }
        return(MoBiModule$new(module))
      })
      names(modules) <- names
      return(modules)
    },

    #' @description
    #' Get a specified individual from the project
    #' @param name Name of the individual
    #' @param stopIfNotFound If `TRUE` (default), an error is thrown if the specified
    #' individual is not present in the project.
    #'
    #' @returns An object of the type `BuildingBlock`. `NULL` if the project does not contain
    #' such an individual and `stopIfNotFound = FALSE`.
    getIndividual = function(name, stopIfNotFound = TRUE) {
      validateIsCharacter(name)
      individual <- .callProjectTask(
        property = "IndividualBuildingBlockByName",
        self,
        name
      )

      if (is.null(individual)) {
        if (stopIfNotFound) {
          stop(messages$errorIndividualNotFound(name))
        }
        return(NULL)
      }

      bb <- BuildingBlock$new(individual, type = BuildingBlockTypes$Individual)
      return(bb)
    },
    #' @description
    #' Get specified expression profiles from the project.
    #' @param names List of names of the expression profiles to retrieve.
    #' @param stopIfNotFound If `TRUE` (default), an error is thrown if any of the specified
    #' expression profiles is not present in the project.
    #' @returns A named list of objects of the type `BuildingBlock`. If `stopIfNotFound = FALSE`,
    #' only the expression profiles that are present in the project are returned.
    getExpressionProfiles = function(names, stopIfNotFound = TRUE) {
      validateIsCharacter(names)
      expressionProfiles <- .callProjectTask(
        property = "ExpressionProfileBuildingBlocksByName",
        self,
        names
      )

      realNames <- vector("character", length(expressionProfiles))
      profiles <- lapply(seq_along(expressionProfiles), function(idx) {
        profile <- expressionProfiles[[idx]]
        bb <- BuildingBlock$new(
          profile,
          type = BuildingBlockTypes$`Expression Profile`
        )
        realNames[[idx]] <<- bb$name
        return(bb)
      })
      names(profiles) <- realNames

      # Check if all requested expression profiles were found
      missingNames <- setdiff(names, realNames)
      if (length(missingNames) > 0) {
        if (stopIfNotFound) {
          stop(messages$errorExpressionProfileNotFound(missingNames))
        }
      }

      return(profiles)
    },

    #' @description
    #' Create a simulation configuration.
    #'
    #'
    #' @param modulesNames A list of the modules from which to create the simulation.
    #' All defined modules must be present in the project. The order of module names defines the order in which the modules will be combined to a simulation!
    #' @param individualName Optional, name of the individual.
    #' @param expressionProfilesNames Optional, list of expression profiles to apply to the simulation.
    #' @param selectedInitialConditions By default, the first Initial Conditions
    #' (IC) building block (BB) of each module will be selected. If a module has multiple
    #' IC BBs, it is possible to specify which IC BB to apply by providing a named list,
    #' where the name should be the name of the module and the value the name of the IC BB.
    #' By explicitly setting the value for a specific module to `NULL`, no IC BB from the specified module will be applied.
    #' If the list contains a module name that is not part of the provided modules, it will be ignored.
    #' @param selectedParameterValues By default, the first Parameter Values
    #' (PV) building block (BB) of each module will be selected. If a module has multiple
    #' PV BBs, it is possible to specify which PV BB to apply by providing a named list,
    #' where the name should be the name of the module and the value the name of the PV BB.
    #' By explicitly setting the value for a specific module to `NULL`, no PV BB from the specified module will be applied.
    #' If the list contains a module name that is not part of the provided modules, it will be ignored.
    #' @param settings Optional, a `SimulationSettings` object defining the simulation settings.
    #' If `NULL` (default), the default simulation settings of the project will be used.
    #' @returns A `SimulationConfiguration` object.
    createSimulationConfiguration = function(
      modulesNames,
      individualName = NULL,
      expressionProfilesNames = NULL,
      selectedInitialConditions = NULL,
      selectedParameterValues = NULL,
      settings = NULL
    ) {
      validateIsOfType(settings, "SimulationSettings", nullAllowed = TRUE)
      modules <- self$getModules(modulesNames)

      # Get the specified individual
      individual <- NULL
      if (!is.null(individualName)) {
        individual <- self$getIndividual(individualName, stopIfNotFound = TRUE)
      }
      # Get the specified expression profiles
      expressionProfiles <- list()
      if (!is.null(expressionProfilesNames)) {
        expressionProfiles <- self$getExpressionProfiles(
          expressionProfilesNames,
          stopIfNotFound = TRUE
        )
      }

      settings <- if (is.null(settings)) {
        self$defaultSimulationSettings
      } else {
        settings
      }

      configuration <- SimulationConfiguration$new(
        modules = modules,
        individual = individual,
        expressionProfiles = expressionProfiles,
        selectedInitialConditions = selectedInitialConditions,
        selectedParameterValues = selectedParameterValues,
        settings = settings
      )
      return(configuration)
    },

    #' @description
    #' Print the object to the console
    #' @param printClassProperties Whether to print class properties.
    #' @param ... Rest arguments.
    print = function(printClassProperties = FALSE, ...) {
      if (printClassProperties) {
        super$print(...)
      }
      ospsuite.utils::ospPrintClass(self)
      ospsuite.utils::ospPrintItems(list(
        "Source file" = self$sourceFile
      ))
      ospsuite.utils::ospPrintItems(self$simulationNames, title = "Simulations")
      ospsuite.utils::ospPrintItems(
        self$parameterIdentificationNames,
        title = "Parameter identifications"
      )
      ospsuite.utils::ospPrintItems(self$individualNames, title = "Individuals")
      ospsuite.utils::ospPrintItems(
        self$expressionProfilesNames,
        title = "Expression profiles"
      )
      ospsuite.utils::ospPrintItems(self$moduleNames, title = "Modules")
    }
  ),
  private = list(
    .sourceFile = NULL
  )
)
