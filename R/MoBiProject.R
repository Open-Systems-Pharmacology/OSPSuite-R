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
        # Get ass simulation names using ProjectTask
        values <- .callProjectTaskAsArray(property = "AllSimulationNames", self)
        return(values)
      } else {
        private$.throwPropertyIsReadonly("simulationNames")
      }
    },

    #' @field moduleNames Names of the modules that are present in the project (read-only)
    moduleNames = function(value) {
      if (missing(value)) {
        values <- .callProjectTaskAsArray(property = "AllModuleNames", self)
        return(values)
      } else {
        private$.throwPropertyIsReadonly("moduleNames")
      }
    },
    #' @field parameterIdentificationNames Names of the parameter identifications
    #' that are present in the project (read-only)
    #' 2DO
    parameterIdentificationNames = function(value) {
      if (missing(value)) {
        values <- .callProjectTaskAsArray(property = "AllParameterIdentificationNames", self)
        return(values)
      } else {
        private$.throwPropertyIsReadonly("parameterIdentificationNames")
      }
    },
    #' @field individualNames Names of the individuals that are present in the project (read-only)
    individualNames = function(value) {
      if (missing(value)) {
        values <- .callProjectTaskAsArray(property = "AllIndividualNames", self)
        return(values)
      } else {
        private$.throwPropertyIsReadonly("individualNames")
      }
    },
    #' @field expressionProfilesNames Names of the expression profiles that are
    #' present in the project (read-only)
    expressionProfilesNames = function(value) {
      if (missing(value)) {
        values <- .callProjectTaskAsArray(property = "AllExpressionProfileNames", self)
        return(values)
      } else {
        private$.throwPropertyIsReadonly("expressionProfilesNames")
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
    #' @returns A `Simulation` object, if the simulation with the given name is present in the
    #' project. `NULL` if no such simulation is available.
    getSimulation = function(simulationName) {
    },

    #' @description
    #' Get observed data present in the project.
    #'
    #' @param dataSetNames Optional. List of names of observed data sets to retrieve
    #' from project. If `NULL`, all data sets are returned. If a specified data set
    #' is not found, the name is ignored.
    #'
    #' @returns A named list of `DataSet` objects.
    getObservedData = function(dataSetNames = NULL) {
      #TODO not implemented https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1582
    },

    #' @description
    #' Get modules present in the project.
    #'
    #' @param names Optional. Names of the modules to retrieve. If `NULL`, all modules are returned.
    #' @returns A named list of `MoBiModule` objects.
    getModules = function(names = NULL) {
      if (is.null(names)) {
        names <- .callProjectTaskAsArray(property = "AllModuleNames", self)
      }
      modules <- lapply(names, function(name) {
        module <- .callProjectTask(property = "ModuleByName", self, name)
        if (is.null(module)) {
          stop(messages$modulesNotPresentInProject(name))
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
      individual <- .callProjectTask(property = "IndividualBuildingBlockByName", self, name)

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
      expressionProfiles <- .callProjectTaskAsArray(property = "ExpressionProfileBuildingBlocksByName", self, names)

      realNames <- vector("character", length(expressionProfiles))
      profiles <- lapply(seq_along(expressionProfiles), function(idx) {
        profile <- expressionProfiles[[idx]]
        bb <- BuildingBlock$new(profile, type = BuildingBlockTypes$`Expression Profile`)
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
    #' @param modulesNames A list of the modules from which to create in simulation.
    #' All defined modules must be present in the project. The order of module names defines the order in which the modules will be combined to a simulation!
    #' @param individualName Optional, name of the individual.
    #' @param expressionProfilesNames Optional, list of expression profiles to apply to the simulation.
    #' @param initialConditions By default, the first Initial Conditions
    #' (IC) building block (BB) of each module will be selected. If a module has multiple
    #' IC BBs, it is possible to specify which IC BB to apply by providing a named list,
    #' where the name should be the name of the module and the value the name of the IC BB.
    #' If `NULL`, all modules will use the first IC BB, if available. When providing a list,
    #' the value can also be set to `NULL`, which means that no IC BB from the specified module
    #' will be selected.
    #' @param parameterValues By default, the first Parameter Values
    #' (PV) building block (BB) of each module will be selected. If a module has multiple
    #' PV BBs, it is possible to specify which PV BB to apply by providing a named list,
    #' where the name should be the name of the module and the value the name of the PV BB.
    #' If `NULL`, all modules will use the first PV BB, if available. When providing a list,
    #' the value can also be set to `NULL`, which means that no PV BB from the specified module
    #' will be selected.
    #'
    #' @returns A `SimulationConfiguration` object.
    createSimulationConfiguration = function(modulesNames, individualName = NULL, expressionProfilesNames = NULL, initialConditions = NULL, parameterValues = NULL) {
      # Task used for getting information (IC and PV BBs) from modules
      modulesTask <- .getMoBiTaskFromCache("ModuleTask")

      modules <- self$getModules()
      # Throw an error when a specified module is not present in the project
      missingModules <- setdiff(modulesNames, names(modules))
      if (length(missingModules) > 0) {
        stop(messages$modulesNotPresentInProject(missingModules))
      }

      # Get the specified individual
      individual <- NULL
      if (!is.null(individualName)) {
        individual <- self$getIndividual(individualName, stopIfNotFound = TRUE)
      }
      # Get the specified expression profiles
      expressionProfiles <- list()
      if (!is.null(expressionProfilesNames)) {
        expressionProfiles <- self$getExpressionProfiles(expressionProfilesNames, stopIfNotFound = TRUE)
      }

      # Create module configurations for each module
      moduleConfigurations <- lapply(modulesNames, function(moduleName) {
        module <- modules[[moduleName]]

        icBB <- NULL
        # If no IC BB is specified, use the first IC BB available in the module
        if (is.null(initialConditions)) {
          # First get the list of all IC BBs
          icBBs <- modulesTask$call("AllInitialConditionsFromModule", module)
          icBBs <- icBBs$call("ToArray")
          # Select the first IC BB if available
          if (length(icBBs) > 0) {
            icBB <- icBBs[[1]]
          }
        } else {
          # If initial conditions selection have been provided, use them.
          # If the name of the module is in the names of 'initialConditions',
          # get the value. The value could be NULL, of no IC BB should be selected.
          if (moduleName %in% names(initialConditions)) {
            selectedICName <- initialConditions[[moduleName]]
            icBB <- modulesTask$call("InitialConditionBuildingBlockByName", module, selectedICName)
            # However, if the provided value for the name is NULL, use NULL to specify no IC BB
            # Cannot create configuration if the speciefied IC BB is not available
            if (is.null(icBB) && !is.null(selectedICName)) {
              stop(messages$icBBNotPresentInModule(moduleName, selectedICName))
            }
          }
        }

        pvBB <- NULL
        # If no PV BB is specified, use the first PV BB available in the module
        if (is.null(parameterValues)) {
          # First get the list of all PV BBs
          pvBBs <- modulesTask$call("AllParameterValuesFromModule", module)
          pvBBs <- pvBBs$call("ToArray")
          # Select the first IC BB if available
          if (length(pvBBs) > 0) {
            pvBB <- pvBBs[[1]]
          }
        } else {
          # If initial conditions selection have been provided, use them.
          # If the name of the module is in the names of 'parameterValues',
          # get the value. The value could be NULL, of no PV BB should be selected.
          if (moduleName %in% names(parameterValues)) {
            selectedPVName <- parameterValues[[moduleName]]
            pvBB <- modulesTask$call("ParameterValueBuildingBlockByName", module, selectedPVName)
            # Cannot create configuration if the speciefied PV BB is not available
            # However, if the provided value for the name is NULL, use NULL to specify no PV BB
            if (is.null(pvBB) && !is.null(selectedPVName)) {
              stop(messages$pvBBNotPresentInModule(moduleName, selectedPVName))
            }
          }
        }
        # Create module configuration with default IC and PV BBs
        moduleConfiguration <- .createModuleConfiguration(module, selectedParameterValue = pvBB, selectedInitialCondition = icBB)
        return(moduleConfiguration)
      })
      netTask <- .getMoBiTaskFromCache("SimulationTask")
      # TODO: remove simulation name after https://github.com/Open-Systems-Pharmacology/MoBi/issues/2018
      # TODO: does not work until https://github.com/Open-Systems-Pharmacology/MoBi/issues/2024 is fixed
      netConfiguration <- netTask$call(
        "CreateConfiguration",
        "dummyName",
        moduleConfigurations,
        expressionProfiles,
        individual
      )
      configuration <- SimulationConfiguration$new(netConfiguration)

      return(configuration)
    },

    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      ospsuite.utils::ospPrintClass(self)
      ospsuite.utils::ospPrintItems(list(
        "Source file" = self$sourceFile
      ))
      ospsuite.utils::ospPrintItems(self$simulationNames, title = "Simulations")
      # ospsuite.utils::ospPrintItems(
      #   self$parameterIdentificationNames, title = "Parameter identifications"
      # )
      ospsuite.utils::ospPrintItems(self$individualNames, title = "Individuals")
      ospsuite.utils::ospPrintItems(self$expressionProfilesNames, title = "Expression profiles")
      ospsuite.utils::ospPrintItems(self$moduleNames, title = "Modules")
    }
  ),
  private = list(
    .sourceFile = NULL
  )
)
