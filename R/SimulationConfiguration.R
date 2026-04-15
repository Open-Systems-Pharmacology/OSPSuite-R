#' Available methods for calculation of partition coefficients.
#'
#' MUST WE DEFINE IT HERE, OR CAN WE GET IT FROM .NET SOMEHOW?
#'
#' @export
PartitionCoefficientMethods <- enum(c(
  "PK-Sim Standard" = "Cellular partition coefficient method - PK-Sim Standard",
  "Rodgers and Rowland" = "Cellular partition coefficient method - Rodgers and Rowland",
  "Schmitt" = "Cellular partition coefficient method - Schmitt",
  "Poulin and Theil" = "Cellular partition coefficient method - Poulin and Theil",
  "Berezhkovskiy" = "Cellular partition coefficient method - Berezhkovskiy"
))

#' Available methods for calculation of cellular permeabilities.
#'
#' MUST WE DEFINE IT HERE, OR CAN WE GET IT FROM .NET SOMEHOW?
#'
#' @export
CellularPermeabilityMethods <- enum(c(
  "PK-Sim Standard" = "Cellular permeability - PK-Sim Standard",
  "Charge dependent Schmitt" = "Cellular permeability - Charge dependent Schmitt",
  "Charge dependent Schmitt normalized to PK-Sim" = "Cellular permeability - Charge dependent Schmitt normalized to PK-Sim"
))

#' @title Simulation configuration
#' @docType class
#' @description Configuration of a simulation. Contains description of the modules used for
#' the simulation, selected Parameter Values (PV) and Initial Conditions (IC), and molecule calculation methods.
#' @format NULL
SimulationConfiguration <- R6::R6Class(
  "SimulationConfiguration",
  cloneable = FALSE,
  active = list(
    #' @field individual A building block of type "Individual" used in the configuration.
    #' Can be `NULL` if no Individual should be applied.
    individual = function(value) {
      if (missing(value)) {
        return(private$.individual)
      } else {
        validateIsOfType(value, "BuildingBlock", nullAllowed = TRUE)
        if (!is.null(value)) {
          # Validate that only one individual is passed
          if (length(c(value)) > 1) {
            stop(messages$errorOnlyOneIndividualPerConfiguration())
          }
          # Check that the bb is of correct type
          .validateBuildingBlockType(value, "Individual")
        }
        private$.individual <- value
      }
    },

    #' @field expressionProfiles A list of building blocks of type "Expression Profile"
    #' used in the configuration. Only one profile per protein is allowed.
    expressionProfiles = function(value) {
      if (missing(value)) {
        return(private$.expressionProfiles)
      } else {
        validateIsOfType(
          value,
          "BuildingBlock",
          nullAllowed = TRUE
        )
        # Store protein names to check for duplicates
        proteinNames <- c()
        profilesNames <- c()
        for (bb in value) {
          # Check that each bb is of correct type
          .validateBuildingBlockType(bb, "Expression Profile")

          proteinName <- bb$get("MoleculeName")
          # Check that no expression profile for the same protein has already been defined
          if (proteinName %in% proteinNames) {
            stop(messages$errorExpressionProfileAlreadyDefined(
              bb$name,
              proteinName
            ))
          }
          proteinNames <- c(proteinNames, proteinName)
          profilesNames <- c(profilesNames, bb$name)
        }
        names(value) <- profilesNames
        private$.expressionProfiles <- value
      }
    },

    #' @field modules A named list of `Module` objects from which to create in simulation.
    #' The order of modules defines the order in which the modules will be combined to a simulation!
    #' When setting the modules, the selection of Initial Conditions and Parameter Values
    #' is reset to the first available ones in the modules.
    modules = function(value) {
      if (missing(value)) {
        return(private$.modules)
      } else {
        # Set the new modules combination
        ospsuite.utils::validateIsOfType(value, "MoBiModule")
        private$.modules <- value
      }
    },

    #' @field selectedInitialConditions A named list with names being the names of the modules,
    #' and the values the names of Initial Conditions Building Blocks
    selectedInitialConditions = function(value) {
      if (missing(value)) {
        # Constructing the return list by creating a new list with the names
        # of modules and filling it with the selected ICs
        # The reason for this is that setting selected IC for a module to NULL
        # removes the entry from the internal list, so we need to reconstruct it here
        returnList <- private$.selectedInitialConditions[names(
          private$.modules
        )]
        names(returnList) <- names(private$.modules)

        return(returnList)
      } else {
        validateIsNamedList(value, "selectedInitialConditions")
        # Set the selected Initial Conditions. If the modules for which the IC
        # selection is provided are not in the configuration, throw an error.

        # Check that all provided module names are in the configuration
        if (any(!(names(value) %in% names(private$.modules)))) {
          invalidModuleNames <- names(value)[
            !(names(value) %in% names(private$.modules))
          ]
          stop(messages$errorModuleNotInConfiguration(
            paste(invalidModuleNames, collapse = ", ")
          ))
        }

        # Store current selection to fall back if an error occurs
        currentSelection <- private$.selectedInitialConditions

        # Iterate through the provided named list
        for (moduleName in names(value)) {
          module <- private$.modules[[moduleName]]
          icBBnames <- module$initialConditionsBBnames
          selectedICName <- value[[moduleName]]
          # If the selected IC is not NULL, check that it exists in the module
          if (
            !is.null(selectedICName) &&
              !(selectedICName %in% icBBnames)
          ) {
            # Restore previous selection
            private$.selectedInitialConditions <- currentSelection

            stop(messages$errorICNotFoundInModule(
              selectedICName,
              module$name
            ))
          }
          private$.selectedInitialConditions[[moduleName]] <- selectedICName
        }
      }
    },

    #' @field selectedParameterValues A named list with names being the names of the modules,
    #' and the values the names of Parameter Values Building Blocks
    #' If the modules for which the PV
    #' selection is provided are not in the configuration, throw an error.
    selectedParameterValues = function(value) {
      if (missing(value)) {
        # Constructing the return list by creating a new list with the names
        # of modules and filling it with the selected PVs
        # The reason for this is that setting selected PV for a module to NULL
        # removes the entry from the internal list, so we need to reconstruct it here
        returnList <- private$.selectedParameterValues[names(
          private$.modules
        )]
        names(returnList) <- names(private$.modules)

        return(returnList)
      } else {
        # Set the selected Parameter Values. If the modules for which the PV
        # selection is provided are not in the configuration, throw an error.

        validateIsNamedList(value, "selectedParameterValues")
        # Check that all provided module names are in the configuration
        if (any(!(names(value) %in% names(private$.modules)))) {
          invalidModuleNames <- names(value)[
            !(names(value) %in% names(private$.modules))
          ]
          stop(messages$errorModuleNotInConfiguration(
            paste(invalidModuleNames, collapse = ", ")
          ))
        }
        # Store current selection to fall back if an error occurs
        currentSelection <- private$.selectedParameterValues
        # Iterate through the provided named list
        for (moduleName in names(value)) {
          module <- private$.modules[[moduleName]]
          pvBBnames <- module$parameterValuesBBnames
          selectedPVName <- value[[moduleName]]
          # If the selected PV is not NULL, check that it exists in the module
          if (
            !is.null(selectedPVName) &&
              !(selectedPVName %in% pvBBnames)
          ) {
            # Restore previous selection
            private$.selectedParameterValues <- currentSelection

            stop(messages$errorPVNotFoundInModule(
              selectedPVName,
              module$name
            ))
          }
          private$.selectedParameterValues[[moduleName]] <- selectedPVName
        }
      }
    },

    #' @field partitionCoefficientOverrides A named list of partition coefficient method
    #' overrides. Names are molecule names, values are the full calculation method names.
    #' Read-only; use `setPartitionCoefficientMethods()` to modify.
    partitionCoefficientOverrides = function(value) {
      if (missing(value)) {
        return(private$.partitionCoefficientOverrides)
      } else {
        stop(messages$errorPropertyReadOnly("partitionCoefficientOverrides"))
      }
    },

    #' @field cellularPermeabilityOverrides A named list of cellular permeability method
    #' overrides. Names are molecule names, values are the full calculation method names.
    #' Read-only; use `setCellularPermeabilityMethods()` to modify.
    cellularPermeabilityOverrides = function(value) {
      if (missing(value)) {
        return(private$.cellularPermeabilityOverrides)
      } else {
        stop(messages$errorPropertyReadOnly("cellularPermeabilityOverrides"))
      }
    },

    #' @field settings A `SimulationSettings` object defining the simulation settings.
    #' If no settings are provided, default settings will be used upon simulation creation.
    #' Individual properties within the `SimulationSettings` object are read-only, but the entire settings object can be replaced with another `SimulationSettings` instance (or set to `NULL` to use defaults).
    #' Setting the solver settings, output intervals, and output selections must be done after creating the simulation from the configuration.
    settings = function(value) {
      if (missing(value)) {
        return(private$.settings)
      } else {
        validateIsOfType(value, "SimulationSettings", nullAllowed = TRUE)
        private$.settings <- value
      }
    }
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #'
    #' Should not be directly used. Instead, use function `createSimulationConfiguration()` or the method `createSimulationConfiguration()`
    #' from the class `MoBiProject`.
    #' @param modules A list of `Module` objects from which to create in simulation.
    #' The order of modules defines the order in which the modules will be combined to a simulation!
    #' @param individual Optional, an individual building block
    #' @param expressionProfiles Optional, a list of expression profiles to apply to the simulation.
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
    #'
    #' @param settings Optional, a `SimulationSettings` object defining the simulation settings. If no settings are provided,
    #' default settings will be used upon simulation creation.
    #'
    #' @returns A new `SimulationConfiguration` object.
    initialize = function(
      modules,
      individual = NULL,
      expressionProfiles = NULL,
      selectedInitialConditions = NULL,
      selectedParameterValues = NULL,
      settings = NULL
    ) {
      # This class does not wrap the .NET domain object. Instead, it is used as a container
      # for the simulation configuration data before creating the actual simulation in .NET.

      self$modules <- modules
      self$individual <- individual
      self$expressionProfiles <- expressionProfiles

      # Get the list of IC and PV BBs from the modules and set the first one as selected
      private$.selectedInitialConditions <- vector("list", length(modules))
      private$.selectedParameterValues <- vector("list", length(modules))
      moduleNames <- vector("character", length(modules))
      for (i in seq_along(modules)) {
        module <- modules[[i]]
        moduleNames[[i]] <- module$name

        ### Initial Conditions
        # Get the names of all IC BBs in the module
        icBBnames <- module$initialConditionsBBnames
        selectedICName <- NULL
        # Apply selected IC if provided
        if (module$name %in% names(selectedInitialConditions)) {
          selectedICName <- selectedInitialConditions[[module$name]]
          # If the selected IC is not NULL, check that it exists in the module
          if (
            !is.null(selectedICName) &&
              !(selectedICName %in% icBBnames)
          ) {
            stop(messages$errorICNotFoundInModule(
              selectedICName,
              module$name
            ))
          }
        } else {
          # If no IC selection provided, select the first one
          if (length(icBBnames) > 0) {
            selectedICName <- icBBnames[[1]]
          }
        }

        ### Parameter Values
        # Get the names of all PV BBs in the module
        pvBBnames <- module$parameterValuesBBnames
        selectedPVName <- NULL
        # Apply selected PV if provided
        if (module$name %in% names(selectedParameterValues)) {
          selectedPVName <- selectedParameterValues[[module$name]]
          # If the selected PV is not NULL, check that it exists in the module
          if (
            !is.null(selectedPVName) &&
              !(selectedPVName %in% pvBBnames)
          ) {
            stop(messages$errorPVNotFoundInModule(
              selectedPVName,
              module$name
            ))
          }
        } else {
          # If no PV selection provided, select the first one
          if (length(pvBBnames) > 0) {
            selectedPVName <- pvBBnames[[1]]
          }
        }

        # Only set the selected IC and PV if not NULL. Otherwise, the value is already NULL and
        # explicitly setting it would delete the entry.
        if (!is.null(selectedICName)) {
          private$.selectedInitialConditions[[i]] <- selectedICName
        }
        if (!is.null(selectedPVName)) {
          private$.selectedParameterValues[[i]] <- selectedPVName
        }
      }
      names(private$.selectedInitialConditions) <- moduleNames
      names(private$.selectedParameterValues) <- moduleNames

      # Set simulation settings
      validateIsOfType(settings, "SimulationSettings", nullAllowed = TRUE)
      private$.settings <- settings
    },

    #' @description
    #' Sets the method used for calculation of partition coefficients. This method can be used to specify the partition coefficient method for each molecule used in the modules. If
    #' not specified, the method set in the Molecules Building Block of the last module in which the molecule is present will be used.
    #'
    #' @param moleculeName The name of the molecule for which to set the partition coefficient method.
    #' @param methodName The name of the method to use for calculation of partition coefficients for the specified molecule. Should be one of the `PartitionCoefficientMethods` enum values.
    #' Use `NULL` to remove an existing override.
    setPartitionCoefficientMethods = function(moleculeName, methodName) {
      private$.setCalculationMethodOverride(
        moleculeName,
        methodName,
        PartitionCoefficientMethods,
        ".partitionCoefficientOverrides"
      )
    },

    #' @description
    #' Sets the method used for calculation of cellular permeabilities. This method can be used to specify the cellular permeability method for each molecule used in the modules. If
    #' not specified, the method set in the Molecules Building Block of the last module in which the molecule is present will be used.
    #'
    #' @param moleculeName The name of the molecule for which to set the cellular permeability method.
    #' @param methodName The name of the method to use for calculation of cellular permeabilities for the specified molecule. Should be one of the `CellularPermeabilityMethods` enum values.
    #' Use `NULL` to remove an existing override.
    setCellularPermeabilityMethods = function(moleculeName, methodName) {
      private$.setCalculationMethodOverride(
        moleculeName,
        methodName,
        CellularPermeabilityMethods,
        ".cellularPermeabilityOverrides"
      )
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
      ospsuite.utils::ospPrintHeader("Modules")
      for (module in names(private$.modules)) {
        ospsuite.utils::ospPrintHeader(
          module,
          level = 2
        )
        ospsuite.utils::ospPrintItems(
          list(
            "Selected Initial Conditions" = private$.selectedInitialConditions[[
              module
            ]],
            "Selected Parameter Values" = private$.selectedParameterValues[[
              module
            ]]
          ),
          print_empty = TRUE
        )
      }

      ospsuite.utils::ospPrintItems(
        private$.individual$name,
        "Individual",
        print_empty = FALSE
      )
      ospsuite.utils::ospPrintItems(
        names(private$.expressionProfiles),
        title = "Expression profiles"
      )

      if (length(private$.partitionCoefficientOverrides) > 0) {
        displayNames <- vapply(
          private$.partitionCoefficientOverrides,
          function(val) enumGetKey(PartitionCoefficientMethods, val),
          character(1)
        )
        overrideList <- as.list(displayNames)
        names(overrideList) <- names(private$.partitionCoefficientOverrides)
        ospsuite.utils::ospPrintItems(
          overrideList,
          title = "Partition coefficient method overrides"
        )
      }

      if (length(private$.cellularPermeabilityOverrides) > 0) {
        displayNames <- vapply(
          private$.cellularPermeabilityOverrides,
          function(val) enumGetKey(CellularPermeabilityMethods, val),
          character(1)
        )
        overrideList <- as.list(displayNames)
        names(overrideList) <- names(private$.cellularPermeabilityOverrides)
        ospsuite.utils::ospPrintItems(
          overrideList,
          title = "Cellular permeability method overrides"
        )
      }
    }
  ),
  private = list(
    .modules = NULL,
    .selectedInitialConditions = NULL,
    .selectedParameterValues = NULL,
    .individual = NULL,
    .expressionProfiles = NULL,
    .settings = NULL,
    .partitionCoefficientOverrides = list(),
    .cellularPermeabilityOverrides = list(),

    .setCalculationMethodOverride = function(
      moleculeName,
      methodName,
      enumObj,
      fieldName
    ) {
      validateIsString(moleculeName)
      if (is.null(methodName)) {
        private[[fieldName]][[moleculeName]] <- NULL
        return(invisible(self))
      }
      validateIsString(methodName)
      validMethods <- enumValues(enumObj)
      if (!(methodName %in% validMethods)) {
        stop(messages$errorInvalidCalculationMethod(methodName, validMethods))
      }
      private[[fieldName]][[moleculeName]] <- methodName
      invisible(self)
    }
  )
)
