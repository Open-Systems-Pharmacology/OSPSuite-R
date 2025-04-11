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
    #' @field simulationNames Names of the simulations that are present in the project
    simulationNames = function(value) {
      if (missing(value)) {
        model <- self$get("SimulationNames")
      } else {
        private$.throwPropertyIsReadonly("simulationNames")
      }
    },
    #' @field parameterIdentificationNames Names of the parameter identifications that are present in the project
    #'
    #' PRIO 2
    parameterIdentificationNames = function(value) {
      if (missing(value)) {
        model <- self$get("ParameterIdentificationNames")
      } else {
        private$.throwPropertyIsReadonly("parameterIdentificationNames")
      }
    },
    #' @field Individuals Names of the individuals that are present in the project
    Individuals = function(value) {
      if (missing(value)) {
        model <- self$get("Individuals")
      } else {
        private$.throwPropertyIsReadonly("Indivduals")
      }
    },
    #' @field ExpressionProfiles Names of the expression profiles that are present in the project
    ExpressionProfiles = function(value) {
      if (missing(value)) {
        model <- self$get("ExpressionProfiles")
      } else {
        private$.throwPropertyIsReadonly("ExpressionProfiles")
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
    #' @returns A named list of `DataSet` objects. `NULL` if the project does not contain
    #' any observed data or no specified data sets are found.
    getObservedData = function(dataSetNames = NULL){
    },

    #' @description
    #' Get modules present in the project.
    #'
    #' @returns A named list of `MoBiModule` objects. `NULL` if the project does not contain
    #' any module.
    getModules = function(){
    },

    #' @description
    #' Get a specified individual from the project
    #' @param name Name of the individual
    #' @param stopIfNotFound If `TRUE` (default), an error is thrown if the specified
    #' individual is not present in the project.
    #'
    #' @returns An object of the type `BuildingBlock`. `NULL` if the project does not contain
    #' such an individual and `stopIfNotFound = FALSE`.
    getIndividual = function(name, stopIfNotFound = TRUE){
    },
    #' @description
    #' Get specified expression profiles from the project.
    #' @param names List of names of the expression profiles to retrieve.
    #' @param stopIfNotFound If `TRUE` (default), an error is thrown if any of the specified
    #' expression profiles is not present in the project.
    #' @returns A named list of objects of the type `BuildingBlock`. `NULL` for each
    #' specified expression profile that is not present in the project  if `stopIfNotFound = FALSE`.
    getExpressionProfiles = function(names, stopInfNotFound = TRUE){
    }

    #' @description
    #' Create a simulation from simulation configuration.
    #'
    #' Simulation is NOT added to the project!
    #'
    #' @param modulesNames A list of the modules from which to create in simulation.
    #' All defined modules must be present in the project. The order of module names defines the order in which the modules will be combined to a simulation!
    #' @param individualName Optional, name of the individual.
    #' @param expressionProfilesNames Optional, list of expression profiles to apply to the simulation.
    #' @param selectedInitialConditions By default, the first Initial Conditions
    #' (IC) building block (BB) of each module will be selected. If a module has multiple
    #' IC BBs, it is possible to specify which IC BB to apply by providing a named list,
    #' where the name should be the name of the module and the value the name of the IC BB.
    #' By setting the value to `NULL`, no IC BB from the specified module will be applied.
    #' @param selectedParameterValues By default, the first Parameter Values
    #' (PV) building block (BB) of each module will be selected. If a module has multiple
    #' PV BBs, it is possible to specify which PV BB to apply by providing a named list,
    #' where the name should be the name of the module and the value the name of the PV BB.
    #' By setting the value to `NULL`, no PV BB from the specified module will be applied.
    #' @param simulationName Name of the simulation.
    #'
    #' @returns A `Simulation` object.
     createSimulation = function(simulationName, modulesNames, individualName = NULL, expressionProfilesNames = NULL, selectedInitialConditions = NULL, selectedParameterValues = NULL){
    },

    #' @description
    #'
    #' PRIO 2
    #'
    #' Save the project.
    #'
    #' @param filePath Path to the file, including file name, where the project should be saved to.
    #' If `NULL` (default), the project is saved to the same file it was loaded from.
    #' @returns Path of the file to which the project is saved.
    saveProject = function(filePath = NULL(){

      return(filePath)
    },
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      ospsuite.utils::ospPrintClass(self)
      ospsuite.utils::ospPrintItems(list(
        "Source file" = self$sourceFile
      ))
    }
  ),
  private = list(
    .sourceFile = NULL
  )
)
