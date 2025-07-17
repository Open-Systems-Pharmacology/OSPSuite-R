#' Available methods for calculation of partition coefficients.
#'
#' MUST WE DEFINE IT HERE, OR CAN WE GET IT FROM .NET SOMEHOW?
#'
#' @export
PartitionCoefficientMethods <- enum(c("PK-Sim Standard" = "Cellular partition coefficient method - PK-Sim Standard",
                                      "Rodgers and Rowland" = "Cellular partition coefficient method - Rodgers and Rowland",
                                      "Schmitt" = "Cellular partition coefficient method - Schmitt",
                                      "Poulin and Theil" = "Cellular partition coefficient method - Poulin and Theil",
                                      "Berezhkovskiy" = "Cellular partition coefficient method - Berezhkovskiy"
))

#' Available methods for calculation of cellularw permeabilities.
#'
#' MUST WE DEFINE IT HERE, OR CAN WE GET IT FROM .NET SOMEHOW?
#'
#' @export
CellularPermeabilityMethods <- enum(c("PK-Sim Standard" = "Cellular permeability - PK-Sim Standard",
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
  inherit = ObjectBase,
  active = list(
    #' @field indivdual A building block of type "Individual" used in the configuration.
    #' Can be `NULL` if no Individual should be applied.
    individual = function(value){
      if (missing(value)) {
        return(self$get("Individual"))
      } else {
        isOfType(value, "Individual", nullAllowed = TRUE)
        # Check for the correct BB type
        if (!is.null(value)){
          if (value$type != "Individual"){
            #2DO throw error
          }
        }
        self$set("Individual", value)
      }
    },

    #' @field expressionProfiles A list of building blocks of type "Expression Profile"
    #' used in the configuration. Only one profile per protein is allowed.
    expressionProfiles = function(value){
      if (missing(value)){
        return(self$get("ExpressionProfiles"))
      } else {
        # Check for the correct type, list of ExpressionProfiles
        #2DO Where to check that only one profile for each protein is passed?
        # In R, or .NET? Preferably .net?
      }
    },

    #' @field modules A named list of `Module` objects from which to create in simulation.
    #' The order of modules defines the order in which the modules will be combined to a simulation!
    #' When setting the modules, the selection of Initial Conditions and Parameter Values
    #' is reset to the first available ones in the modules.
    modules = function(value){
      if (missing(value)){
        return(self$get("Modules"))
      } else {
        # Set the new modules combination
      }
    },

    #' @field selectedInitialConditions A named list with names being the names of the modules,
    #' and the values the names of Initial Conditions Building Blocks
    selectedInitialCondistions = function(value){
      if (missing(value)){
        return(self$get("SelectedInitialConditions"))
      } else {
        # Set the selected Initial Conditions. If the modules for which the IC
        # selection is provided are not in the configuration, throw an error.

      }
    },

    #' @field selectedParameterValues A named list with names being the names of the modules,
    #' and the values the names of Parameter Values Building Blocks
    #' If the modules for which the PV
    #' selection is provided are not in the configuration, throw an error.
    selectedParameterValues = function(value){
      if (missing(value)){
        return(self$get("SelectedParameterValues"))
      } else {
        # Set the selected Parameter Values. If the modules for which the PV
        # selection is provided are not in the configuration, throw an error.
      }
    },

    #' @field partitionCoefficientMethods The method used for calculation of partition coefficients. A named list with names being the molecules used in all modules,
    #' and the values being one of the `PartitionCoefficientMethods` enum values.
    #' To set the partition coefficient method for a molecule, provide a named list.
    partitionCoefficientMethods = function(value){
      if (missing(value)){
        return(self$get("PartitionCoefficientMethod"))
      } else {
        # Check that the provided value is a named list with names being the molecules
        # and values being one of the PartitionCoefficientMethods enum values.
        # If not, throw an error.
      }
    },

    #' @field cellularPermeabilityMethods The method used for calculation of cellular permeabilities. A named list with names being the molecules used in all modules,
    #' and the values being one of the `CellularPermeabilityMethods` enum values.
    #' To set the cellular permeability method for a molecule, provide a named list.
    cellularPermeabilityMethods = function(value){
      if (missing(value)){
        return(self$get("CellularPermeabilityMethod"))
      } else {
        # Check that the provided value is a named list with names being the molecules
        # and values being one of the CellularPermeabilityMethods enum values.
        # If not, throw an error.
      }
    }
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #'
    #' 2DO Should not be directly used. Instead, use function `loadMoBiProject()`
    #' to load a project.
    #' @param netObject Reference to `NetObject` .NET Simulation configuration object
    #' @returns A new `SimulationConfiguration` object.
    initialize = function(netObject) {
      super$initialize(netObject)
    },

    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      ospsuite.utils::ospPrintClass(self)

    }
    ),
    private = list(
      .individual = NULL

    )
  )
