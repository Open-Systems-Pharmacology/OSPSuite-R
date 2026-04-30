#' @title MoBi Module
#' @docType class
#' @description  A MoBi module, either loaded from a project or from a pkml file
#' @format NULL
MoBiModule <- R6::R6Class(
  "MoBiModule",
  cloneable = FALSE,
  inherit = ObjectBase,
  active = list(
    #' @field name Name of the module (read-only)
    name = function(value) {
      if (missing(value)) {
        self$get("Name")
      } else {
        private$.throwPropertyIsReadonly("name")
      }
    },
    #' @field isPKSimModule Whether the module is a PK-Sim module (read-only)
    isPKSimModule = function(value) {
      if (missing(value)) {
        self$get("IsPKSimModule")
      } else {
        private$.throwPropertyIsReadonly("isPKSimModule")
      }
    },
    #' @field mergeBehavior Merge behavior of the module. Must be one of "Extend" or "Overwrite".
    mergeBehavior = function(value) {
      if (missing(value)) {
        return(enumGetKey(enum = MergeBehavior, self$get("MergeBehavior")))
      } else {
        # Check that the provided merge behavior is either "Extend" or "Overwrite".
        if (!(value %in% enumKeys(MergeBehavior))) {
          stop(
            "Invalid value for enum 'MergeBehavior'. Must be one of: ",
            paste(ospsuite.utils::enumKeys(MergeBehavior), collapse = ", ")
          )
        }

        self$set(
          "MergeBehavior",
          as.integer(enumGetValue(enum = MergeBehavior, key = value))
        )
      }
    },
    #' @field parameterValuesBBnames Names of the Parameter Values Building Blocks (PV BBs) in the module (read-only)
    parameterValuesBBnames = function(value) {
      if (missing(value)) {
        return(.callModuleTask("AllParameterValuesBuildingBlockNames", self))
      } else {
        private$.throwPropertyIsReadonly("parameterValuesBBnames")
      }
    },
    #' @field initialConditionsBBnames Names of the Initial Conditions Building Blocks (IC BBs) in the module (read-only)
    initialConditionsBBnames = function(value) {
      if (missing(value)) {
        return(.callModuleTask("AllInitialConditionsBuildingBlockNames", self))
      } else {
        private$.throwPropertyIsReadonly("initialConditionsBBnames")
      }
    }
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #'
    #' @param netObject Underlying reference to the MoBi module in the simulation engine.
    #' @return A new `MoBiModule` object.
    initialize = function(netObject) {
      super$initialize(netObject)
    },

    #' @description
    #' Get the list of Parameter Values Building Blocks (PV BBs) in the module.
    #'
    #' @param names Optional names of the Parameter Values Building Block to retrieve.
    #' If `NULL`, returns all PV BBs.
    #' @param stopIfNotFound If `TRUE` (default), an error is thrown if any of the specified
    #' parameter values BB is not present in the project.
    #' @returns A named list of `BuildingBlock` objects, with names being the names of the PV BBs.
    getParameterValuesBBs = function(names = NULL, stopIfNotFound = TRUE) {
      .getICPVBBsFromModule(
        self,
        names = names,
        bbType = "Parameter Values",
        stopIfNotFound
      )
    },

    #' @description
    #' Get the list of Initial Conditions Building Blocks (IC BBs) in the module.
    #'
    #' @param names Optional names of the Initial Conditions Building Block to retrieve.
    #' If `NULL`, returns all IC BBs.
    #' @param stopIfNotFound If `TRUE` (default), an error is thrown if any of the specified
    #' initial conditions BB is not present in the project.
    #' @returns A named list of `BuildingBlock` objects, with names being the names of the IC BBs.
    getInitialConditionsBBs = function(names = NULL, stopIfNotFound = TRUE) {
      .getICPVBBsFromModule(
        self,
        names = names,
        bbType = "Initial Conditions",
        stopIfNotFound
      )
    },

    #' @description
    #' Get the `Molecules` Building Block of the module, if any.
    #' @returns A `MoleculesBuildingBlock` object exposing molecule-name
    #' queries (e.g. `allMoleculeNames()`, `allMoleculeNamesOfType()`,
    #' `moleculeTypeFor()`), or `NULL` if the module has no Molecules BB.
    getMoleculesBB = function() {
      .getBBFromModule(self, bbType = BuildingBlockTypes$Molecules)
    },

    #' @description
    #' Add one or more building blocks to the module.
    #'
    #' Single-type building blocks (Molecules, Reactions, Spatial Structure,
    #' Passive Transports, Observers, Event Groups) can appear at most once
    #' per module; trying to add a second BB of the same single type raises
    #' an error. Initial Conditions and Parameter Values BBs may appear
    #' multiple times.
    #'
    #' @param buildingBlocks A `BuildingBlock`, a list of `BuildingBlock`
    #'   objects, or `NULL` / empty list (no-op).
    #' @returns The module, invisibly.
    addBuildingBlocks = function(buildingBlocks) {
      validateIsOfType(buildingBlocks, "BuildingBlock", nullAllowed = TRUE)
      .callModuleTaskWithBBs(
        "AddBuildingBlocksToModule",
        self,
        buildingBlocks = buildingBlocks
      )
      invisible(self)
    },

    #' @description
    #' Remove a building block from the module by its name and type.
    #'
    #' Single-type building blocks (Molecules, Reactions, Spatial Structure,
    #' Passive Transports, Observers, Event Groups) are matched on type and
    #' the lone BB's `Name` is verified against `name`. Multi-type building
    #' blocks (Initial Conditions, Parameter Values) are looked up by name
    #' within their type. Expression Profile and Individual are not
    #' module-level and raise an error.
    #'
    #' @param name Name of the building block to remove (the BB's `Name`
    #'   property).
    #' @param type Type of the building block to remove. One of the values
    #'   defined in `BuildingBlockTypes` (excluding `Expression Profile` and
    #'   `Individual`).
    #' @returns The module, invisibly.
    removeBuildingBlock = function(name, type) {
      validateIsString(name)
      validateIsString(type)

      # Modules carry two flavours of building blocks that need different
      # lookup strategies:
      #   - single-type BBs (Molecules, Reactions, ...) appear at most once,
      #     are accessed via `module$get(<type>)`, and have to be name-checked
      #     against the lone BB's `Name`.
      #   - multi-type BBs (Initial Conditions, Parameter Values) can have
      #     many instances and must be looked up by name within the type via
      #     `.getICPVBBsFromModule`.
      # `.getBBFromModule` only handles the single-type case, hence the split.
      singleTypes <- c(
        BuildingBlockTypes$Molecules,
        BuildingBlockTypes$Reactions,
        BuildingBlockTypes$SpatialStructure,
        BuildingBlockTypes$`Passive Transports`,
        BuildingBlockTypes$Observers,
        BuildingBlockTypes$EventGroups
      )
      multiTypes <- c(
        BuildingBlockTypes$`Initial Conditions`,
        BuildingBlockTypes$`Parameter Values`
      )

      if (type %in% multiTypes) {
        bb <- .getICPVBBsFromModule(
          self,
          names = name,
          bbType = type,
          stopIfNotFound = TRUE
        )[[1]]
      } else if (type %in% singleTypes) {
        bb <- .getBBFromModule(self, bbType = type)
        if (is.null(bb)) {
          stop(sprintf(
            "No '%s' building block in module '%s'.",
            type,
            self$name
          ))
        }
        if (bb$name != name) {
          stop(sprintf(
            "Building block named '%s' (type '%s') not present in module '%s' (found '%s').",
            name,
            type,
            self$name,
            bb$name
          ))
        }
      } else {
        stop(sprintf(
          "Cannot remove building block of type '%s' from a module. Supported types: %s.",
          type,
          paste(c(singleTypes, multiTypes), collapse = ", ")
        ))
      }

      .callModuleTask("RemoveBuildingBlockFromModule", self, bb)
      invisible(self)
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
        "PK-Sim module" = self$isPKSimModule,
        "Merge behavior" = self$mergeBehavior
      ))
      ospsuite.utils::ospPrintItems(
        self$parameterValuesBBnames,
        title = "Parameter Values Building Blocks",
        print_empty = FALSE
      )
      ospsuite.utils::ospPrintItems(
        self$initialConditionsBBnames,
        title = "Initial Conditions Building Blocks",
        print_empty = FALSE
      )
    }
  ),
  private = list()
)
