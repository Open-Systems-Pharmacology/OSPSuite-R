#' @title MoBi Module # OR SHOULD WE CALL IT AN OspModule ?
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
        private$.throwPropertyIsReadonly("isPkSimModule")
      }
    },
    #' @field mergeBehavior Merge behavior of the module (read/write)
    mergeBehavior = function(value) {
      if (missing(value)) {
        return(enumGetKey(enum = MergeBehavior, self$get("MergeBehavior")))
      } else {
        private$.throwPropertyIsReadonly("mergeBehavior")
        # TODO: https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1591
        # # Check that the provided merge behavior is either "Extend" or "Overwrite".
        # if (!(value %in% enumKeys(MergeBehavior))) {
        #   stop("Invalid value for enum 'MergeBehavior'. Must be one of: ", paste(ospsuite.utils::enumKeys(MergeBehavior), collapse = ", "))
        # }
        #
        # self$set("MergeBehavior", enumGetValue(enum = MergeBehavior, key = value))
      }
    },
    #' @field parameterValuesBBnames Names of the Parameter Values Building Blocks (PV BBs) in the module (read-only)
    parameterValuesBBnames = function(value) {
      if (missing(value)) {
        return(.callModuleTask("AllParameterValueBuildingBlockNames", self))
      } else {
        private$.throwPropertyIsReadonly("parameterValuesBBnames")
      }
    },
    #' @field initialConditionsBBnames Names of the Initial Conditions Building Blocks (IC BBs) in the module (read-only)
    initialConditionsBBnames = function(value) {
      if (missing(value)) {
        return(.callModuleTask("AllInitialConditionBuildingBlockNames", self))
      } else {
        private$.throwPropertyIsReadonly("initialConditionsBBnames")
      }
    }
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #'
    #' @param netObject Reference to `NetObject` .NET MoBi-module object
    #' @return A new `MoBiModule` object.
    initialize = function(netObject) {
      super$initialize(netObject)
      # TODO: Initialize names once https://github.com/Open-Systems-Pharmacology/MoBi/issues/2029
    },

    #' @description
    #' Get the list of Parameter Values Building Blocks (PV BBs) in the module.
    #'
    #' @param names Optional names of the Parameter Values Building Block to retrieve.
    #' If `NULL`, returns all PV BBs.
    #' @returns A named list of `BuildingBlock` objects, with names being the names of the PV BBs.
    getParameterValuesBBs = function(names = NULL) {
      private$.getBBsWithNames(names = names, bbType = "Parameter Values")
    },

    #' @description
    #' Get the list of Initial Conditions Building Blocks (IC BBs) in the module.
    #'
    #' @param names Optional names of the Initial Conditions Building Block to retrieve.
    #' If `NULL`, returns all IC BBs.
    #' @returns A named list of `BuildingBlock` objects, with names being the names of the IC BBs.
    getInitialConditionsBBs = function(names = NULL) {
      private$.getBBsWithNames(names = names, bbType = "Initial Conditions")
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
        "PK-Sim module" = self$sPKSimModule,
        "Merge behavior" = self$mergeBehavior
      ))
      # TODO: List names of IC and PV BBs
    }
  ),
  private = list(
    #' @description
    #' Get the list of Parameter Values (PV) or Initial Conditions (IC) Building Blocks (BBs) in the module.
    #'
    #' @param names Optional names of the Parameter Values Building Block to retrieve.
    #' If `NULL`, returns all PV BBs.
    #' @param bbType Type of Building Block to retrieve, either "Parameter Values" or "Initial Conditions".
    #' @returns A named list of `BuildingBlock` objects, with names being the names of the PV BBs.
    .getBBsWithNames = function(names = NULL, bbType) {
      # First get the list of all BBs
      if (bbType == "Parameter Values") {
        bbs <- .callModuleTask("AllParameterValuesFromModule", self)
      } else if (bbType == "Initial Conditions") {
        bbs <- .callModuleTask("AllInitialConditionsFromModule", self)
      } else {
        stop("Invalid Building Block type. Must be either 'Parameter Values' or 'Initial Conditions'.")
      }

      # Create a list of BuildingBlock objects
      # and filter by names if provided
      newNames <- list()
      bbs <- lapply(bbs, function(bb) {
        bb <- BuildingBlock$new(bb, type = bbType)
        if (is.null(names) || bb$name %in% names) {
          newNames <<- c(newNames, bb$name)
          return(bb)
        }
        return(NULL)
      })
      # remove NULL
      bbs <- Filter(Negate(is.null), bbs)

      names(bbs) <- newNames
      # check if any passed names are not present
      if (!is.null(names)) {
        missingNames <- setdiff(names, newNames)
        if (length(missingNames) > 0) {
          stop(paste("No", bbType, "Building Blocks found with names:", paste(names, collapse = ", ")))
        }
      }
      return(bbs)
    }
  )
)
