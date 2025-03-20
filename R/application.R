#' @title Application
#' @docType class
#' @description  Application representing one instance of an application in a Simulation
#'
#' @format NULL
Application <- R6::R6Class(
  "Application",
  cloneable = FALSE,
  inherit = DotNetWrapper,
  active = list(
    #' @field startTime Start time of application instance (Read-Only)
    startTime = function(value) {
      if (missing(value)) {
        .toObjectType(private$.wrapReadOnlyProperty("StartTime", value), Parameter)
      }
    },
    #' @field infusionTime Infusion time of application instance or null if undefined (Read-Only)
    infusionTime = function(value) {
      if (missing(value)) {
        .toObjectType(private$.wrapReadOnlyProperty("InfusionTime", value), Parameter)
      }
    },
    #' @field drugMass Applied drugmass of the application instance of null if undefined (Read-Only)
    drugMass = function(value) {
      if (missing(value)) {
        .toObjectType(private$.wrapReadOnlyProperty("DrugMass", value), Parameter)
      }
    }
  ),
  public = list(
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      ospsuite.utils::osp_print_class(self)
      # Get the container of the application to get its path. Using the parameter
      # of the application to get the parent container.
      container <- self$startTime$parentContainer
      while (container$containerType != "Application") {
        container <- container$parentContainer
      }
      infusionTime <- NULL
      if (!is.null(self$infusionTime)) {
        infusionTime <- self$infusionTime$getPrintValue()
      }
      ospsuite.utils::osp_print_items(list(
        "Path" = container$path,
        "Start time" = self$startTime$getPrintValue(),
        "Infusion time" = infusionTime,
        "Drug mass" = self$drugMass$getPrintValue()
      ),
      print_empty = FALSE)
    }
  )
)
