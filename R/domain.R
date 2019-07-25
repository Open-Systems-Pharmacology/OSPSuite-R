#' @title DotNetRwapper
#' @docType class
#' @description  Wrapper class for .net Object
#' @field ref The actual .NET object instanced being wrapped
#'
#' @section Methods:
#' \describe{
#' \item{wrapProperties Simple way to wrap a get;set; .NET property
#' }
#' @importFrom R6 R6Class
#' @export
DotNetWrapper <- R6::R6Class(
  "DotNetWrapper",
  public = list(
    ref = NULL,
    initialize = function(ref) {
      self$ref <- ref
    },
    wrapProperties = function(name, value) {
      if (missing(value)) {
        rClr::clrGet(self$ref, name)
      } else {
        rClr::clrSet(self$ref, name, value)
      }
    },

    wrapReadOnlyProperties = function(name, value) {
      if (missing(value)) {
        rClr::clrGet(self$ref, name)
      } else {
        stop(paste0("Property ", "'$", name, "' is readonly"), call. = FALSE)
      }
    }
  ),
)

#' @title ApiConfig
#' @docType class
#' @description  Global configuration for the OSPSuite .NET API
#'
#' @export
ApiConfig <- R6::R6Class(
  "ApiConfig",
  inherit = DotNetWrapper,
  active = list(
    pkParametersFilePath = function(value) {
      self$wrapProperties("PKParametersFilePath", value)
    },
    dimensionFilePath = function(value) {
      self$wrapProperties("DimensionFilePath", value)
    }
  ),
)

#' @title ObjectBase
#' @docType class
#' @description  Abstract wrapper for an OSPSuite.Core ObjectBase.
#'
#' @field id The id of the .NET wrapped object. (read-only)
#' @field name The name of the object
#'
#' @export
ObjectBase <- R6Class("ObjectBase",
  inherit = DotNetWrapper,
  active = list(
    name = function(value) {
      self$wrapProperties("Name", value)
    },
    id = function(value) {
      self$wrapReadOnlyProperties("Id", value)
    }
  )
)

#' @title Entity
#' @docType class
#' @description  Abstract wrapper for an OSPSuite.Core Entity class
#'
#' @field path The path of the entity in the container hiearchy. (read-only)
#'
#' @export
Entity <- R6Class("Entity",
  inherit = ObjectBase,
  active = list(
    path = function(value) {
      if (missing(value)) {
        rClr::clrCallStatic(typename = "OSPSuite.Core.Domain.EntityExtensions", methodName = "EntityPath", self$ref)
      } else {
        stop("Property '$path' is readonly", call. = FALSE)
      }
    }
  )
)

#' @title Parameter
#' @docType class
#' @description  A model parameter with a value
#'
#' @field value The value of the parameter
#'
#' @export
Parameter <- R6Class(
  "Parameter",
  inherit = Entity,
  active = list(
    value = function(value) {
      self$wrapProperties("Value", value)
    }
  ),
  public = list(
    print = function(...) {
      print(paste0("Parameter '", self$path, "' has a value of ", self$value))
      invisible(self)
    }
  )
)

#' @title Container
#' @docType class
#' @description  Contains other entities such as Parameter or containers
#'
#' @field path The path of the entity in the container hiearchy. (read-only)
#'
#' @export
Container <- R6Class("Container",
  inherit = Entity,
)


#' @title Simulation
#' @docType class
#' @description  An OSPSuite simulation
#'
#' @field root The rot container of the simulation (read-only)
#'
#' @export
Simulation <- R6Class(
  "Simulation",
  inherit = Entity,
  active = list(
    root = function(value) {
      if (missing(value)) {
        model <- rClr::clrGet(self$ref, "Model")
        root <- rClr::clrGet(model, "Root")
        Container$new(root)
      } else {
        stop("Property '$root' is readonly", call. = FALSE)
      }
    }
  )
)
