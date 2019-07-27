
#' @title Container
#' @docType class
#' @description  Contains other entities such as Parameter or containers
#' @section Methods:
#' \describe{
#'   \item{containerType}{Type of container}
#'   }
#'
Container <- R6Class("Container",
  inherit = Entity,
  active = list(
    containerType = function(value) {
      type <- self$wrapReadOnlyProperties("ContainerType", value)
      ContainerType[which(ContainerType == type)]
    }
  )
)

