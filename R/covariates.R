#' @title Covariates
#' @docType class
#' @description  All covariates for one individual
#'@section Methods:
#' \describe{
#'   \item{valueFor(covariateName)}{Returns the value of the covariate named \code{covariateName} or an empty string if not defined}
#'   }
#' @format NULL
Covariates <- R6::R6Class(
  "Covariates",
  inherit = DotNetWrapper,
  public = list(
    valueFor = function(covariateName) {
      validateIsString(covariateName);
      rClr::clrCall(self$ref, "Covariate", covariateName)
    }
  )
)
