#' @title Population
#' @docType class
#' @description  List of individuals used in a population simulation
#' @format NULL
Population <- R6::R6Class(
  "Population",
  inherit = DotNetWrapper,
  active = list(
    #' @field count the number of individual in the population
    count = function(value) {
      private$wrapReadOnlyProperty("Count", value)
    },
    #' @field allCovariateNames the names of all covariates defined in the population
    allCovariateNames = function(value) {
      private$readOnlyProperty("allCovariateNames", value, rClr::clrCall(self$ref, "AllCovariatesNames"))
    }
  ),
  public = list(
    #' @description
    #' Returns \code{TRUE} if the population has variability defined for \code{parameterOrPath} otherwise \code{FALSE}
    #' @param parameterOrPath Parameter instance of parameter path
    has = function(parameterOrPath) {
      parameterPath <- private$getPathFrom(parameterOrPath)
      rClr::clrCall(self$ref, "Has", parameterPath)
    },
    #' @description
    #' Sets the variability values in the population for \code{parameterOrPath}
    #' @param parameterOrPath Parameter instance of parameter path
    #' @param values double vector containing the value to set for the `parameterOrPath`
    setValues = function(parameterOrPath, values) {
      parameterPath <- private$getPathFrom(parameterOrPath)
      validateIsNumeric(values)
      rClr::clrCall(self$ref, "SetValues", parameterPath, values)
      invisible(self)
    },
    #' @description
    #' Returns the variability values defined in the population for \code{parameterOrPath}
    #' @param parameterOrPath Parameter instance of parameter path
    getValues = function(parameterOrPath) {
      parameterPath <- private$getPathFrom(parameterOrPath)
      rClr::clrCall(self$ref, "GetValues", parameterPath)
    },
    #' @description
    #' Returns the values defined in the population for the covariate named `covariateName`
    #' @param covariateName Name of covariate for which values should be retrieved
    getCovariateValues = function(covariateName) {
      rClr::clrCall(self$ref, "GetCovariateValues", covariateName)
    },
    #' @description
    #' Returns the values defined in the population for the covariate named `covariateName and invididual with id `individualId`
    #' @param covariateName Name of covariate for which values should be retrieved
    #' @param individualId Id of individual for which the value for covariate `covariateName` should be retrieved
    getCovariateValue = function(covariateName, individualId) {
      rClr::clrCall(self$ref, "CovariateValueFor", covariateName, as.integer(individualId))
    },
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      private$printLine("Number of Individuals", self$count)
      invisible(self)
    }
  ),
  private = list(
    getPathFrom = function(parameterOrPath) {
      validateIsOfType(parameterOrPath, c("character", Parameter))
      if (isOfType(parameterOrPath, Parameter)) {
        return(parameterOrPath$path)
      }
      parameterOrPath
    }
  )
)
