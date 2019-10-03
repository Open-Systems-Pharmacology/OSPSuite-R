FormulaExtensions <- "OSPSuite.Core.Domain.Formulas.FormulaExtensions"

#' @title Formula
#' @docType class
#' @description  A formula of the model (Typically related to a \code{Quantity} such as a parameter)
#' @field isTable Is this a table formula (Read-Only)
#' @field isConstant Is this a constant formula (Read-Only)
#' @format NULL
Formula <- R6::R6Class(
  "Formula",
  inherit = ObjectBase,
  active = list(
    isTable = function(value) {
      private$wrapExtensionMethod(FormulaExtensions, "IsTable")
    },
    isConstant = function(value) {
      private$wrapExtensionMethod(FormulaExtensions, "IsConstant")
    },
    isExplicit = function(value) {
      private$wrapExtensionMethod(FormulaExtensions, "IsExplicit")
    },
    isDistributed = function(value) {
      private$wrapExtensionMethod(FormulaExtensions, "IsDistributed")
    },
    formulaString = function(value) {
      if (missing(value)) {
        if(self$isExplicit){
          return( rClr::clrGet(self$ref, "FormulaString"))
        }
        return(NULL)
      } else {
        private$throwPropertyIsReadonly("formulaString")
      }
    }
  ),
  public = list(
    print = function(...) {
      private$printClass()
      self$printFormula()
    },
    printFormula = function() {
      if(self$isConstant){
        private$printLine("isConstant", TRUE)
      }
      else if(self$isExplicit){
        private$printLine("isFormula", TRUE)
        private$printLine("formula", self$formulaString)
      }
      else if(self$isTable){
        private$printLine("isTable", TRUE)
      }
      else if(self$isDistributed){
        private$printLine("isDistributed", TRUE)
      }
      invisible(self)
    }
  )
)
