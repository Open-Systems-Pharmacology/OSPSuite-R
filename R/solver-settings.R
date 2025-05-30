#' @title SolverSettings
#' @docType class
#' @description  Solver settings associated with a given simulation
#'
SolverSettings <- R6::R6Class(
  "SolverSettings",
  cloneable = FALSE,
  inherit = DotNetWrapper,
  active = list(
    #' @field useJacobian Use of Jacobian matrix during calculations
    useJacobian = function(value) {
      private$.wrapProperty("UseJacobian", value)
    },
    #' @field h0 Initial time step size
    h0 = function(value) {
      private$.wrapProperty("H0", value)
    },
    #' @field hMin Minimum absolute value of step size allowed
    hMin = function(value) {
      private$.wrapProperty("HMin", value)
    },
    #' @field hMax Maximum absolute value of step size allowed
    hMax = function(value) {
      private$.wrapProperty("HMax", value)
    },
    #' @field mxStep Maximum number of internal steps to be taken by the solver in its attempt to reach tout
    mxStep = function(value) {
      private$.wrapProperty("MxStep", value, asInteger = TRUE)
    },
    #' @field relTol Relative tolerance of unknowns
    relTol = function(value) {
      private$.wrapProperty("RelTol", value)
    },
    #' @field absTol Absolute tolerance of unknowns
    absTol = function(value) {
      private$.wrapProperty("AbsTol", value)
    }
  ),
  public = list(
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      ospsuite.utils::ospPrintClass(self)
      ospsuite.utils::ospPrintItems(list(
        "useJacobian" = self$useJacobian,
        "h0" = self$h0,
        "hMin" = self$hMin,
        "hMax" = self$hMax,
        "mxStep" = self$mxStep,
        "relTol" = self$relTol,
        "absTol" = self$absTol
      ))
    }
  )
)
