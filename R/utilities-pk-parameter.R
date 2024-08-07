#' @title Adds and returns a User-Defined PK-Parameter to the managed list of PK-Parameters
#'
#' @param name Name of the user defined PK-Parameter
#' @param standardPKParameter Defined the standard PK-Parameter to use to perform the calculations
#' @param displayName Display Name to use when exporting the values (optional, default value is name)
#' @param displayUnit Unit in which the value will be exported
#'
#' @return The newly created `UserDefinedPKParameter`that was added to the list of PK-Parameters
#'
#' @examples
#'
#' # Adds a user defined parameter named MyAuc that will calculate the value of AUC
#' # between t=50 min and t=80min
#' myAUC <- addUserDefinedPKParameter(
#'   name = "MyAUC",
#'   standardPKParameter = StandardPKParameter$AUC_tEnd
#' )
#' myAUC$startTime <- 50
#' myAUC$endTime <- 80
#'
#' # Adds a user defined parameter named MyCMax that will calculate the value of Cmax
#' # between the 4th and 5th application
#' myCMax <- addUserDefinedPKParameter(
#'   name = "MyCMax",
#'   standardPKParameter = StandardPKParameter$C_max
#' )
#' myCMax$startApplicationIndex <- 4
#' myCMax$endApplicationIndex <- 5
#' @export
addUserDefinedPKParameter <- function(name, standardPKParameter, displayName = NULL, displayUnit = NULL) {
  validateIsString(name)
  validateEnumValue(standardPKParameter, StandardPKParameter)
  validateIsString(displayName, nullAllowed = TRUE)
  validateIsString(displayUnit, nullAllowed = TRUE)

  displayUnit <- .encodeUnit(displayUnit %||% "")
  displayName <- displayName %||% ""

  pkParameterTask <- .getNetTask("PKParameterTask")
  netUserDefinedPKParameter <- pkParameterTask$call("CreateUserDefinedPKParameter", name, as.integer(standardPKParameter), displayName, displayUnit)
  userDefinedPKParameter <- UserDefinedPKParameter$new(netUserDefinedPKParameter)
  pkParameterTask$call("AddUserDefinedPKParameter", netUserDefinedPKParameter)
  return(userDefinedPKParameter)
}

#' @title Removes all User-Defined PK-Parameters that may have been added to the system
#'
#' @export
removeAllUserDefinedPKParameters <- function() {
  pkParameterTask <- .getNetTask("PKParameterTask")
  pkParameterTask$call("RemoveAllUserDefinedPKParameters")
}

#' @title Updates some properties of a PK-Parameter (displayName and displayUnit)
#'
#' @param name Name of PK-Parameter to update
#' @param displayName Optional display name
#' @param displayUnit Optional display unit. Note that the unit should be defined in unit of the dimension
#'
#' @examples
#'
#' updatePKParameter("t_max", "MyTmax", "min")
#' @export
updatePKParameter <- function(name, displayName = NULL, displayUnit = NULL) {
  pkParameterTask <- .getNetTask("PKParameterTask")
  pkParameter <- pkParameterByName(name)

  .updatePKParameterProperties(pkParameter, displayName, displayUnit)
}

.updatePKParameterProperties <- function(pkParameter, displayName = NULL, displayUnit = NULL) {
  if (!is.null(displayName)) {
    pkParameter$displayName <- displayName
  }

  if (!is.null(displayUnit)) {
    pkParameter$displayUnit <- displayUnit
  }

  return(pkParameter)
}

#' @title Returns an instance of a PK-Parameter by name or NULL if the parameter by name is not found
#'
#' @param name Name of PK-Parameter to update
#' @param stopIfNotFound Boolean. If `TRUE` (default) and no pk parameter exist for the given name,
#' an error is thrown. If `FALSE`, `NULL` is returned.
#'
#' @examples
#'
#' pkParameter <- pkParameterByName(name = "t_max")
#' @export
pkParameterByName <- function(name, stopIfNotFound = TRUE) {
  pkParameterTask <- .getNetTask("PKParameterTask")
  pkParameter <- pkParameterTask$call("PKParameterByName", name)
  pkParameter <- .toObjectType(pkParameter, PKParameter)

  if (!is.null(pkParameter) || !stopIfNotFound) {
    return(pkParameter)
  }

  stop(messages$errorPKParameterNotFound(name, allPKParameterNames()))
}


#' @title Returns the name of all pk parameters defined in the system
#'
#' @examples
#'
#' pkParameterNames <- allPKParameterNames()
#' @export
allPKParameterNames <- function() {
  pkParameterTask <- .getNetTask("PKParameterTask")
  pkParameterTask$call("AllPKParameterNames")
}
