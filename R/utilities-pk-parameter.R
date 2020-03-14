#' @title Adds a User-Defined PK-Parameter to the managed list of PK-Parameters
#'
#' @param name Name of the user defined PK-Parameter
#' @param standardPKParameter Defined the standard PK-Parameter to use to perform the calculations
#' @param displayUnit Display Name to use when exporting the values (optional, default value is name)
#' @param displayName Unit in which the value will be exported
#'
#' @examples
#'
#' # Adds a user defined parameter named MyAuc that will calculat the value of AUC between t=50 min and t=80min
#' userDefinedPKParameter <- addUserDefinedPKParameter$new(name = "MyAUC", standardPKParameter = StandardPKParameter$AucTend)
#' userDefinedPKParameter$startTime <- 50
#' userDefinedPKParameter$endTime <- 80
#'
#' # Adds a user defined parameter named MyCMax that will calculat the value of Cmax between the 4th and 5th application
#' userDefinedPKParameter <- addUserDefinedPKParameter(name = "MyCMax", standardPKParameter = StandardPKParameter$Cmax)
#' userDefinedPKParameter$startApplicationIndex <- 4
#' userDefinedPKParameter$endApplicationIndex <- 5
#' @export
addUserDefinedPKParameter <- function(name, standardPKParameter, displayName = NULL, displayUnit = NULL) {
  validateIsString(name)
  validateEnumValue(standardPKParameter, StandardPKParameter)
  validateIsString(displayName, nullAllowed = TRUE)
  validateIsString(displayUnit, nullAllowed = TRUE)
  userDefinedPKParameter <- UserDefinedPKParameter$new(name = name, standardPKParameter = standardPKParameter)

  dimension <- NULL
  if (!is.null(displayUnit)) {
    dimension <- getDimensionForUnit(displayUnit)
    if (is.null(dimension)) {
      # we have a display unit and a dimension that is not found, this will be a user defiend dimension
      dimension <- createUserDefinedDimension(displayUnit)
    }
  }
  else {
    dimension <- getDimensionForStandardPKParameter(standardPKParameter)
  }

  rClr::clrSet(userDefinedPKParameter$ref, "Dimension", dimension)
  pkParameterTask <- getNetTask("PKParameterTask")
  rClr::clrCall(pkParameterTask, "AddUserDefinedPKParameter", userDefinedPKParameter$ref)
  .updatePKParameterProperties(userDefinedPKParameter, displayName, displayUnit)
}

#' @title Removes all User-Defined PK-Parameters that may have been added to the system
#'
#' @export
removeAllUserDefinedPKParameters <- function() {
  pkParameterTask <- getNetTask("PKParameterTask")
  rClr::clrCall(pkParameterTask, "RemoveAllUserDefinedPKParameters")
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
  pkParameterTask <- getNetTask("PKParameterTask")
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
#' @param stopIfNotFound Indicates whether an exception is thrown when the PK-Parameter was not found. Default is \code{TRUE}
#'
#' @examples
#'
#' pkParameter <- pkParameterByName(name = "t_max")
#' @export
pkParameterByName <- function(name, stopIfNotFound = TRUE) {
  pkParameterTask <- getNetTask("PKParameterTask")
  pkParameter <- rClr::clrCall(pkParameterTask, "PKParameterByName", name)
  pkParameter <- toObjectType(pkParameter, PKParameter)

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
allPKParameterNames <- function(){
  pkParameterTask <- getNetTask("PKParameterTask")
  rClr::clrCall(pkParameterTask, "AllPKParameterNames")
}
