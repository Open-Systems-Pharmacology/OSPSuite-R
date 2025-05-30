#' Convert `.NET` object to wrapper class in R
#'
#' @description
#'
#' Transforms a single or a list of `.NET` object(s) to their
#' corresponding wrapper class in R. Note that if the object is a single object,
#' `NULL` will be returned if the `.NET` object is `null`. This allows semantic
#' equivalence between `.NET` and R.
#'
#' @param netObject The `.NET` object instances (single or list) to wrap.
#' @param class The class definition that will be used to convert the parameter.
#'
#' @return The wrapped object (single or a list).
#' @keywords internal
.toObjectType <- function(netObject, class) {
  if (!is.list(netObject)) {
    return(ifNotNull(netObject, class$new(netObject = netObject)))
  }

  sapply(c(netObject), function(x) class$new(netObject = x))
}


#' Encoding mu symbol
#'
#' @details
#'
#' This is required to ensure that we have no issue using the mu symbol in different OS
#' See https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/476 for details.
#'
#' @param unit Unit to encode.
#'
#' @examples
#' ospsuite:::.encodeUnit("µl")
#'
#' @keywords internal
.encodeUnit <- function(unit) {
  unit <- enc2utf8(unit)
  mutants <- enc2utf8("\xc2\xb5|\xce\xbc|\xb5")
  if (stringr::str_detect(unit, pattern = mutants)) {
    unit <- stringr::str_replace_all(unit, pattern = mutants, replacement = ospsuiteEnv$muSymbol)
  }
  return(unit)
}

#' Retrieves the name of the constant in the specified enumeration that has the
#' specified value.
#'
#' @inheritParams rSharp::getEnumNames
#' @param enumValue The value of a particular enumerated constant in terms of
#'   its underlying type. Typically an integer.
#'
#' @return
#'
#' A string containing the name of the enumerated constant in `enumType` whose
#' value is `enumValue`; or `null` if no such constant is found.
#'
#' @examples
#' ospsuite:::.netEnumName("OSPSuite.Core.Domain.Data.AuxiliaryType", 2L)
#'
#' @keywords internal
.netEnumName <- function(enumType, enumValue) {
  netTypeObj <- rSharp::getType(enumType)
  rSharp::callStatic("System.Enum", methodName = "GetName", netTypeObj, enumValue)
}


#' Clears the memory used by all underlying objects
#'
#' @details
#'
#' The function aims at clearing the memory used by object references allocated
#' during some workflows. The memory should typically be freed automatically
#' when the system is under memory pressure or when the garbage collection is
#' kicking in. However, it may be necessary sometimes to explicitly start the
#' garbage collection process.
#'
#' @param clearSimulationsCache optional - Should the simulation cache also be
#'   cleared? Default is `FALSE`.
#'
#' @examples
#'
#' # This will clear the memory and also clear the simulations cache but leave
#' # the environment intact.
#' clearMemory(clearSimulationsCache = TRUE)
#'
#' @export
clearMemory <- function(clearSimulationsCache = FALSE) {
  if (clearSimulationsCache) {
    resetSimulationCache()
  }

  # performs R GC first
  gc()

  # then forces `.NET` garbage collection
  rSharp::callStatic("OSPSuite.R.Api", "ForceGC")
  invisible()
}

#' Get characters that are not allowed in simulation naming.
#'
#' The characters are extracted from OSPSuite.Core.Domain.Constants.ILLEGAL_CHARACTERS
#'
#' @returns A vector of characters.
#' @noRd
.getIllegalCharacters <- function() {
  netList <- rSharp::getStatic("OSPSuite.Core.Domain.Constants", "ILLEGAL_CHARACTERS")

  return(netList$call("ToArray"))
}

#' Get illegal simulation names
#'
#' Illegal names are the names of all direct child containers of the top container of the simulation,
#' plus the reserved word `MoleculeProperties`.
#'
#' @param sim A `Simulation` object.
#'
#' @returns A vector of illegal names.
#' @noRd
.getIllegalSimulationNames <- function(sim) {
  # Get all direct child containers of the top container of the simulation
  topContainer <- getAllContainersMatching("*", sim)
  # Extract the paths
  illegalNames <- sapply(topContainer, function(x) x$path)
  # Add the keyword `MoleculeProperties` to the list of illegal names
  illegalNames <- c(illegalNames, "MoleculeProperties")
}
