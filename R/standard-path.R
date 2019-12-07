
#' Standard containers typically available in a PBPK simulation
#'
#' @include enum.R
#' @export
StandardContainer <- enum(c(
  Organism = "Organism",
  Applications = "Applications",
  Neighborhoods = "Neighborhoods",
  Events = "Events"
))

#' Standard parameter paths typically available in a PBPK simulation
#'
#' @include enum.R
#' @include utilities-path.R
#' @export
StandardPath <- enum(c(
  Age = toPathString(StandardContainer$Organism, "Age"),
  Height = toPathString(StandardContainer$Organism, "Height"),
  Weight = toPathString(StandardContainer$Organism, "Weight"),
  BMI = toPathString(StandardContainer$Organism, "BMI"),
  BSA = toPathString(StandardContainer$Organism, "BSA"),
  GestationalAge = toPathString(StandardContainer$Organism, "Gestational age"),
  OntogenyFactorAlbumin = toPathString(StandardContainer$Organism, "Ontogeny factor (albumin)"),
  OntogenyFactorAlbuminAGP = toPathString(StandardContainer$Organism, "Ontogeny factor (alpha1-acid glycoprotein)")
))

#' Standard molecule parameter names typically available in an endogenous molecule (enzyme, transporter etc...) coming from PK-Sim
#'
#' @include enum.R
#' @export
MoleculeParameter <- enum(c(
  ReferenceConcentration = "Reference concentration",
  tHalfLiver = "t1/2 (liver)",
  tHalfIntestine = "t1/2 (intestine)",
  OntogenyFactor = "Ontogeny factor",
  OntogenyFactorGI = "Ontogeny factor GI"
))
