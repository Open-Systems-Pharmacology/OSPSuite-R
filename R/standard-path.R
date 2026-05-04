#' Standard parameter paths typically available in a PBPK simulation
#'
#'
#' @include enums.R utilities-path.R
#' @export
StandardPath <- enum(c(
  Age = toPathString(StandardContainer$Organism, "Age"),
  Height = toPathString(StandardContainer$Organism, "Height"),
  Weight = toPathString(StandardContainer$Organism, "Weight"),
  BMI = toPathString(StandardContainer$Organism, "BMI"),
  BSA = toPathString(StandardContainer$Organism, "BSA"),
  GestationalAge = toPathString(StandardContainer$Organism, "Gestational age"),
  OntogenyFactorAlbumin = toPathString(
    StandardContainer$Organism,
    "Ontogeny factor (albumin)"
  ),
  OntogenyFactorAlbuminAGP = toPathString(
    StandardContainer$Organism,
    "Ontogeny factor (alpha1-acid glycoprotein)"
  )
))
