

#' All possible container types as defined in OSPSuite
#'
#' @include enum.R
#' @export
ContainerType <- enum(list(
  Other = 0,
  Simulation = 1,
  Model = 2,
  Organism = 3,
  Organ = 4,
  Compartment = 5,
  Application = 6,
  Event = 7,
  EventGroup = 8,
  Neighborhood = 9,
  Molecule = 10,
  Reaction = 11,
  Formulation = 12,
  Transport = 13
))
