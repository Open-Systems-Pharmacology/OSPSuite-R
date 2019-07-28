.onLoad <- function(...) {

  # Only for x64 bits packages. This is required to avoid error when package is being checked on CI for x86
  is64 <- (.Machine$sizeof.pointer == 8)
  if (!is64) {
    return()
  }
  #
  # Should only be called once
  initPackage()


  ContainerType <- enum(c(
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

  assign("ContainerType", ContainerType, envir = parent.env(environment()))
  # to do
  #  ggplot_global$theme_current <- theme_gray()
}
