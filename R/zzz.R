.onLoad <- function(...) {

  # Only for x64 bits packages. This is required to avoid error when package is being checked on CI for x86
  is64 <- (.Machine$sizeof.pointer == 8)
  if (!is64) {
    return()
  }
  #
  # Should only be called once
  initPackage()

  ContainerType <- containerType();

  # Ensures that the variable ContainerType is available in the package as ospsuite::ContainerType
  assign("ContainerType", ContainerType, envir = parent.env(environment()))
  # to do
  #  ggplot_global$theme_current <- theme_gray()
}
