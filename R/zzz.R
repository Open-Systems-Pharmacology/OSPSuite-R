# nocov start
.onLoad <- function(libname, pkgname) {
  # Only for x64 bits packages.
  # This is required to avoid error when package is being checked on CI for x86
  is64 <- (.Machine$sizeof.pointer == 8)
  if (!is64) {
    return()
  }

  # Check that the package is support by the user architecture
  supportedArchitecture <- R.version$arch %in% c("x86_64", "aarch64")
  if (!supportedArchitecture) {
    stop("The processor architecture you are using is not supported by this
         package. (only x86_64 and aarch64 are supported)")
  }

  .initPackage()
}
# nocov end
