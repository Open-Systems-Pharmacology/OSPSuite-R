# Shared vignette setup: gate runtime-dependent chunks on a working runtime.
#
# Evaluate the runtime chunks only when the OSPSuite .NET runtime initialised
# successfully (native libraries + .NET). On machines without a working runtime
# the code is shown but not executed, so the vignette still renders.
#
# This file is sourced from each vignette's setup chunk. It only defines
# `.ospRuntimeAvailable`; each vignette keeps its own `knitr::opts_chunk$set()`
# call so per-vignette options (figure size, output width, etc.) stay local.
.ospRuntimeAvailable <- isTRUE(tryCatch(
  requireNamespace("ospsuite", quietly = TRUE) &&
    ospsuite::getOSPSuiteSetting("initialized"),
  error = function(e) FALSE
))
