# Run this script to update package internal data

devtools::load_all()

ospDimensions <- .getDimensionsEnum()
ospUnits <- .getUnitsEnum()

usethis::use_data(ospDimensions, ospUnits, internal = TRUE, overwrite = TRUE)
