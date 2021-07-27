library(ospsuite)

# Load the obs data from a pkml file
obsData <- loadDataRepositoryFromPKML("inst/extdata/obs_data.pkml")

# get the meta data properties as a list
metaData <- obsData$metaData

columns <- obsData$columns


