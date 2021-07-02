library(ospsuite)

# Load the obs data from a pkml file
obsData <- loadDataRepositoryFromPKML("tests/data/obs_data.pkml")

# Create an empty data set
dataSet <- DataSet$new()

# get the values
xValues <- dataSet$xValues

xUnit <- dataSet$xUnit

