library(ospsuite)

# Load the obs data from a pkml file
obsData <- loadDataRepositoryFromPKML("tests/data/obs_data.pkml")

# Create an empty data set
dataSet <- DataSet$new(dataRepository = obsData)

# get the values
xValues <- dataSet$xValues

xUnit <- dataSet$xUnit


emptyDataSet <- DataSet$new(name = "NewDataSet")

yVal <- emptyDataSet$yValues

# this will be Time in min
emptyDataSet$xValues <- c(1, 2, 3, 4, 5)
emptyDataSet$xDimension <- ospDimensions$Amount

dataSet <- DataSet$new(name = "NewDataSet")
dataSet$addMetaData("Meta", "Value")
expect_equal(dataSet$metaData[["Meta"]], "Value")
