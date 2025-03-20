# DefaultPlotConfiguration

test_that("It stores new values when defaults are changed", {
  myPlotConfiguration <- DefaultPlotConfiguration$new()

  myNewTitle <- "My Plot Title"
  myNewPointSize <- 2.5
  myNewLegendTitle <- "My Legend Title"

  myPlotConfiguration$title <- myNewTitle
  myPlotConfiguration$pointsSize <- myNewPointSize
  myPlotConfiguration$legendTitle <- myNewLegendTitle

  expect_equal(myPlotConfiguration$title, myNewTitle)
  expect_equal(myPlotConfiguration$pointsSize, myNewPointSize)
  expect_equal(myPlotConfiguration$legendTitle, myNewLegendTitle)
})

test_that("Defaults of the plot configuration", {
  myPlotConfiguration <- DefaultPlotConfiguration$new()

expect_snapshot(myPlotConfiguration)
})
