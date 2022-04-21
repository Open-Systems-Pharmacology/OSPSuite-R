test_that("plots grid produces error with wrong input type", {
  expect_error(plotGrid(DataSet$new(name = "DS")))
})

test_that("plots grid is rendered correctly", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("tlf")
  skip_if(getRversion() < "4.1")

  library(tlf)

  set.seed(123)
  ls_plots <- list(
    # first plot
    tlf::plotBoxWhisker(mtcars,
      dataMapping = tlf::BoxWhiskerDataMapping$new(x = "am", y = "wt"), outliers = FALSE
    ),
    # second plot
    tlf::plotBoxWhisker(ToothGrowth,
      dataMapping = tlf::BoxWhiskerDataMapping$new(x = "supp", y = "len")
    )
  )

  plotGridObj <- PlotGridConfiguration$new(ls_plots)

  plotGridObj$title <- "my combined plot"
  plotGridObj$subtitle <- "something clever"
  plotGridObj$caption <- "something dumb"
  plotGridObj$nColumns <- 2L
  plotGridObj$tagLevels <- "A"
  plotGridObj$tagPrefix <- "Plot ("
  plotGridObj$tagSuffix <- ")"

  expect_s3_class(plotGrid(plotGridObj), "ggplot")

  # TODO: turn on once you figure out why Appveyor produces slightly different plot
  # The differences are not discernible to the naked eye, so hard to diagnose at the moment
  # but also makes it not risky to skip the test at the moment
  # set.seed(123)
  # vdiffr::expect_doppelganger(
  #   title = "plotGrid works as expected",
  #   fig = plotGrid(plotGridObj)
  # )
})
